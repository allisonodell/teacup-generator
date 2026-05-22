################################################################################
# helper_functions.R
#
# Shared utilities sourced by both rezviz_data_generator.R (daily script) and
# setup_historical_baseline.R (full baseline rebuild).
#
# Currently provides the elevation-to-storage curve loader and lookup used to
# convert lake-elevation / gage-height observations from USGS (and similar
# sources) into reservoir storage volumes in acre-feet.
################################################################################

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

# Load elevation-storage curves for locations that report elevation
# (or gage height) instead of storage directly.
#
# The CSV format is:
#   location_id, elevation_ft, storage_af
# with comment lines beginning with `#`. Each location_id can have many rows;
# linear interpolation between adjacent rows is used at lookup time.
#
# Caller must define CONFIG_DIR before sourcing this file.
load_elevation_curves <- function(config_dir = CONFIG_DIR) {
  elev_curves_file <- file.path(config_dir, "elevation_storage_curves.csv")
  if (file.exists(elev_curves_file)) {
    curves <- read_csv(elev_curves_file, comment = "#", show_col_types = FALSE)
    ids <- unique(curves$location_id)
    message(sprintf("Loaded elevation-storage curves for %d location(s): %s",
                    length(ids), paste(ids, collapse = ", ")))
    list(curves = curves, ids = ids)
  } else {
    message("No elevation-storage curves file found")
    list(curves = NULL, ids = character(0))
  }
}

# Convert elevation (or gage height) to storage via linear interpolation.
#
# @param location_id The location identifier (matches `location_id` in the
#                    elevation_storage_curves.csv).
# @param elevation_ft Water-surface elevation in feet OR gage height in feet,
#                    depending on how the curve is indexed for that location.
#                    For Lake Tahoe (10337000) the curve is indexed by USGS
#                    gage height (ft above the natural rim of 6223.00 ft LTD).
#                    For Upper Klamath (11507001) the curve is indexed by
#                    lake-surface elevation in BOR datum feet.
# @return Storage in acre-feet, or NA if no curve is available for this site.
#
# Out-of-range inputs clamp to the min/max of the curve (so far below or far
# above the table both return the table's edge values).
#
# Requires that the caller has called `load_elevation_curves()` and assigned
# its result to a list named `elev_curves_data` in the calling environment,
# OR that `elev_curves` is already in scope as the data frame of curve rows.
elevation_to_storage <- function(location_id, elevation_ft) {
  curves <- if (exists("elev_curves_data") && !is.null(elev_curves_data$curves)) {
              elev_curves_data$curves
            } else if (exists("elev_curves")) {
              elev_curves
            } else {
              NULL
            }
  if (is.null(curves) || is.na(elevation_ft)) return(NA_real_)

  loc_id <- as.character(location_id)
  curve <- curves |> filter(location_id == loc_id)

  if (nrow(curve) == 0) {
    warning(sprintf("No elevation-storage curve for location %s", loc_id))
    return(NA_real_)
  }

  # Clamp out-of-range inputs to the curve endpoints.
  if (elevation_ft <= min(curve$elevation_ft)) return(min(curve$storage_af))
  if (elevation_ft >= max(curve$elevation_ft)) return(max(curve$storage_af))

  # Linear interpolation between the two bracketing rows.
  curve <- curve |> arrange(elevation_ft)
  idx_upper <- which(curve$elevation_ft >= elevation_ft)[1]
  idx_lower <- idx_upper - 1

  x1 <- curve$elevation_ft[idx_lower]
  x2 <- curve$elevation_ft[idx_upper]
  y1 <- curve$storage_af[idx_lower]
  y2 <- curve$storage_af[idx_upper]

  y1 + (elevation_ft - x1) * (y2 - y1) / (x2 - x1)
}
