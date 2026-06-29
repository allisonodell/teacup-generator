# rezviz_data_generator.R
#
# DAILY SCRIPT: Query current reservoir conditions, combine with historical
# statistics, generate output CSV for teacup visualization, and upload to
# HydroShare.
#
# Designed to run daily via cron/scheduler.
# Depends on historical_statistics.parquet created by setup_historical_baseline.R
#
# Fetches data from multiple sources:
#   - RISE via WWDH EDR API (majority of locations)
#   - USACE CDA API (Cochiti, Abiquiu, Santa Rosa, Grand Coulee, Fort Peck, Lucky Peak)
#   - USGS Water Data OGC API (Lahontan, Boca, Prosser Creek, Stampede, Upper Klamath)
#   - CDEC (Tahoe)
#
# HydroShare credentials loaded from .env file in the working directory.
# Expected .env format:
#   HYDROSHARE_USERNAME=user@example.com
#   HYDROSHARE_PASSWORD=yourpassword
#
# Author: Kyle Onda, CGS
# Created: 2026-01-28
################################################################################

library(httr2)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(stringr)
library(sf)
library(jsonlite)

# Shared utilities (elevation->storage curve loader + interpolator).
# Must source AFTER libraries but BEFORE elev_curves is first needed.
source("helper_functions.R")

# Load .env file if present
if (file.exists(".env")) {
  readRenviron(".env")
}

################################################################################
# CONFIGURATION
################################################################################

# API base URL for RISE data via WWDH
WWDH_API_BASE <- "https://api.wwdh.internetofwater.app"

# HydroShare
HYDROSHARE_RESOURCE_ID <- "22b2f10103e5426a837defc00927afbd"
HYDROSHARE_BASE_URL <- "https://www.hydroshare.org"

# Paths
OUTPUT_DIR       <- "output"
CONFIG_DIR       <- "config"
HYDROSHARE_DIR   <- "hydroshare"

# Target date (default: yesterday, can override via command line)
args <- commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  TARGET_DATE <- as.Date(args[1])
} else {
  TARGET_DATE <- Sys.Date() - 1
}

# How many days to look back when fetching from each source. Must comfortably
# exceed REPORT_DAYS so every report date still resolves to the most recent
# value on or before it even when a source has a multi-day gap.
LOOKBACK_DAYS <- 14

# Number of days to include in output CSV (target date + previous days).
# Set to 7 so each daily run publishes the last 7 days of data.
REPORT_DAYS <- 7

# Historical statistics period
STATS_PERIOD <- "10/1/1990 - 9/30/2020"

message(sprintf("=== Reservoir Data Generator ==="))
message(sprintf("Target date: %s", TARGET_DATE))
message(sprintf("Run time: %s", Sys.time()))

################################################################################
# PARQUET SYNC FROM HYDROSHARE
################################################################################

# Always download the latest parquet files from HydroShare at runtime.
# Bundled parquet files (from Docker build) serve as fallback if download fails.
# This ensures backfills from previous runs are picked up without rebuilding
# the Docker image - preventing the same locations from being re-backfilled
# on every run.

stats_file    <- file.path(OUTPUT_DIR, "historical_statistics.parquet")
baseline_file <- file.path(OUTPUT_DIR, "historical_baseline.parquet")

# Get HydroShare credentials
hs_username <- Sys.getenv("HYDROSHARE_USERNAME", "")
hs_password <- Sys.getenv("HYDROSHARE_PASSWORD", "")

download_parquet_from_hydroshare <- function(filename, dest_path, resource_id, username, password) {
  url <- sprintf("%s/hsapi/resource/%s/files/%s/", HYDROSHARE_BASE_URL, resource_id, filename)
  tmp <- paste0(dest_path, ".tmp")
  tryCatch({
    req <- request(url) |> req_timeout(300)
    if (username != "" && password != "") {
      req <- req |> req_auth_basic(username, password)
    }
    req |> req_perform() |> resp_body_raw() |> writeBin(tmp)
    # Validate parquet magic bytes before replacing bundled file
    magic <- readBin(tmp, "raw", n = 4)
    if (rawToChar(magic) == "PAR1") {
      file.rename(tmp, dest_path)
      message(sprintf("  Updated %s from HydroShare (%.1f MB)", filename, file.size(dest_path) / 1e6))
      return(TRUE)
    } else {
      file.remove(tmp)
      message(sprintf("  WARNING: Downloaded %s failed validation, keeping bundled file", filename))
      return(FALSE)
    }
  }, error = function(e) {
    if (file.exists(tmp)) file.remove(tmp)
    message(sprintf("  WARNING: Could not download %s: %s - using bundled file", filename, e$message))
    return(FALSE)
  })
}

message("\nSyncing parquet files from HydroShare...")
download_parquet_from_hydroshare("historical_baseline.parquet",   baseline_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
download_parquet_from_hydroshare("historical_statistics.parquet", stats_file,    HYDROSHARE_RESOURCE_ID, hs_username, hs_password)

if (!file.exists(stats_file) || !file.exists(baseline_file)) {
  stop("Parquet files not found and could not be downloaded from HydroShare.")
}

################################################################################
# LOAD HISTORICAL STATISTICS
################################################################################

if (!file.exists(stats_file)) {
  stop("Historical statistics file not found. Run setup_historical_baseline.R first.")
}

historical_stats <- read_parquet(stats_file)
message(sprintf("Loaded historical statistics: %d rows for %d locations",
                nrow(historical_stats),
                n_distinct(historical_stats$location_id)))

# Filter out locations whose historical baseline does not adequately cover the
# full 30-water-year period (Oct 1990 – Sep 2020).  We require at least 20
# water years of observations; locations with fewer have a systematically
# incomplete period of record and their percentiles/means are unreliable.
MIN_WATER_YEARS <- 20

baseline_file <- file.path(OUTPUT_DIR, "historical_baseline.parquet")
if (file.exists(baseline_file)) {
  baseline <- read_parquet(baseline_file)
  wy_coverage <- baseline |>
    mutate(water_year = ifelse(month(date) >= 10, year(date) + 1, year(date))) |>
    group_by(location_id) |>
    summarize(n_water_years = n_distinct(water_year), .groups = "drop")

  inadequate <- wy_coverage |>
    filter(n_water_years < MIN_WATER_YEARS) |>
    pull(location_id)

  if (length(inadequate) > 0) {
    message(sprintf("  Excluding %d locations with < %d water years of baseline data:",
                    length(inadequate), MIN_WATER_YEARS))
    for (loc_id in inadequate) {
      nwy <- wy_coverage$n_water_years[wy_coverage$location_id == loc_id]
      message(sprintf("    %s (%d water years)", loc_id, nwy))
    }
    historical_stats <- historical_stats |>
      filter(!location_id %in% inadequate)
    message(sprintf("  Retained historical statistics for %d locations",
                    n_distinct(historical_stats$location_id)))
  }
  rm(baseline, wy_coverage, inadequate)
} else {
  message("  WARNING: historical_baseline.parquet not found; cannot verify coverage.")
}

################################################################################
# LOAD LOCATION METADATA
################################################################################

locations_file <- file.path(CONFIG_DIR, "locations.geojson")
locations_sf <- st_read(locations_file, quiet = TRUE)

# Extract location metadata from geojson - ALL locations
locations <- locations_sf |>
  st_drop_geometry() |>
  transmute(
    name = Name,
    location_id = Identifier,
    capacity = as.numeric(str_remove_all(`Total.Capacity`, ",")),
    active_capacity = as.numeric(str_remove_all(`Active.Capacity`, ",")),
    label_map = `Preferred.Label.for.Map.and.Table`,
    label_popup = `Preferred.Label.for.PopUp.and.Modal`,
    state = state,
    doi_region = doiRegion,
    huc6 = huc6,
    longitude = Longitude,
    latitude = Latitude,
    source = `Source.for.Storage.Data`,
    data_type = `Storage.Data.Type`  # "Storage" or "Elevation"
  )

# RISE parameter ID (from the new column in locations.csv/geojson); fall back to
# NA if the geojson was generated before the column was added.
if ("RISE.Parameter.ID.for.Storage.Data" %in% names(locations_sf)) {
  locations$rise_param_id <- locations_sf$`RISE.Parameter.ID.for.Storage.Data`
} else {
  locations$rise_param_id <- NA_character_
}

# USGS parameter code override (e.g. "00065" for Lake Tahoe gage height).
# When set, fetch_usgs() uses this exact code instead of the default
# elevation-fallback chain (62614 / 72275 / 62615) or the default storage code
# (00054). Blank/NA -> use the existing defaults.
if ("USGS.Parameter.Code" %in% names(locations_sf)) {
  locations$usgs_param_code <- locations_sf$`USGS.Parameter.Code`
} else {
  locations$usgs_param_code <- NA_character_
}

message(sprintf("Loaded %d locations from geojson", nrow(locations)))

################################################################################
# ELEVATION-TO-STORAGE CONVERSION
################################################################################

# Load elevation-storage curves for locations that report elevation
# (or gage height) instead of storage. The loader + lookup function live in
# helper_functions.R; we expose `elev_curves` and `elev_curve_ids` here for
# backward-compat with the missing-curves check below and historical reads.
elev_curves_data <- load_elevation_curves(CONFIG_DIR)
elev_curves    <- elev_curves_data$curves
elev_curve_ids <- elev_curves_data$ids

# Check for locations that need elevation conversion but lack curves
elevation_locations <- locations |> filter(tolower(data_type) == "elevation")
if (nrow(elevation_locations) > 0) {
  missing_curves <- setdiff(elevation_locations$location_id, elev_curve_ids)
  if (length(missing_curves) > 0) {
    message(sprintf("WARNING: %d location(s) report elevation but lack conversion curves:",
                    length(missing_curves)))
    for (loc_id in missing_curves) {
      loc_name <- locations$name[locations$location_id == loc_id]
      message(sprintf("  - %s (ID: %s)", loc_name, loc_id))
    }
  }
}

################################################################################
# SOURCE TYPE CLASSIFICATION
################################################################################

# classify_source() now lives in helper_functions.R so the daily script and the
# standalone backfill routine share one implementation.

# Note: All data sources now use the geojson Identifier directly:
#   - RISE: Identifier is the RISE location ID (e.g., "7166")
#   - USACE: Identifier is "provider/ts_name" (e.g., "spa/Abiquiu.Stor.Inst.15Minutes.0.DCP-rev")
#   - USGS: Identifier is the USGS site number (e.g., "10344490")
#   - CDEC: Identifier is the CDEC station code (e.g., "THC")

################################################################################
# DETECT AND BACKFILL NEW LOCATIONS
################################################################################

# Check which locations are missing from historical baseline
# This catches both:
#   1. Newly added locations (not previously in locations.csv)
#   2. Status changes from "Do Not Include" -> "Include" (now in geojson but no baseline data)
#
# The geojson only contains "Include" locations, so any location_id in geojson
# but not in historical_baseline.parquet needs to be backfilled.

baseline_file <- file.path(OUTPUT_DIR, "historical_baseline.parquet")
baseline_location_ids <- if (file.exists(baseline_file)) {
  read_parquet(baseline_file) |> pull(location_id) |> unique()
} else {
  character(0)
}

new_location_ids <- setdiff(locations$location_id, baseline_location_ids)
# Exclude locations with no valid identifier (e.g., "--" for RISE Pending)
new_location_ids <- new_location_ids[!is.na(new_location_ids) & new_location_ids != "--" & new_location_ids != ""]
new_locations <- locations |> filter(location_id %in% new_location_ids)

if (nrow(new_locations) > 0) {

  message(sprintf("\n=== Detected %d new location(s) requiring backfill ===", nrow(new_locations)))
  message("(These may be newly added or changed from 'Do Not Include' to 'Include')\n")
  for (j in seq_len(nrow(new_locations))) {
    message(sprintf("  - %s (ID: %s)", new_locations$name[j], new_locations$location_id[j]))
  }
  message("")

  # Historical period for baseline statistics (used for percentiles). The
  # day-of-year statistics are ALWAYS computed over this 30-water-year window.
  BASELINE_START <- as.Date("1990-10-01")
  BASELINE_END   <- as.Date("2020-09-30")

  # Backfill fetch range: pull every available daily observation from as far
  # back as the sources provide (potentially the 19th century) through today,
  # so the new location lands in the daily droughtData CSV with its COMPLETE
  # record, not just the 1990-2020 window. Sources without old data simply
  # return nothing for the early years. The stats computation below still
  # filters to BASELINE_START..BASELINE_END regardless of how far back we fetch.
  FETCH_START  <- as.Date("1870-01-01")
  BACKFILL_END <- Sys.Date()

  new_baseline_data <- list()

  for (i in seq_len(nrow(new_locations))) {
    loc <- new_locations[i, ]
    loc_id        <- loc$location_id
    loc_name      <- loc$name
    source_str    <- loc$source
    data_type_str <- if (!is.na(loc$data_type)) loc$data_type else "Storage"
    src_type      <- classify_source(source_str)

    type_suffix <- if (tolower(data_type_str) == "elevation") " (elevation->storage)" else ""
    message(sprintf("[%d/%d] Backfilling %s (ID: %s) [%s]%s...",
                    i, nrow(new_locations), loc_name, loc_id, src_type, type_suffix))

    # Fetch full historical range based on source type. The per-source logic
    # lives in fetch_full_history() (helper_functions.R) so the daily
    # auto-backfill and the standalone backfill_history.R routine stay in sync.
    hist_data <- fetch_full_history(loc, FETCH_START, BACKFILL_END)

    if (!is.null(hist_data) && nrow(hist_data) > 0) {
      message(sprintf("    Retrieved %d historical observations", nrow(hist_data)))
      new_baseline_data[[length(new_baseline_data) + 1]] <- hist_data
    } else {
      message(sprintf("    No historical data retrieved"))
    }

    Sys.sleep(1)  # Rate limiting for bulk fetches
  }

  # Combine and append to baseline
  if (length(new_baseline_data) > 0) {
    new_baseline <- bind_rows(new_baseline_data)
    message(sprintf("\nTotal new historical observations: %d", nrow(new_baseline)))

    # Load existing baseline and append
    if (file.exists(baseline_file)) {
      existing_baseline <- read_parquet(baseline_file)
      combined_baseline <- bind_rows(existing_baseline, new_baseline)
    } else {
      combined_baseline <- new_baseline
    }

    # Save updated baseline
    write_parquet(combined_baseline, baseline_file)
    message(sprintf("Updated historical_baseline.parquet: %d total observations",
                    nrow(combined_baseline)))

    # Compute statistics for new locations
    message("\nComputing statistics for new locations...")

    new_stats <- new_baseline |>
      filter(date >= BASELINE_START, date <= BASELINE_END) |>
      mutate(month = month(date), day = day(date)) |>
      group_by(location_id, month, day) |>
      summarize(
        min  = min(value, na.rm = TRUE),
        max  = max(value, na.rm = TRUE),
        mean = mean(value, na.rm = TRUE),
        p10  = quantile(value, 0.10, na.rm = TRUE),
        p25  = quantile(value, 0.25, na.rm = TRUE),
        p50  = quantile(value, 0.50, na.rm = TRUE),
        p75  = quantile(value, 0.75, na.rm = TRUE),
        p90  = quantile(value, 0.90, na.rm = TRUE),
        count = sum(!is.na(value)),   # canonical column name (matches calculate_daily_stats / stats parquet)
        unit = first(unit),
        .groups = "drop"
      )

    # Append to historical statistics
    stats_file <- file.path(OUTPUT_DIR, "historical_statistics.parquet")
    if (file.exists(stats_file)) {
      existing_stats <- read_parquet(stats_file)
      combined_stats <- bind_rows(existing_stats, new_stats)
    } else {
      combined_stats <- new_stats
    }

    write_parquet(combined_stats, stats_file)
    message(sprintf("Updated historical_statistics.parquet: %d total rows",
                    nrow(combined_stats)))

    # Reload historical_stats for use in daily processing
    historical_stats <- combined_stats

    # Generate backfill rows for new locations - to be merged into daily CSV
    message("\n=== Generating backfill rows for new locations ===\n")

    # For each date in new_baseline, generate a CSV row with statistics.
    # These rows are appended to the daily droughtData CSV (only on first
    # detection of a new location). build_drought_csv_rows() (helper_functions.R)
    # is shared with backfill_history.R so the row layout cannot drift.
    backfill_rows <- build_drought_csv_rows(new_baseline, locations, new_stats, STATS_PERIOD)

    message(sprintf("Backfill prepared: %d rows for %d new location(s) (will be merged into daily CSV)",
                    nrow(backfill_rows), n_distinct(backfill_rows$SiteName)))

    PARQUET_UPDATED <- TRUE  # Flag to upload parquet files at end
  } else {
    message("\nNo historical data retrieved for new locations")
    backfill_rows <- NULL
    PARQUET_UPDATED <- FALSE
  }
} else {
  message("\nNo new locations detected")
  backfill_rows <- NULL
  PARQUET_UPDATED <- FALSE
}

# Reload statistics after potential updates
historical_stats <- read_parquet(file.path(OUTPUT_DIR, "historical_statistics.parquet"))

# Track which locations have historical statistics (for logging)
locations_with_stats <- unique(historical_stats$location_id)
n_with_stats <- sum(locations$location_id %in% locations_with_stats)
message(sprintf("  %d locations have historical statistics", n_with_stats))
message(sprintf("  %d locations will have NA for historical metrics",
                nrow(locations) - n_with_stats))

################################################################################
# DATA FETCHING FUNCTIONS
################################################################################

#' Fetch from RISE via WWDH API
#' @param data_type "Storage" or "Elevation" - if Elevation, converts to storage using curve
#' Returns tibble(date, value, unit, url) with all available daily values in range
fetch_rise <- function(location_id, target_date, lookback_days = LOOKBACK_DAYS,
                       data_type = "Storage", rise_param_id = NA_character_) {
  # Query the entire lookback window at once (not day-by-day).
  # The WWDH API returns empty results for single-day queries on recent dates
  # but returns data correctly when queried as a date range.
  # Randomize end date far in future to bust WWDH API cache layer.
  # Each run gets a unique URL, avoiding stale cached responses.
  start_date <- target_date - lookback_days
  end_date   <- target_date + sample(30:90, 1)

  empty <- tibble(date = as.Date(character(0)), value = numeric(0),
                  unit = character(0), url = character(0))

  # The WWDH endpoint now requires parameter-name=<numeric id> (e.g. "3" for
  # storage). Without it the server returns HTTP 500. The id comes from the
  # "RISE Parameter ID for Storage Data" column in locations.csv.
  # If unknown ("TBD" / empty / NA), skip this location entirely.
  pid <- if (is.na(rise_param_id)) "" else trimws(as.character(rise_param_id))
  if (pid == "" || toupper(pid) == "TBD") {
    return(empty)
  }

  url <- paste0(
    WWDH_API_BASE,
    "/collections/rise-edr/locations/", location_id,
    "?parameter-name=", pid,
    "&limit=50",
    "&datetime=", start_date, "/", end_date,
    "&f=json"
  )

  tryCatch({
    response <- request(url) |>
      req_timeout(60) |>
      req_retry(max_tries = 4, backoff = ~ 3,
                is_transient = ~ resp_status(.x) %in% c(408, 425, 429, 500, 502, 503, 504)) |>
      req_perform()

    if (resp_status(response) != 200) return(empty)

    body <- resp_body_json(response)

    coverages <- body$coverages
    if (is.null(coverages) || length(coverages) == 0) return(empty)

    # Identify the storage parameter key from the parameters block.
    # Look for param whose label contains "Storage" (typically key "3").
    storage_key <- NULL
    params <- body$parameters
    if (!is.null(params)) {
      for (pk in names(params)) {
        label <- tryCatch(params[[pk]]$observedProperty$label$en, error = function(e) "")
        if (!is.null(label) && grepl("Storage", label, ignore.case = TRUE) &&
            !grepl("Change In Storage|Bank Storage", label, ignore.case = TRUE)) {
          storage_key <- pk
          break
        }
      }
    }
    # Fallback: use key "3" (the standard RISE storage parameter)
    if (is.null(storage_key)) storage_key <- "3"

    # Collect date/value pairs from storage coverages only
    all_rows <- list()
    for (cov in coverages) {
      # Skip modeled/forecast coverages — only use observed data
      if (!is.null(cov$isModeled) && isTRUE(cov$isModeled)) next

      t_vals <- cov$domain$axes$t$values
      ranges <- cov$ranges
      if (is.null(t_vals) || length(t_vals) == 0 || is.null(ranges) || length(ranges) == 0) next

      # Only process coverages that contain the storage parameter
      if (!storage_key %in% names(ranges)) next

      raw_values <- ranges[[storage_key]]$values
      if (is.null(raw_values) || length(raw_values) == 0) next

      for (i in seq_along(raw_values)) {
        if (!is.null(raw_values[[i]])) {
          all_rows[[length(all_rows) + 1]] <- list(
            date  = as.Date(substr(t_vals[[i]], 1, 10)),
            value = as.numeric(raw_values[[i]])
          )
        }
      }
    }

    if (length(all_rows) == 0) return(empty)

    # Unit from parameters block
    raw_unit <- tryCatch({
      u <- body$parameters[[storage_key]]$unit$symbol
      if (is.null(u)) "af" else u
    }, error = function(e) "af")

    result <- bind_rows(all_rows) |>
      mutate(unit = raw_unit, url = url) |>
      distinct(date, .keep_all = TRUE) |>
      arrange(desc(date))

    # Convert elevation to storage if needed
    if (tolower(data_type) == "elevation") {
      result <- result |>
        rowwise() |>
        mutate(value = elevation_to_storage(location_id, value)) |>
        ungroup() |>
        filter(!is.na(value)) |>
        mutate(unit = "af")
    }

    return(result)
  }, error = function(e) {
    return(empty)
  })
}

#' Fetch from USACE CDA API
#' The location_id is "provider/ts_name" (e.g., "spa/Abiquiu.Stor.Inst.15Minutes.0.DCP-rev")
#' @param data_type "Storage" or "Elevation" - if Elevation, converts to storage using curve
#' Returns tibble(date, value, unit, url) with all available daily values in range
fetch_usace <- function(location_id, target_date, lookback_days = LOOKBACK_DAYS,
                        data_type = "Storage") {
  id_str <- as.character(location_id)
  slash_pos <- str_locate(id_str, "/")[1, "start"]

  empty <- tibble(date = as.Date(character(0)), value = numeric(0),
                  unit = character(0), url = character(0))

  if (is.na(slash_pos)) {
    message(sprintf("    USACE: invalid identifier format '%s' (expected 'provider/ts_name')", id_str))
    return(empty)
  }

  provider <- str_sub(id_str, 1, slash_pos - 1)
  ts_name  <- str_sub(id_str, slash_pos + 1)

  begin_date <- target_date - lookback_days
  end_date   <- target_date + 1
  begin_str  <- paste0(format(begin_date, "%Y-%m-%dT00:00:00"), ".000Z")
  end_str    <- paste0(format(end_date, "%Y-%m-%dT00:00:00"), ".000Z")

  url <- sprintf(
    "https://water.usace.army.mil/cda/reporting/providers/%s/timeseries?name=%s&begin=%s&end=%s&format=csv",
    provider, URLencode(ts_name, reserved = TRUE), begin_str, end_str
  )

  tryCatch({
    response <- request(url) |>
      req_timeout(60) |>
      req_retry(max_tries = 2, backoff = ~ 2) |>
      req_perform()

    body <- resp_body_string(response)
    body <- str_replace_all(body, "\r", "")
    lines <- str_split(body, "\n")[[1]]
    data_lines <- lines[!str_starts(lines, "##") & nchar(trimws(lines)) > 0]

    if (length(data_lines) == 0) return(empty)

    # Extract unit from ## comment lines
    unit_line <- lines[str_starts(lines, "##unit:")]
    unit_val <- if (length(unit_line) > 0) {
      trimws(str_remove(unit_line[1], "##unit:"))
    } else {
      "ac-ft"
    }

    parsed <- tibble(raw = data_lines) |>
      mutate(
        datetime = str_extract(raw, "^[^,]+"),
        value    = as.numeric(str_extract(raw, "[^,]+$")),
        date     = as.Date(str_sub(datetime, 1, 10))
      ) |>
      filter(!is.na(value)) |>
      distinct(date, .keep_all = TRUE) |>
      arrange(desc(date)) |>
      transmute(date, value, unit = unit_val, url = url)

    if (nrow(parsed) == 0) return(empty)

    # Convert elevation to storage if needed
    if (tolower(data_type) == "elevation") {
      parsed <- parsed |>
        rowwise() |>
        mutate(value = elevation_to_storage(location_id, value)) |>
        ungroup() |>
        filter(!is.na(value)) |>
        mutate(unit = "af")
    }

    return(parsed)
  }, error = function(e) {
    message(sprintf("    USACE fetch error: %s", conditionMessage(e)))
    return(empty)
  })
}

#' Fetch from USGS Water Data OGC API (daily values)
#' Parameter 00054 = reservoir storage (acre-feet)
#' Parameter 62614 = lake/reservoir water surface elevation (ft NGVD29)
#' Parameter 62615 = lake/reservoir water surface elevation (ft NAVD88)
#' Parameter 72275 = lake/reservoir elevation (ft USBR datum, e.g. Klamath Basin)
#' Parameter 00065 = gage height (ft above local gage datum, e.g. Lake Tahoe)
#'
#' For elevation data without a param_override, tries 62614 → 72275 → 62615.
#' Set `param_override` to skip the chain and use a specific code (e.g. "00065"
#' for Lake Tahoe, where gage height is the only available reading and the
#' elevation_storage_curves.csv table is indexed by gage height).
#'
#' @param data_type "Storage" or "Elevation" - if Elevation, converts via curve
#' @param param_override Explicit USGS parameter code (or NA for default)
#' Returns tibble(date, value, unit, url) with all available daily values in range
fetch_usgs <- function(location_id, target_date, lookback_days = LOOKBACK_DAYS,
                       data_type = "Storage", param_override = NA_character_) {
  site_no <- as.character(location_id)

  start_date <- target_date - lookback_days
  end_date   <- target_date

  empty <- tibble(date = as.Date(character(0)), value = numeric(0),
                  unit = character(0), url = character(0))

  has_override <- !is.na(param_override) && nzchar(trimws(as.character(param_override)))
  if (has_override) {
    param_code <- trimws(as.character(param_override))
  } else if (tolower(data_type) == "elevation") {
    param_code <- "62614"
  } else {
    param_code <- "00054"
  }

  url <- sprintf(
    "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items?f=json&monitoring_location_id=USGS-%s&parameter_code=%s&time=%s/%s&limit=50",
    site_no, param_code, start_date, end_date
  )

  tryCatch({
    response <- request(url) |>
      req_timeout(60) |>
      req_retry(max_tries = 2, backoff = ~ 2) |>
      req_perform()

    body <- resp_body_string(response)
    data <- jsonlite::fromJSON(body, simplifyVector = FALSE)
    features <- data$features

    # If elevation query returned nothing, try alternate elevation parameters.
    # Skip the fallback chain when caller specified an explicit override.
    if (length(features) == 0 && tolower(data_type) == "elevation" && !has_override) {
      for (alt_param in c("72275", "62615")) {
        url <- sprintf(
          "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items?f=json&monitoring_location_id=USGS-%s&parameter_code=%s&time=%s/%s&limit=50",
          site_no, alt_param, start_date, end_date
        )
        response <- request(url) |>
          req_timeout(60) |>
          req_retry(max_tries = 2, backoff = ~ 2) |>
          req_perform()
        body <- resp_body_string(response)
        data <- jsonlite::fromJSON(body, simplifyVector = FALSE)
        features <- data$features
        if (length(features) > 0) break
      }
    }

    if (length(features) == 0) return(empty)

    parsed <- tibble(
      date  = as.Date(sapply(features, function(f) f$properties$time)),
      value = as.numeric(sapply(features, function(f) f$properties$value)),
      unit  = sapply(features, function(f) f$properties$unit_of_measure)
    ) |>
      filter(!is.na(value)) |>
      distinct(date, .keep_all = TRUE) |>
      arrange(desc(date)) |>
      mutate(unit = tolower(unit), url = url)

    if (nrow(parsed) == 0) return(empty)

    # Convert elevation to storage if needed
    if (tolower(data_type) == "elevation") {
      parsed <- parsed |>
        rowwise() |>
        mutate(value = elevation_to_storage(location_id, value)) |>
        ungroup() |>
        filter(!is.na(value)) |>
        mutate(unit = "af")
    } else {
      parsed <- parsed |> mutate(unit = ifelse(unit == "acre-ft", "af", unit))
    }

    return(parsed)
  }, error = function(e) {
    message(sprintf("    USGS OGC API error: %s", conditionMessage(e)))
    return(empty)
  })
}

#' Fetch from CDEC API
#' Sensor 15 = reservoir storage
#' The location_id IS the CDEC station code (from geojson Identifier)
#' @param data_type "Storage" or "Elevation" - if Elevation, converts to storage using curve
#' Returns list(value, date, unit, url)
fetch_cdec <- function(location_id, target_date, lookback_days = LOOKBACK_DAYS,
                       data_type = "Storage") {
  station <- as.character(location_id)

  start_date <- target_date - lookback_days
  end_date   <- target_date

  empty <- tibble(date = as.Date(character(0)), value = numeric(0),
                  unit = character(0), url = character(0))

  url <- sprintf(
    "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=%s&SensorNums=15&dur_code=D&Start=%s&End=%s",
    station, start_date, end_date
  )

  tryCatch({
    response <- request(url) |>
      req_timeout(60) |>
      req_retry(max_tries = 2, backoff = ~ 2) |>
      req_perform()

    body <- resp_body_string(response)
    data <- read_csv(I(body), show_col_types = FALSE,
                     col_types = cols(`DATE TIME` = col_character(),
                                     `OBS DATE` = col_character(),
                                     .default = col_guess()))

    if (nrow(data) == 0 || !"VALUE" %in% names(data)) return(empty)

    unit_val <- if ("UNITS" %in% names(data)) tolower(data$UNITS[1]) else "af"

    parsed <- data |>
      mutate(date  = as.Date(str_sub(`DATE TIME`, 1, 8), format = "%Y%m%d"),
             value = as.numeric(VALUE)) |>
      filter(!is.na(value)) |>
      distinct(date, .keep_all = TRUE) |>
      arrange(desc(date)) |>
      transmute(date, value, unit = unit_val, url = url)

    if (nrow(parsed) == 0) return(empty)

    # Convert elevation to storage if needed
    if (tolower(data_type) == "elevation") {
      parsed <- parsed |>
        rowwise() |>
        mutate(value = elevation_to_storage(location_id, value)) |>
        ungroup() |>
        filter(!is.na(value)) |>
        mutate(unit = "af")
    }

    return(parsed)
  }, error = function(e) {
    return(empty)
  })
}

#' Master fetch: dispatch to the correct source-specific function
#' @param data_type "Storage" or "Elevation" - passed to source-specific fetchers
#' Returns tibble(date, value, unit, url) with all available daily values in range
fetch_all_values <- function(location_id, source_str, target_date,
                             lookback_days = LOOKBACK_DAYS,
                             data_type = "Storage",
                             rise_param_id = NA_character_,
                             usgs_param_code = NA_character_) {
  src_type <- classify_source(source_str)

  result <- switch(src_type,
    "rise"     = fetch_rise(location_id, target_date, lookback_days, data_type, rise_param_id),
    "usace_cda" = fetch_usace(location_id, target_date, lookback_days, data_type),
    "usgs"     = fetch_usgs(location_id, target_date, lookback_days, data_type, usgs_param_code),
    "cdec"     = fetch_cdec(location_id, target_date, lookback_days, data_type),
    fetch_rise(location_id, target_date, lookback_days, data_type, rise_param_id)
  )

  return(result)
}

################################################################################
# MAIN PROCESSING LOOP
################################################################################

# Fetch once per location (using the 7-day lookback window), then extract
# values for each of the last REPORT_DAYS days from the single API response.

report_dates <- TARGET_DATE - seq(0, REPORT_DAYS - 1)  # e.g., Apr 1, Mar 31, Mar 30
message(sprintf("\n=== Fetching values (report dates: %s) ===\n",
                paste(report_dates, collapse = ", ")))

all_location_rows <- list()

for (i in seq_len(nrow(locations))) {
  loc <- locations[i, ]
  location_id   <- loc$location_id
  location_name <- loc$name
  source_str    <- loc$source
  data_type_str <- if (!is.na(loc$data_type)) loc$data_type else "Storage"

  src_type <- classify_source(source_str)
  type_suffix <- if (tolower(data_type_str) == "elevation") " (elevation->storage)" else ""
  message(sprintf("[%d/%d] %s (ID: %s) [%s]%s...",
                  i, nrow(locations), location_name, location_id, src_type, type_suffix))

  # Skip locations with no valid identifier
  if (is.na(location_id) || location_id == "--" || location_id == "") {
    message(sprintf("  Skipping: no valid identifier"))
    for (di in seq_along(report_dates)) {
      all_location_rows[[length(all_location_rows) + 1]] <- tibble(
        location_id = location_id, name = location_name,
        report_date = report_dates[di], data_value = NA_real_,
        data_date = as.Date(NA), data_unit = NA_character_, data_url = NA_character_
      )
    }
    next
  }

  # Single API call returns all daily values in the lookback window
  all_vals <- fetch_all_values(location_id, source_str, TARGET_DATE,
                                data_type = data_type_str,
                                rise_param_id = loc$rise_param_id,
                                usgs_param_code = loc$usgs_param_code)

  if (nrow(all_vals) == 0) {
    message(sprintf("  No data found"))
    for (di in seq_along(report_dates)) {
      all_location_rows[[length(all_location_rows) + 1]] <- tibble(
        location_id = location_id, name = location_name,
        report_date = report_dates[di], data_value = NA_real_,
        data_date = as.Date(NA), data_unit = NA_character_, data_url = NA_character_
      )
    }
    next
  }

  # For each report date, find the most recent value on or before that date
  for (di in seq_along(report_dates)) {
    rd <- report_dates[di]
    candidates <- all_vals |> filter(date <= rd)
    if (nrow(candidates) > 0) {
      best <- candidates |> slice(1)  # already sorted desc by date
      all_location_rows[[length(all_location_rows) + 1]] <- tibble(
        location_id = location_id, name = location_name,
        report_date = rd,
        data_value = best$value, data_date = best$date,
        data_unit = best$unit, data_url = best$url
      )
    } else {
      all_location_rows[[length(all_location_rows) + 1]] <- tibble(
        location_id = location_id, name = location_name,
        report_date = rd, data_value = NA_real_, data_date = as.Date(NA),
        data_unit = NA_character_, data_url = all_vals$url[1]
      )
    }
  }

  dates_found <- all_vals |> filter(date %in% report_dates) |> pull(date)
  message(sprintf("  Latest: %s %s (date: %s) | %d/%d report dates covered",
                  format(all_vals$value[1], big.mark = ","),
                  all_vals$unit[1], all_vals$date[1],
                  length(dates_found), REPORT_DAYS))

  Sys.sleep(0.25)
}

# Combine all rows
current_data <- bind_rows(all_location_rows)

################################################################################
# JOIN WITH HISTORICAL STATISTICS
################################################################################

message("\n=== Joining with historical statistics ===\n")

# For each row, look up stats for that row's report_date (not data_date)
# so percentiles match the calendar day being reported
output_data <- current_data |>
  left_join(locations, by = c("location_id", "name")) |>
  mutate(
    stat_month = month(report_date),
    stat_day   = day(report_date)
  ) |>
  left_join(
    historical_stats |> select(location_id, month, day, min, max, p10, p25, p50, p75, p90, mean, unit),
    by = c("location_id", "stat_month" = "month", "stat_day" = "day"),
    suffix = c("", "_hist")
  ) |>
  mutate(
    pct_median    = data_value / p50,
    pct_average   = data_value / mean,
    pct_full      = data_value / capacity,
    data_date_fmt = format(data_date, "%m/%d/%Y"),
    date_queried  = format(Sys.Date(), "%m/%d/%Y")
  ) |>
  select(-stat_month, -stat_day)

################################################################################
# GENERATE OUTPUT CSV
################################################################################

message("\n=== Generating output CSV ===\n")

output_csv <- output_data |>
  transmute(
    SiteName       = label_popup,
    Lat            = latitude,
    Lon            = longitude,
    State          = state,
    DoiRegion      = doi_region,
    Huc6           = huc6,
    DataUnits      = coalesce(data_unit, unit),
    DataValue      = data_value,
    DataDate       = data_date_fmt,
    DateQueried    = date_queried,
    DataDateMax    = max,
    DataDateP90    = p90,
    DataDateP75    = p75,
    DataDateP50    = p50,
    DataDateP25    = p25,
    DataDateP10    = p10,
    DataDateMin    = min,
    DataDateAvg    = mean,
    DataValuePctMdn = pct_median,
    DataValuePctAvg = pct_average,
    StatsPeriod    = STATS_PERIOD,
    MaxCapacity    = capacity,
    ActiveCapacity = active_capacity,
    PctFull        = pct_full,
    TeacupUrl      = NA_character_,
    DataUrl        = data_url,
    Comment        = NA_character_
  )

# Append backfill rows (historical data for newly-detected locations) to daily CSV.
# Backfill only happens on first detection of a new location, so this is one-shot per location.
n_backfill <- 0
if (exists("backfill_rows") && !is.null(backfill_rows) && nrow(backfill_rows) > 0) {
  n_backfill <- nrow(backfill_rows)
  output_csv <- bind_rows(output_csv, backfill_rows)
  message(sprintf("Merged %d backfill row(s) for %d new location(s) into daily CSV",
                  n_backfill, n_distinct(backfill_rows$SiteName)))
}

# Merge any pending gap-fill rows staged by the monthly backfill routine
# (backfill_history.R). The standalone backfill cannot push to the database
# directly — the downstream loader only ingests droughtData CSVs — so it stages
# gap-filled observations as pending_backfill.parquet on HydroShare and the next
# daily run rides them into the daily CSV (keyed by their real historical
# DataDate). The staging file is deleted after a successful upload below so the
# rows are emitted exactly once. Rows are pre-rendered in the daily CSV schema
# by build_drought_csv_rows(), so the types align for bind_rows().
n_pending <- 0
pending_file <- file.path(OUTPUT_DIR, "pending_backfill.parquet")
# Defensive: start from a clean slate so we only ever merge a freshly downloaded
# staging file, never a leftover local copy from earlier in this run.
if (file.exists(pending_file)) file.remove(pending_file)
if (hs_username != "" && hs_password != "") {
  got_pending <- download_parquet_from_hydroshare(
    "pending_backfill.parquet", pending_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
  if (got_pending && file.exists(pending_file)) {
    pending_rows <- tryCatch(read_parquet(pending_file), error = function(e) NULL)
    if (!is.null(pending_rows) && nrow(pending_rows) > 0) {
      n_pending <- nrow(pending_rows)
      output_csv <- bind_rows(output_csv, pending_rows)
      message(sprintf("Merged %d pending gap-fill row(s) from monthly backfill into daily CSV",
                      n_pending))
    }
  }
}

# Generate filename and write to hydroshare directory (git-ignored, uploaded to HS)
output_filename <- sprintf("droughtData%s.csv", format(TARGET_DATE, "%Y%m%d"))
if (!dir.exists(HYDROSHARE_DIR)) dir.create(HYDROSHARE_DIR, recursive = TRUE)
output_path <- file.path(HYDROSHARE_DIR, output_filename)

# Write CSV (standard comma-separated)
write_csv(output_csv, output_path, na = "")

message(sprintf("Output written to: %s", output_path))
message(sprintf("  Total rows: %d (%d locations x %d days + %d backfill)",
                nrow(output_csv), nrow(locations), REPORT_DAYS, n_backfill))
message(sprintf("  With data: %d", sum(!is.na(output_csv$DataValue))))
message(sprintf("  Missing data: %d", sum(is.na(output_csv$DataValue))))
message(sprintf("  With historical stats: %d", sum(!is.na(output_csv$DataDateP50))))
message(sprintf("  Without historical stats: %d", sum(is.na(output_csv$DataDateP50))))

# Source type breakdown
source_counts <- locations |>
  mutate(src_type = sapply(source, classify_source)) |>
  count(src_type)
message("\n  Source breakdown:")
for (j in seq_len(nrow(source_counts))) {
  message(sprintf("    %s: %d", source_counts$src_type[j], source_counts$n[j]))
}

################################################################################
# UPLOAD TO HYDROSHARE
################################################################################

message("\n=== Uploading to HydroShare ===\n")

if (hs_username == "" || hs_password == "") {
  message("WARNING: HYDROSHARE_USERNAME and/or HYDROSHARE_PASSWORD not set.")
  message("Skipping HydroShare upload. Set environment variables to enable upload.")
} else {

  upload_to_hydroshare <- function(file_path, resource_id, username, password) {
    filename <- basename(file_path)

    # First, try to delete the existing file (if updating daily, old file may exist)
    delete_url <- sprintf("%s/hsapi/resource/%s/files/%s/",
                          HYDROSHARE_BASE_URL, resource_id, filename)

    tryCatch({
      request(delete_url) |>
        req_auth_basic(username, password) |>
        req_method("DELETE") |>
        req_timeout(60) |>
        req_perform()
      message(sprintf("  Deleted existing file: %s", filename))
    }, error = function(e) {
      message(sprintf("  No existing file to delete (or delete failed): %s", filename))
    })

    # Upload the new file
    upload_url <- sprintf("%s/hsapi/resource/%s/files/",
                          HYDROSHARE_BASE_URL, resource_id)

    response <- request(upload_url) |>
      req_auth_basic(username, password) |>
      req_body_multipart(file = curl::form_file(file_path)) |>
      req_timeout(120) |>
      req_perform()

    status <- resp_status(response)
    if (status >= 200 && status < 300) {
      message(sprintf("  Successfully uploaded %s to HydroShare resource %s",
                      filename, resource_id))
    } else {
      warning(sprintf("  Upload returned status %d", status))
    }

    return(status)
  }

  tryCatch({
    upload_to_hydroshare(output_path, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
  }, error = function(e) {
    message(sprintf("ERROR uploading to HydroShare: %s", e$message))
  })

  # NOTE: the staged pending_backfill.parquet is NOT deleted here. These rows are
  # only durably persisted once the downstream loader ingests this CSV, which is a
  # SEPARATE workflow step (gcloud run jobs execute) that runs after this container
  # exits — its success is not observable from R. Deleting here on upload-success
  # would drop the rows permanently if that load step then failed (a later daily
  # run would regenerate the CSV without these out-of-window historical dates).
  # The daily workflow deletes pending_backfill.parquet only AFTER the load step
  # succeeds; re-merging it on intervening runs is safe (idempotent insert-or-ignore).

  # Upload updated parquet files if backfill occurred
  # This ensures the next run starts with the latest historical data
  if (exists("PARQUET_UPDATED") && PARQUET_UPDATED) {
    message("\n=== Uploading updated parquet files to HydroShare ===")

    stats_file <- file.path(OUTPUT_DIR, "historical_statistics.parquet")
    baseline_file <- file.path(OUTPUT_DIR, "historical_baseline.parquet")

    if (file.exists(stats_file)) {
      message(sprintf("Uploading historical_statistics.parquet (%.1f MB)...",
                      file.size(stats_file) / 1e6))
      tryCatch({
        upload_to_hydroshare(stats_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
      }, error = function(e) {
        message(sprintf("ERROR uploading historical_statistics.parquet: %s", e$message))
      })
    }

    if (file.exists(baseline_file)) {
      message(sprintf("Uploading historical_baseline.parquet (%.1f MB)...",
                      file.size(baseline_file) / 1e6))
      tryCatch({
        upload_to_hydroshare(baseline_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
      }, error = function(e) {
        message(sprintf("ERROR uploading historical_baseline.parquet: %s", e$message))
      })
    }

    message("Parquet files uploaded - next run will use updated historical data")
  }
}

################################################################################
# SUMMARY
################################################################################

message("\n=== Summary ===")
message(sprintf("Target date: %s (%d days of data)", TARGET_DATE, REPORT_DAYS))
message(sprintf("Report dates: %s", paste(report_dates, collapse = ", ")))
message(sprintf("Total rows: %d (%d locations x %d days)", nrow(output_csv), nrow(locations), REPORT_DAYS))
message(sprintf("Rows with current data: %d (%.1f%%)",
                sum(!is.na(output_csv$DataValue)),
                100 * sum(!is.na(output_csv$DataValue)) / nrow(output_csv)))
message(sprintf("Rows with historical stats: %d (%.1f%%)",
                sum(!is.na(output_csv$DataDateP50)),
                100 * sum(!is.na(output_csv$DataDateP50)) / nrow(output_csv)))
message(sprintf("Output file: %s", output_path))
message(sprintf("Completed at: %s", Sys.time()))
