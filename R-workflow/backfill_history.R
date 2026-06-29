# backfill_history.R
#
# MONTHLY (or on-demand) BACKFILL: Download the entire available daily record
# for every reservoir, find any observations missing from the historical
# baseline, fetch and append them, recompute the affected day-of-year
# statistics, and upload the updated parquet files to HydroShare.
#
# Because the daily script (rezviz_data_generator.R) downloads the latest
# parquet files from HydroShare at the start of every run, uploading the
# refreshed baseline/statistics here is what "adds the backfilled data to the
# daily run" — the next daily run automatically picks up the gap-filled record
# and the recomputed percentiles. No Docker image rebuild is required.
#
# This complements the daily script's auto-backfill (which only fires for
# brand-new locations): this routine sweeps EVERY location for gaps, including
# dates that were missed because a source API was temporarily unavailable.
#
# Usage:
#   Rscript backfill_history.R                # all locations
#   Rscript backfill_history.R 7166,393,THC   # only these location_ids
#
# Environment:
#   HYDROSHARE_USERNAME / HYDROSHARE_PASSWORD  (loaded from .env if present)
#   BACKFILL_DRY_RUN=1                          # skip the HydroShare upload
#
# Author: Kyle Onda, CGS
# Created: 2026-06-29
################################################################################

library(httr2)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(stringr)
library(sf)
library(jsonlite)

# Shared utilities: elevation->storage curves, classify_source(),
# calculate_daily_stats(), and the per-source fetch_full_history().
source("helper_functions.R")

# Load .env file if present
if (file.exists(".env")) {
  readRenviron(".env")
}

################################################################################
# CONFIGURATION
################################################################################

WWDH_API_BASE <- "https://api.wwdh.internetofwater.app"

HYDROSHARE_RESOURCE_ID <- "22b2f10103e5426a837defc00927afbd"
HYDROSHARE_BASE_URL    <- "https://www.hydroshare.org"

OUTPUT_DIR <- "output"
CONFIG_DIR <- "config"

# Day-of-year statistics period (30 water years). Statistics are ALWAYS computed
# over this window. Backfilled observations that fall inside it change the
# percentiles; observations outside it only extend the raw baseline record.
BASELINE_START <- as.Date("1990-10-01")
BASELINE_END   <- as.Date("2020-09-30")

# StatsPeriod label written into the staged CSV rows (must match the daily script).
STATS_PERIOD <- "10/1/1990 - 9/30/2020"

# Full-history fetch range. Reach back to the 19th century so no early data is
# arbitrarily cut off; sources without old data simply return nothing for those
# years. Override with BACKFILL_FETCH_START=YYYY-MM-DD if a shorter sweep is
# wanted. Pull through today.
FETCH_START <- {
  override <- Sys.getenv("BACKFILL_FETCH_START", "")
  if (nzchar(override)) as.Date(override) else as.Date("1870-01-01")
}
BACKFILL_END <- Sys.Date()

DRY_RUN <- Sys.getenv("BACKFILL_DRY_RUN", "") %in% c("1", "true", "TRUE")

# Optional positional arg: comma-separated location_ids to restrict the sweep.
args <- commandArgs(trailingOnly = TRUE)
restrict_ids <- if (length(args) > 0 && nzchar(trimws(args[1]))) {
  trimws(str_split(args[1], ",")[[1]])
} else {
  character(0)
}

message("=== Reservoir History Backfill ===")
message(sprintf("Run time: %s", Sys.time()))
message(sprintf("Fetch range: %s -> %s", FETCH_START, BACKFILL_END))
message(sprintf("Statistics window: %s -> %s", BASELINE_START, BASELINE_END))
if (length(restrict_ids) > 0) {
  message(sprintf("Restricted to %d location id(s): %s",
                  length(restrict_ids), paste(restrict_ids, collapse = ", ")))
}
if (DRY_RUN) message("DRY RUN: parquet files will NOT be uploaded to HydroShare")

stats_file    <- file.path(OUTPUT_DIR, "historical_statistics.parquet")
baseline_file <- file.path(OUTPUT_DIR, "historical_baseline.parquet")

hs_username <- Sys.getenv("HYDROSHARE_USERNAME", "")
hs_password <- Sys.getenv("HYDROSHARE_PASSWORD", "")

################################################################################
# HYDROSHARE I/O
################################################################################

download_parquet_from_hydroshare <- function(filename, dest_path, resource_id, username, password) {
  url <- sprintf("%s/hsapi/resource/%s/files/%s/", HYDROSHARE_BASE_URL, resource_id, filename)
  tmp <- paste0(dest_path, ".tmp")
  tryCatch({
    req <- request(url) |> req_timeout(300)
    if (username != "" && password != "") {
      req <- req |> req_auth_basic(username, password)
    }
    req |> req_perform() |> resp_body_raw() |> writeBin(tmp)
    magic <- readBin(tmp, "raw", n = 4)
    if (rawToChar(magic) == "PAR1") {
      file.rename(tmp, dest_path)
      message(sprintf("  Updated %s from HydroShare (%.1f MB)", filename, file.size(dest_path) / 1e6))
      return(TRUE)
    } else {
      file.remove(tmp)
      message(sprintf("  WARNING: Downloaded %s failed validation, keeping local file", filename))
      return(FALSE)
    }
  }, error = function(e) {
    if (file.exists(tmp)) file.remove(tmp)
    message(sprintf("  WARNING: Could not download %s: %s - using local file", filename, e$message))
    return(FALSE)
  })
}

upload_to_hydroshare <- function(file_path, resource_id, username, password) {
  filename <- basename(file_path)

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

  upload_url <- sprintf("%s/hsapi/resource/%s/files/",
                        HYDROSHARE_BASE_URL, resource_id)
  response <- request(upload_url) |>
    req_auth_basic(username, password) |>
    req_body_multipart(file = curl::form_file(file_path)) |>
    req_timeout(300) |>
    req_perform()

  status <- resp_status(response)
  if (status >= 200 && status < 300) {
    message(sprintf("  Successfully uploaded %s", filename))
  } else {
    warning(sprintf("  Upload of %s returned status %d", filename, status))
  }
  status
}

################################################################################
# SYNC PARQUET FROM HYDROSHARE
################################################################################

message("\nSyncing parquet files from HydroShare...")
download_parquet_from_hydroshare("historical_baseline.parquet",   baseline_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
download_parquet_from_hydroshare("historical_statistics.parquet", stats_file,    HYDROSHARE_RESOURCE_ID, hs_username, hs_password)

if (!file.exists(baseline_file)) {
  stop("historical_baseline.parquet not found and could not be downloaded from HydroShare.")
}

baseline <- read_parquet(baseline_file)
message(sprintf("Loaded baseline: %d observations for %d locations",
                nrow(baseline), n_distinct(baseline$location_id)))

################################################################################
# LOAD LOCATION METADATA (same shape as the daily script)
################################################################################

locations_file <- file.path(CONFIG_DIR, "locations.geojson")
locations_sf <- st_read(locations_file, quiet = TRUE)

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
    data_type = `Storage.Data.Type`
  )

locations$rise_param_id <- if ("RISE.Parameter.ID.for.Storage.Data" %in% names(locations_sf)) {
  locations_sf$`RISE.Parameter.ID.for.Storage.Data`
} else {
  NA_character_
}
locations$usgs_param_code <- if ("USGS.Parameter.Code" %in% names(locations_sf)) {
  locations_sf$`USGS.Parameter.Code`
} else {
  NA_character_
}

# Drop locations without a usable identifier, and apply the optional restriction.
locations <- locations |>
  filter(!is.na(location_id), location_id != "--", location_id != "")
if (length(restrict_ids) > 0) {
  locations <- locations |> filter(location_id %in% restrict_ids)
}

message(sprintf("Sweeping %d location(s) for missing data\n", nrow(locations)))

# Elevation->storage curves must be in scope for fetch_full_history() (USGS
# elevation conversion). load_elevation_curves() lives in helper_functions.R.
elev_curves_data <- load_elevation_curves(CONFIG_DIR)
elev_curves    <- elev_curves_data$curves
elev_curve_ids <- elev_curves_data$ids

################################################################################
# SWEEP EACH LOCATION FOR MISSING OBSERVATIONS
################################################################################

# Existing (location_id, date) pairs, used to identify gaps without re-adding
# observations we already hold.
existing_keys <- baseline |> distinct(location_id, date)

new_rows_list <- list()
affected_ids  <- character(0)
n_failed      <- 0

for (i in seq_len(nrow(locations))) {
  loc      <- locations[i, ]
  loc_id   <- loc$location_id
  loc_name <- loc$name
  src_type <- classify_source(loc$source)

  message(sprintf("[%d/%d] %s (ID: %s) [%s]...",
                  i, nrow(locations), loc_name, loc_id, src_type))

  hist_data <- tryCatch(
    fetch_full_history(loc, FETCH_START, BACKFILL_END),
    error = function(e) {
      message(sprintf("    ERROR: %s", conditionMessage(e)))
      NULL
    }
  )

  if (is.null(hist_data) || nrow(hist_data) == 0) {
    message("    No data retrieved")
    n_failed <- n_failed + 1
    Sys.sleep(1)
    next
  }

  # Keep only observations we do not already have for this location.
  have_dates <- existing_keys |> filter(location_id == loc_id) |> pull(date)
  missing <- hist_data |>
    filter(!(date %in% have_dates)) |>
    select(location_id, date, value, unit)

  message(sprintf("    Retrieved %d observations; %d missing from baseline",
                  nrow(hist_data), nrow(missing)))

  if (nrow(missing) > 0) {
    new_rows_list[[length(new_rows_list) + 1]] <- missing
    affected_ids <- c(affected_ids, loc_id)
  }

  Sys.sleep(1)  # rate limiting for bulk fetches
}

################################################################################
# APPLY UPDATES
################################################################################

if (length(new_rows_list) == 0) {
  message("\nNo missing observations found across any location. Nothing to update.")
  message("\n=== Backfill complete (no changes) ===")
  quit(save = "no", status = 0)
}

new_rows <- bind_rows(new_rows_list)
message(sprintf("\nTotal missing observations to add: %d across %d location(s)",
                nrow(new_rows), length(affected_ids)))

# --- Update baseline -------------------------------------------------------
combined_baseline <- bind_rows(baseline, new_rows) |>
  distinct(location_id, date, .keep_all = TRUE)

write_parquet(combined_baseline, baseline_file)
message(sprintf("Updated historical_baseline.parquet: %d observations (+%d)",
                nrow(combined_baseline), nrow(combined_baseline) - nrow(baseline)))

# --- Recompute statistics for affected locations ---------------------------
# Only the 30-water-year window feeds the day-of-year statistics.
message("\nRecomputing day-of-year statistics for affected locations...")

CANON_STAT_COLS <- c("location_id", "month", "day", "min", "max",
                     "p10", "p25", "p50", "p75", "p90", "mean", "count", "unit")

existing_stats <- if (file.exists(stats_file)) read_parquet(stats_file) else tibble()

recomputed <- list()
for (loc_id in affected_ids) {
  loc_baseline <- combined_baseline |>
    filter(location_id == loc_id, date >= BASELINE_START, date <= BASELINE_END)
  if (nrow(loc_baseline) == 0) next
  recomputed[[loc_id]] <- calculate_daily_stats(loc_baseline, loc_id)
}

if (length(recomputed) > 0) {
  new_stats <- bind_rows(recomputed)
  recomputed_ids <- unique(new_stats$location_id)

  # Replace the affected locations' rows, keep everyone else untouched.
  combined_stats <- bind_rows(
    existing_stats |> filter(!location_id %in% recomputed_ids),
    new_stats
  ) |>
    # Keep the canonical column set/order so the parquet schema stays stable.
    select(any_of(CANON_STAT_COLS))

  write_parquet(combined_stats, stats_file)
  write_csv(combined_stats, file.path(OUTPUT_DIR, "historical_statistics.csv"), na = "")
  message(sprintf("Updated historical_statistics.parquet: %d rows (%d locations recomputed)",
                  nrow(combined_stats), length(recomputed_ids)))
} else {
  combined_stats <- existing_stats
  message("No affected location had observations inside the baseline window; statistics unchanged.")
}

# --- Stage gap-fill rows for the daily run ---------------------------------
# The downstream PostgreSQL loader ingests ONLY droughtData CSVs (keyed on each
# row's DataDate); it never reads the parquet files. So gap-filled observations
# only reach the database if they are emitted as daily-schema CSV rows. We render
# them here with the shared build_drought_csv_rows() and stage them as
# pending_backfill.parquet on HydroShare; the next daily run merges them into its
# droughtData CSV and then clears the staging file (see rezviz_data_generator.R).
message("\nStaging gap-fill rows for the next daily run...")

pending_file <- file.path(OUTPUT_DIR, "pending_backfill.parquet")
staging_rows <- build_drought_csv_rows(new_rows, locations, combined_stats, STATS_PERIOD)

# Merge with any pending rows not yet consumed by a daily run, so a second
# backfill before the daily run does not clobber un-emitted rows.
if (file.exists(pending_file)) file.remove(pending_file)
if (!DRY_RUN && hs_username != "" && hs_password != "") {
  got_prev <- download_parquet_from_hydroshare(
    "pending_backfill.parquet", pending_file, HYDROSHARE_RESOURCE_ID, hs_username, hs_password)
  if (got_prev && file.exists(pending_file)) {
    prev_rows <- tryCatch(read_parquet(pending_file), error = function(e) NULL)
    if (!is.null(prev_rows) && nrow(prev_rows) > 0) {
      staging_rows <- bind_rows(staging_rows, prev_rows) |>
        distinct(SiteName, DataDate, .keep_all = TRUE)
    }
  }
}

write_parquet(staging_rows, pending_file)
message(sprintf("Staged %d gap-fill row(s) for %d location(s) -> pending_backfill.parquet",
                nrow(staging_rows), n_distinct(staging_rows$SiteName)))

################################################################################
# UPLOAD TO HYDROSHARE
################################################################################

if (DRY_RUN) {
  message("\nDRY RUN: skipping HydroShare upload.")
} else if (hs_username == "" || hs_password == "") {
  message("\nWARNING: HydroShare credentials not set; skipping upload.")
  message("Set HYDROSHARE_USERNAME and HYDROSHARE_PASSWORD to enable upload.")
} else {
  message("\n=== Uploading updated files to HydroShare ===")
  # baseline + stats: internal generator state (percentiles for the daily join).
  # pending_backfill.parquet: gap-fill rows the next daily run merges into its
  # droughtData CSV so they reach the database.
  for (f in c(baseline_file, stats_file, pending_file)) {
    if (file.exists(f)) {
      message(sprintf("Uploading %s (%.1f MB)...", basename(f), file.size(f) / 1e6))
      tryCatch(
        upload_to_hydroshare(f, HYDROSHARE_RESOURCE_ID, hs_username, hs_password),
        error = function(e) message(sprintf("ERROR uploading %s: %s", basename(f), e$message))
      )
    }
  }
  message("Upload complete - the next daily run will merge the staged gap-fill rows")
  message("into its droughtData CSV, and the loader will ingest them into the database.")
}

################################################################################
# SUMMARY
################################################################################

message("\n=== Backfill complete ===")
message(sprintf("Locations swept:        %d", nrow(locations)))
message(sprintf("Locations with no data: %d", n_failed))
message(sprintf("Locations updated:      %d", length(affected_ids)))
message(sprintf("Observations added:     %d", nrow(new_rows)))
