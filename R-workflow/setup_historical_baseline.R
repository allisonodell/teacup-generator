# setup_historical_baseline.R
#
# ONE-TIME SETUP: Generate historical baseline data and statistics for
# reservoirs from multiple data sources (1990-10-01 to 2020-09-30).
#
# Run this ONCE before starting daily operations to create cached historical
# data and pre-computed statistics.
#
# Author: Kyle Onda, Internet of Water
# Created: 2026-01-28
################################################################################

library(httr2)
library(dplyr)
library(readr)
library(lubridate)
library(arrow)
library(purrr)
library(tidyr)
library(stringr)
library(jsonlite)

################################################################################
# CONFIGURATION
################################################################################

# Historical date range (30 water years)
START_DATE <- "1990-10-01"
END_DATE <- "2020-09-30"

# API base URL for RISE data via WWDH
WWDH_API_BASE <- "https://api.wwdh.internetofwater.app"

# Output paths
OUTPUT_DIR <- "output"
dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

################################################################################
# USACE CDA TIMESERIES LOOKUP
#
# Maps geojson Identifier → USACE CDA API parameters (provider, ts_name).
# Same lookup table as the daily script.
################################################################################

usace_lookup <- list(
  "305"         = list(provider = "spa",
                       ts_name  = "Cochiti.Stor.Inst.15Minutes.0.DCP-rev"),
  "abiquiu"     = list(provider = "spa",
                       ts_name  = "Abiquiu.Stor.Inst.15Minutes.0.DCP-rev"),
  "Santa Rosa"  = list(provider = "spa",
                       ts_name  = "Santa Rosa.Stor.Inst.15Minutes.0.DCP-rev"),
  "gcl"         = list(provider = "nwdp",
                       ts_name  = "GCL.Stor.Inst.1Hour.0.CBT-REV"),
  "FTPK"        = list(provider = "nwdm",
                       ts_name  = "FTPK.Stor.Inst.~1Day.0.Best-MRBWM"),
  "luc"         = list(provider = "nww",
                       ts_name  = "LUC.Stor-Total.Inst.0.0.USBR-COMPUTED-REV")
)

# Note: USGS and CDEC locations use their geojson Identifier directly as the
# site/station code. No lookup table needed — the Identifier IS the USGS site
# number or CDEC station code.

################################################################################
# LOAD LOCATION METADATA
################################################################################

locations_raw <- read_csv("config/locations.csv", show_col_types = FALSE)

# Filter to locations that are NOT "Do Not Include" and parse source info
# The "Post-Review Decision" column contains values like:
#   - "Include"
#   - "Include if possible"
#   - "Include (Omit % Avg if needed)"
#   - "Do Not Include"
locations <- locations_raw |>
  filter(`Post-Review Decision` != "Do Not Include") |>
  transmute(
    name = Name,
    decision = `Post-Review Decision`,
    source = `Source for Storage Data`,
    # For RISE locations, this is the RISE Location ID used with the WWDH API
    rise_location_id = `RISE Location ID`,
    capacity = as.numeric(str_remove_all(`Total Capacity`, ",")),
    label_map = `Preferred Label for Map and Table`,
    label_popup = `Preferred Label for PopUp and Modal`
  ) |>
  # Determine source type from the "Source for Storage Data" column
  mutate(
    source_type = case_when(
      source == "RISE" ~ "RISE",
      str_detect(source, "RISE \\(Pending\\)") ~ "RISE_PENDING",
      str_detect(source, "USACE") ~ "USACE",
      str_detect(source, "USGS") ~ "USGS",
      str_detect(source, "CDEC|cdec") ~ "CDEC",
      str_detect(source, "TROA") ~ "TROA",
      TRUE ~ "OTHER"
    )
  )

message(sprintf("Loaded %d locations (excluding 'Do Not Include')", nrow(locations)))

# Attach the RISE numeric parameter id (e.g. "3" for storage); the WWDH endpoint
# now requires &parameter-name=<id>. Fall back to NA if the CSV pre-dates the
# column, in which case RISE locations will be skipped.
if ("RISE Parameter ID for Storage Data" %in% names(locations_raw)) {
  locations <- locations |>
    left_join(
      locations_raw |>
        transmute(name = Name,
                  rise_param_id = `RISE Parameter ID for Storage Data`),
      by = "name"
    )
} else {
  locations$rise_param_id <- NA_character_
}

# Also load the geojson to get Identifier values and authoritative source
# classification for non-RISE sources. The geojson Identifier is the key used
# by the USACE/USGS/CDEC lookup tables and the daily script.
library(sf)
locations_geojson <- st_read("config/locations.geojson", quiet = TRUE) |>
  st_drop_geometry() |>
  transmute(
    name = Name,
    geojson_id = Identifier,
    geojson_source = `Source.for.Storage.Data`
  ) |>
  # Classify source type from the geojson source field (authoritative)
  mutate(
    geojson_source_type = case_when(
      str_detect(tolower(geojson_source), "^rise")  ~ "RISE",
      str_detect(tolower(geojson_source), "usace")  ~ "USACE",
      str_detect(tolower(geojson_source), "usgs")   ~ "USGS",
      str_detect(tolower(geojson_source), "cdec")   ~ "CDEC",
      TRUE ~ "OTHER"
    )
  )

# Split RISE locations (keyed by RISE Location ID from locations.csv).
# The WWDH endpoint now requires &parameter-name=<id>; drop locations whose
# parameter id is unknown ("TBD") or empty — they can't be fetched.
rise_locations <- locations |>
  filter(source_type == "RISE", !is.na(rise_location_id), rise_location_id != "--")

rise_skipped <- rise_locations |>
  filter(is.na(rise_param_id) | trimws(rise_param_id) == "" |
         toupper(trimws(rise_param_id)) == "TBD")
if (nrow(rise_skipped) > 0) {
  message(sprintf("Skipping %d RISE location(s) with unknown parameter id (TBD/empty):",
                  nrow(rise_skipped)))
  for (j in seq_len(nrow(rise_skipped))) {
    message(sprintf("  - %s (ID: %s)", rise_skipped$name[j], rise_skipped$rise_location_id[j]))
  }
}
rise_locations <- rise_locations |>
  filter(!is.na(rise_param_id) & trimws(rise_param_id) != "" &
         toupper(trimws(rise_param_id)) != "TBD")

# For non-RISE sources, use the geojson-based classification (more accurate
# than the CSV, e.g. Tahoe is "TROA? USGS?" in CSV but "CDEC" in geojson).
non_rise_geojson <- locations_geojson |>
  filter(geojson_source_type %in% c("USACE", "USGS", "CDEC")) |>
  # Join with locations.csv to get capacity and other metadata
  left_join(
    locations |> select(name, decision, capacity),
    by = "name"
  ) |>
  # Only include locations not marked "Do Not Include" (already filtered in locations)
  filter(!is.na(decision))

usace_locations <- non_rise_geojson |> filter(geojson_source_type == "USACE")
usgs_locations  <- non_rise_geojson |> filter(geojson_source_type == "USGS")
cdec_locations  <- non_rise_geojson |> filter(geojson_source_type == "CDEC")

message(sprintf("  - RISE: %d locations", nrow(rise_locations)))
message(sprintf("  - USACE: %d locations", nrow(usace_locations)))
message(sprintf("  - USGS: %d locations", nrow(usgs_locations)))
message(sprintf("  - CDEC: %d locations", nrow(cdec_locations)))

################################################################################
# DATA FETCHING FUNCTIONS BY SOURCE
################################################################################

#' Fetch historical storage data from RISE via WWDH API
#'
#' @param location_id RISE location ID (numeric or character)
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return tibble with columns: date, value, unit (storage in acre-feet typically)
#'
#' API URL pattern:
#'   https://api.wwdh.internetofwater.app/collections/rise-edr/locations/{location_id}
#'     ?parameter-name=Storage&limit=9999&datetime={start}/{end}&f=csv
#'
fetch_rise_historical <- function(location_id, rise_param_id,
                                  start_date = START_DATE, end_date = END_DATE,
                                  max_retries = 5) {

  empty_result <- tibble(date = Date(), value = numeric(), unit = character())

  # Skip if no parameter id (caller should already have filtered, but guard).
  pid <- if (is.na(rise_param_id)) "" else trimws(as.character(rise_param_id))
  if (pid == "" || toupper(pid) == "TBD") {
    message(sprintf("  Skipping RISE location %s: no parameter id", location_id))
    return(empty_result)
  }

  # Fetch in 5-year chunks. A single full-range request times out on the WWDH
  # endpoint for long-record reservoirs. The endpoint also requires
  # &parameter-name=<id> (HTTP 500 without it) and now returns CoverageJSON
  # only (f=csv is broken as of May 2026), so we parse the JSON response.
  api_end_date <- as.Date(end_date) + 1
  chunk_starts <- seq(as.Date(start_date), as.Date(end_date), by = "5 years")

  message(sprintf("  Fetching RISE location %s (param=%s, %d chunks)...",
                  location_id, pid, length(chunk_starts)))

  parse_coverage_response <- function(body) {
    coverages <- body$coverages
    if (is.null(coverages) || length(coverages) == 0) return(NULL)

    unit_val <- tryCatch({
      u <- body$parameters[[pid]]$unit$symbol
      if (is.null(u)) "af" else u
    }, error = function(e) "af")
    if (tolower(unit_val) == "ac-ft") unit_val <- "af"

    rows <- list()
    for (cov in coverages) {
      if (!is.null(cov$isModeled) && isTRUE(cov$isModeled)) next
      t_vals <- cov$domain$axes$t$values
      ranges <- cov$ranges
      if (is.null(t_vals) || length(t_vals) == 0 || is.null(ranges) || length(ranges) == 0) next
      if (!pid %in% names(ranges)) next
      raw_values <- ranges[[pid]]$values
      if (is.null(raw_values) || length(raw_values) == 0) next
      for (j in seq_along(raw_values)) {
        if (!is.null(raw_values[[j]])) {
          rows[[length(rows) + 1]] <- list(
            date  = as.Date(substr(t_vals[[j]], 1, 10)),
            value = as.numeric(raw_values[[j]]),
            unit  = unit_val
          )
        }
      }
    }
    if (length(rows) == 0) return(NULL)
    bind_rows(rows)
  }

  chunk_results <- list()

  for (ci in seq_along(chunk_starts)) {
    cs <- chunk_starts[ci]
    ce <- min(cs + years(5), api_end_date)
    url <- paste0(
      WWDH_API_BASE,
      "/collections/rise-edr/locations/", location_id,
      "?parameter-name=", pid,
      "&limit=50000",
      "&datetime=", cs, "/", ce,
      "&f=json"
    )

    chunk_data <- NULL
    for (attempt in 1:max_retries) {
      attempt_result <- tryCatch({
        response <- request(url) |>
          req_timeout(300) |>
          req_retry(max_tries = 5, backoff = ~ 5,
                    is_transient = ~ resp_status(.x) %in% c(408, 425, 429, 500, 502, 503, 504)) |>
          req_perform()

        status <- resp_status(response)
        if (status != 200) {
          warning(sprintf("HTTP %d for location %s chunk %s..%s", status, location_id, cs, ce))
          return(empty_result)
        }

        body <- resp_body_json(response)
        parsed <- parse_coverage_response(body)
        if (is.null(parsed)) empty_result else parsed |> filter(!is.na(value))

      }, error = function(e) {
        if (attempt < max_retries) {
          wait_time <- 10 * attempt
          message(sprintf("    Chunk %s..%s attempt %d failed: %s. Waiting %ds...",
                          cs, ce, attempt, e$message, wait_time))
          Sys.sleep(wait_time)
          NULL
        } else {
          warning(sprintf("Failed chunk %s..%s for location %s after %d attempts: %s",
                          cs, ce, location_id, max_retries, e$message))
          empty_result
        }
      })

      if (!is.null(attempt_result)) {
        chunk_data <- attempt_result
        break
      }
    }

    if (!is.null(chunk_data) && nrow(chunk_data) > 0) {
      chunk_results[[length(chunk_results) + 1]] <- chunk_data
    }

    Sys.sleep(0.5)
  }

  if (length(chunk_results) == 0) {
    return(empty_result)
  }

  combined <- bind_rows(chunk_results)
  rows_before <- nrow(combined)
  combined <- combined |>
    arrange(date) |>
    distinct(date, .keep_all = TRUE)
  rows_after <- nrow(combined)

  if (rows_before != rows_after) {
    message(sprintf("    Removed %d duplicate days (kept first instance)", rows_before - rows_after))
  }

  combined
}

#' Fetch historical storage data from USACE CDA timeseries API
#'
#' The CDA API returns sub-daily data (15-min or hourly depending on the
#' time series). We aggregate to daily values by taking the last observation
#' per date. The API can handle large date ranges, but we chunk by year to
#' be safe with response sizes.
#'
#' CSV format: ## comment lines (including ##unit:), then datetime,value rows.
#'
#' @param location_id Geojson Identifier (used to look up USACE provider/ts_name)
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return tibble with columns: date, value, unit
fetch_usace_historical <- function(location_id, start_date = START_DATE, end_date = END_DATE) {
  lookup <- usace_lookup[[as.character(location_id)]]
  if (is.null(lookup)) {
    warning(sprintf("No USACE lookup entry for identifier '%s'", location_id))
    return(tibble(date = Date(), value = numeric(), unit = character()))
  }

  message(sprintf("  Fetching USACE %s (provider: %s)...", lookup$ts_name, lookup$provider))

  all_data <- list()
  unit_val <- "ac-ft"
  sd <- as.Date(start_date)
  ed <- as.Date(end_date)

  # Chunk by year to manage response sizes (sub-daily data is large)
  chunk_start <- sd
  while (chunk_start <= ed) {
    chunk_end <- min(chunk_start + 365, ed)

    begin_str <- paste0(format(chunk_start, "%Y-%m-%dT00:00:00"), ".000Z")
    end_str   <- paste0(format(chunk_end + 1, "%Y-%m-%dT00:00:00"), ".000Z")

    url <- sprintf(
      "https://water.usace.army.mil/cda/reporting/providers/%s/timeseries?name=%s&begin=%s&end=%s&format=csv",
      lookup$provider,
      URLencode(lookup$ts_name, reserved = TRUE),
      begin_str, end_str
    )

    tryCatch({
      response <- request(url) |>
        req_timeout(300) |>
        req_retry(max_tries = 3, backoff = ~ 10) |>
        req_perform()

      body <- resp_body_string(response)

      # Strip \r from \r\n line endings
      body <- str_replace_all(body, "\r", "")
      lines <- str_split(body, "\n")[[1]]

      # Extract unit from ## comment lines
      unit_line <- lines[str_starts(lines, "##unit:")]
      if (length(unit_line) > 0) {
        unit_val <- trimws(str_remove(unit_line[1], "##unit:"))
      }

      # Parse data lines (skip ## comments and empty lines)
      data_lines <- lines[!str_starts(lines, "##") & nchar(trimws(lines)) > 0]

      if (length(data_lines) > 0) {
        chunk <- tibble(raw = data_lines) |>
          mutate(
            datetime = str_extract(raw, "^[^,]+"),
            value    = as.numeric(str_extract(raw, "[^,]+$")),
            date     = as.Date(str_sub(datetime, 1, 10))
          ) |>
          filter(!is.na(value), !is.na(date)) |>
          select(date, value)

        if (nrow(chunk) > 0) {
          # Aggregate sub-daily to daily: take the last value per date
          chunk <- chunk |>
            group_by(date) |>
            summarize(value = last(value), .groups = "drop")

          all_data[[length(all_data) + 1]] <- chunk
        }
      }
    }, error = function(e) {
      message(sprintf("    USACE CDA error for %s-%s: %s",
                      chunk_start, chunk_end, conditionMessage(e)))
    })

    chunk_start <- chunk_end + 1
    Sys.sleep(1)  # Rate limiting - USACE can be slow
  }

  if (length(all_data) == 0) {
    return(tibble(date = Date(), value = numeric(), unit = character()))
  }

  # Normalize unit
  if (tolower(unit_val) == "ac-ft") unit_val <- "af"

  result <- bind_rows(all_data) |>
    mutate(unit = unit_val) |>
    arrange(date) |>
    distinct(date, .keep_all = TRUE)

  return(result)
}

#' Fetch historical daily storage data from USGS Water Data OGC API
#'
#' Uses the new OGC API (replaces legacy NWIS web services).
#' Parameter code 00054 = reservoir storage (acre-feet).
#' The API has a limit of 10,000 items per request, so we paginate by year
#' to safely cover the full 30-year range.
#' The location_id IS the USGS site number (from geojson Identifier).
#'
#' @param location_id Geojson Identifier which IS the USGS site number
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return tibble with columns: date, value, unit
fetch_usgs_historical <- function(location_id, start_date = START_DATE, end_date = END_DATE) {
  # location_id is the USGS site number directly from geojson Identifier
  site_no <- as.character(location_id)

  message(sprintf("  Fetching USGS site %s...", site_no))

  all_data <- list()
  sd <- as.Date(start_date)
  ed <- as.Date(end_date)

  # Paginate by year to stay well under the 10,000 item limit
  year_start <- sd
  while (year_start <= ed) {
    year_end <- min(year_start + 365, ed)

    url <- sprintf(
      "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items?f=json&monitoring_location_id=USGS-%s&parameter_code=00054&time=%s/%s&limit=500",
      site_no, year_start, year_end
    )

    tryCatch({
      response <- request(url) |>
        req_timeout(120) |>
        req_retry(max_tries = 3, backoff = ~ 5) |>
        req_perform()

      body <- resp_body_string(response)
      data <- fromJSON(body, simplifyVector = FALSE)
      features <- data$features

      if (length(features) > 0) {
        chunk <- tibble(
          date  = as.Date(sapply(features, function(f) f$properties$time)),
          value = as.numeric(sapply(features, function(f) f$properties$value)),
          unit  = sapply(features, function(f) f$properties$unit_of_measure)
        ) |>
          filter(!is.na(value))

        if (nrow(chunk) > 0) {
          # Normalize unit
          chunk$unit <- tolower(chunk$unit)
          chunk$unit[chunk$unit == "acre-ft"] <- "af"
          all_data[[length(all_data) + 1]] <- chunk
        }
      }
    }, error = function(e) {
      message(sprintf("    USGS OGC API error for %s-%s: %s",
                      year_start, year_end, conditionMessage(e)))
    })

    year_start <- year_end + 1
    Sys.sleep(0.5)  # Rate limiting
  }

  if (length(all_data) == 0) {
    return(tibble(date = Date(), value = numeric(), unit = character()))
  }

  result <- bind_rows(all_data) |>
    arrange(date) |>
    distinct(date, .keep_all = TRUE)

  return(result)
}

#' Fetch historical daily storage data from CDEC
#'
#' CDEC CSVDataServlet with sensor 15 (reservoir storage), daily duration.
#' Note: CDEC requires a User-Agent header to return data.
#' Data availability varies — Tahoe storage only goes back to ~2023.
#' The location_id IS the CDEC station code (from geojson Identifier).
#'
#' @param location_id Geojson Identifier which IS the CDEC station code
#' @param start_date Start date (YYYY-MM-DD)
#' @param end_date End date (YYYY-MM-DD)
#' @return tibble with columns: date, value, unit
fetch_cdec_historical <- function(location_id, start_date = START_DATE, end_date = END_DATE) {
  # location_id is the CDEC station code directly from geojson Identifier
  station <- as.character(location_id)

  message(sprintf("  Fetching CDEC station %s...", station))

  url <- sprintf(
    "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=%s&SensorNums=15&dur_code=D&Start=%s&End=%s",
    station, start_date, end_date
  )

  tryCatch({
    response <- request(url) |>
      req_headers(`User-Agent` = "Mozilla/5.0 (R httr2)") |>
      req_timeout(120) |>
      req_retry(max_tries = 3, backoff = ~ 5) |>
      req_perform()

    body <- resp_body_string(response)
    data <- read_csv(I(body), show_col_types = FALSE,
                     col_types = cols(`DATE TIME` = col_character(),
                                     `OBS DATE` = col_character(),
                                     .default = col_guess()))

    if (nrow(data) == 0 || !"VALUE" %in% names(data)) {
      message(sprintf("    No data returned for CDEC station %s", station))
      return(tibble(date = Date(), value = numeric(), unit = character()))
    }

    # CDEC UNITS column
    unit_val <- if ("UNITS" %in% names(data)) tolower(data$UNITS[1]) else "af"

    result <- data |>
      mutate(
        date  = as.Date(str_sub(`DATE TIME`, 1, 8), format = "%Y%m%d"),
        value = as.numeric(VALUE),
        unit  = unit_val
      ) |>
      filter(!is.na(value)) |>
      select(date, value, unit) |>
      arrange(date) |>
      distinct(date, .keep_all = TRUE)

    return(result)
  }, error = function(e) {
    message(sprintf("    CDEC error: %s", conditionMessage(e)))
    return(tibble(date = Date(), value = numeric(), unit = character()))
  })
}

################################################################################
# STATISTICS CALCULATION
################################################################################

#' Calculate day-of-year statistics from historical time series
#'
#' IMPORTANT: Statistics are computed separately for EACH calendar day (month-day).
#' This groups all historical values by their month-day combination, then computes
#' stats within each group. For example:
#'   - January 1:  Uses all Jan 1 values from 1991, 1992, ..., 2020 (~30 values)
#'   - January 2:  Uses all Jan 2 values from 1991, 1992, ..., 2020 (~30 values)
#'   - ...
#'   - December 31: Uses all Dec 31 values from 1990, 1991, ..., 2019 (~30 values)
#'   - February 29: Uses all Feb 29 values from leap years only (~7-8 values)
#'
#' This produces 366 rows of statistics per location (one for each possible
#' calendar day including Feb 29), enabling comparison of a current day's value
#' against the historical distribution for THAT SPECIFIC day of the year.
#'
#' @param data tibble with columns: date, value, unit
#' @param location_id Identifier for the location
#' @return tibble with columns: location_id, month, day, min, max, p10, p25, p50, p75, p90, mean, count, unit
#'
calculate_daily_stats <- function(data, location_id) {

  if (nrow(data) == 0) {
    return(tibble())
  }

  # Preserve the unit from the source data
  # (assumes all values for a location have the same unit)
  data_unit <- unique(data$unit)[1]

  data |>
    mutate(
      month = month(date),
      day = day(date)
    ) |>
    group_by(month, day) |>
    summarise(
      min = min(value, na.rm = TRUE),
      max = max(value, na.rm = TRUE),
      p10 = quantile(value, 0.10, na.rm = TRUE),
      p25 = quantile(value, 0.25, na.rm = TRUE),
      p50 = quantile(value, 0.50, na.rm = TRUE),  # median
      p75 = quantile(value, 0.75, na.rm = TRUE),
      p90 = quantile(value, 0.90, na.rm = TRUE),
      mean = mean(value, na.rm = TRUE),
      count = sum(!is.na(value)),
      .groups = "drop"
    ) |>
    mutate(
      location_id = location_id,
      unit = data_unit
    ) |>
    select(location_id, month, day, everything())
}

################################################################################
# MAIN PROCESSING LOOP
################################################################################

# Initialize result containers
all_historical_data <- list()
all_statistics <- list()
failed_locations <- character()

# Process RISE locations
message("\n=== Processing RISE locations ===\n")

for (i in seq_len(nrow(rise_locations))) {
  loc <- rise_locations[i, ]
  location_id <- loc$rise_location_id
  location_name <- loc$name

  message(sprintf("[%d/%d] Processing %s (ID: %s)",
                  i, nrow(rise_locations), location_name, location_id))

  # Fetch historical data
  hist_data <- fetch_rise_historical(location_id, loc$rise_param_id)

  if (nrow(hist_data) == 0) {
    message(sprintf("  WARNING: No data returned for %s", location_name))
    failed_locations <- c(failed_locations, location_name)
    next
  }

  message(sprintf("  Retrieved %d observations", nrow(hist_data)))

  # Calculate day-of-year statistics
  stats <- calculate_daily_stats(hist_data, location_id)
  message(sprintf("  Computed statistics for %d day-of-year groups", nrow(stats)))

  # Store results — key by RISE location_id (same as used in daily script)
  all_historical_data[[location_id]] <- hist_data |> mutate(location_id = location_id)
  all_statistics[[location_id]] <- stats

  # Be respectful of API rate limits
  Sys.sleep(0.5)
}

# Process USACE locations
message("\n=== Processing USACE locations ===\n")

for (i in seq_len(nrow(usace_locations))) {
  loc <- usace_locations[i, ]
  location_id <- loc$geojson_id
  location_name <- loc$name

  message(sprintf("[%d/%d] Processing %s (ID: %s)",
                  i, nrow(usace_locations), location_name, location_id))

  # Fetch historical data via CDA timeseries API
  hist_data <- fetch_usace_historical(location_id)

  if (nrow(hist_data) == 0) {
    message(sprintf("  WARNING: No data returned for %s", location_name))
    failed_locations <- c(failed_locations, location_name)
    next
  }

  message(sprintf("  Retrieved %d daily observations (%s to %s)",
                  nrow(hist_data), min(hist_data$date), max(hist_data$date)))

  # Calculate day-of-year statistics
  stats <- calculate_daily_stats(hist_data, location_id)
  message(sprintf("  Computed statistics for %d day-of-year groups", nrow(stats)))

  # Store results — key by geojson Identifier (same as used in daily script)
  all_historical_data[[location_id]] <- hist_data |> mutate(location_id = location_id)
  all_statistics[[location_id]] <- stats
}

# Process USGS locations
message("\n=== Processing USGS locations ===\n")

for (i in seq_len(nrow(usgs_locations))) {
  loc <- usgs_locations[i, ]
  location_id <- loc$geojson_id
  location_name <- loc$name

  message(sprintf("[%d/%d] Processing %s (ID: %s)",
                  i, nrow(usgs_locations), location_name, location_id))

  # Fetch historical data via OGC API
  hist_data <- fetch_usgs_historical(location_id)

  if (nrow(hist_data) == 0) {
    message(sprintf("  WARNING: No data returned for %s", location_name))
    failed_locations <- c(failed_locations, location_name)
    next
  }

  message(sprintf("  Retrieved %d daily observations (%s to %s)",
                  nrow(hist_data), min(hist_data$date), max(hist_data$date)))

  # Calculate day-of-year statistics
  stats <- calculate_daily_stats(hist_data, location_id)
  message(sprintf("  Computed statistics for %d day-of-year groups", nrow(stats)))

  # Store results — key by geojson Identifier (same as used in daily script)
  all_historical_data[[location_id]] <- hist_data |> mutate(location_id = location_id)
  all_statistics[[location_id]] <- stats

  Sys.sleep(0.5)
}

# Process CDEC locations
message("\n=== Processing CDEC locations ===\n")

for (i in seq_len(nrow(cdec_locations))) {
  loc <- cdec_locations[i, ]
  location_id <- loc$geojson_id
  location_name <- loc$name

  message(sprintf("[%d/%d] Processing %s (ID: %s)",
                  i, nrow(cdec_locations), location_name, location_id))

  # Fetch historical data via CDEC servlet
  hist_data <- fetch_cdec_historical(location_id)

  if (nrow(hist_data) == 0) {
    message(sprintf("  WARNING: No data returned for %s", location_name))
    failed_locations <- c(failed_locations, location_name)
    next
  }

  message(sprintf("  Retrieved %d daily observations (%s to %s)",
                  nrow(hist_data), min(hist_data$date), max(hist_data$date)))

  # Calculate day-of-year statistics
  stats <- calculate_daily_stats(hist_data, location_id)
  message(sprintf("  Computed statistics for %d day-of-year groups", nrow(stats)))

  # Store results — key by geojson Identifier (same as used in daily script)
  all_historical_data[[location_id]] <- hist_data |> mutate(location_id = location_id)
  all_statistics[[location_id]] <- stats
}

################################################################################
# COMBINE AND SAVE OUTPUTS
################################################################################

message("\n=== Saving outputs ===\n")

# Combine all historical data
historical_baseline <- bind_rows(all_historical_data)
message(sprintf("Total historical observations: %d", nrow(historical_baseline)))

# Combine all statistics
historical_statistics <- bind_rows(all_statistics)
message(sprintf("Total statistics rows: %d (should be ~366 per location)", nrow(historical_statistics)))

# Save as Parquet (efficient binary format)
write_parquet(historical_baseline, file.path(OUTPUT_DIR, "historical_baseline.parquet"))
write_parquet(historical_statistics, file.path(OUTPUT_DIR, "historical_statistics.parquet"))

# Save statistics as CSV for human readability
write_csv(historical_statistics, file.path(OUTPUT_DIR, "historical_statistics.csv"))

# Save list of failed locations
if (length(failed_locations) > 0) {
  writeLines(failed_locations, file.path(OUTPUT_DIR, "failed_locations.txt"))
  message(sprintf("\nWARNING: %d locations failed to process. See failed_locations.txt",
                  length(failed_locations)))
}

message("\n=== Done ===")
message(sprintf("Outputs saved to: %s/", OUTPUT_DIR))
message("  - historical_baseline.parquet (full time series)")
message("  - historical_statistics.parquet (day-of-year stats for daily script)")
message("  - historical_statistics.csv (human-readable copy)")
