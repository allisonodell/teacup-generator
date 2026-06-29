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
  library(httr2)
  library(stringr)
  library(lubridate)
  library(jsonlite)
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

################################################################################
# SOURCE TYPE CLASSIFICATION
################################################################################

#' Classify a location's data source type from the Source field in geojson
#'
#' @param source_str The "Source for Storage Data" field value
#' @return One of: "rise", "usace_cda", "usgs", "cdec", "unknown"
classify_source <- function(source_str) {
  if (is.na(source_str) || source_str == "") return("unknown")
  s <- tolower(source_str)
  if (str_detect(s, "^rise"))                                  return("rise")
  if (str_detect(s, "usace") || str_detect(s, "water\\.usace")) return("usace_cda")
  if (str_detect(s, "^usgs") || str_detect(s, "waterdata\\.usgs")) return("usgs")
  if (str_detect(s, "cdec\\.water\\.ca\\.gov"))                return("cdec")
  return("unknown")
}

################################################################################
# DAY-OF-YEAR STATISTICS
################################################################################

#' Calculate day-of-year statistics from a historical time series.
#'
#' Groups all values by calendar day (month-day) and computes the distribution
#' for THAT day of the year across all years in the input. Produces up to 366
#' rows per location (including Feb 29). The output schema matches
#' historical_statistics.parquet:
#'   location_id, month, day, min, max, p10, p25, p50, p75, p90, mean, count, unit
#'
#' @param data tibble with columns: date, value, unit
#' @param location_id Identifier for the location
#' @return tibble of day-of-year statistics (empty tibble if no data)
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
# DROUGHT CSV ROW BUILDER (shared by daily new-location backfill and backfill_history.R)
################################################################################

#' Render historical observations into the daily droughtData CSV schema.
#'
#' Produces the exact 27-column schema the downstream PostgreSQL loader ingests
#' (SiteName, DataDate as %m/%d/%Y, DataValue, DataDateAvg/P10/P90, ...), with
#' `Comment = "backfill"`. Used both by the daily script when it backfills a
#' newly-detected location and by backfill_history.R when it stages gap-filled
#' observations for the next daily run — keeping the two row layouts identical
#' so they cannot drift out of sync with the loader's expectations.
#'
#' @param obs tibble(location_id, date, value, unit) of observations to render.
#' @param locations location metadata (must include location_id, label_popup,
#'   latitude, longitude, state, doi_region, huc6, capacity, active_capacity).
#' @param stats_lookup day-of-year statistics keyed by (location_id, month, day)
#'   with columns min, max, p10, p25, p50, p75, p90, mean. Rows with no matching
#'   stats get NA percentile columns (the loader still ingests the raw value).
#' @param stats_period the StatsPeriod label string (e.g. "10/1/1990 - 9/30/2020").
#' @return tibble in the daily droughtData CSV schema.
build_drought_csv_rows <- function(obs, locations, stats_lookup, stats_period) {
  # Be robust to an empty / column-less stats table (e.g. no statistics computed
  # yet): fall back to a zero-row lookup so every observation still renders, just
  # with NA percentile columns (the loader ingests the raw value regardless).
  if (!all(c("location_id", "month", "day") %in% names(stats_lookup))) {
    stats_lookup <- tibble(
      location_id = character(), month = integer(), day = integer(),
      min = double(), max = double(), p10 = double(), p25 = double(),
      p50 = double(), p75 = double(), p90 = double(), mean = double()
    )
  }

  obs |>
    left_join(locations, by = "location_id") |>
    mutate(
      data_month = month(date),
      data_day   = day(date)
    ) |>
    left_join(
      stats_lookup |> select(location_id, month, day, min, max, p10, p25, p50, p75, p90, mean),
      by = c("location_id", "data_month" = "month", "data_day" = "day")
    ) |>
    mutate(
      pct_median  = value / p50,
      pct_average = value / mean,
      pct_full    = value / capacity
    ) |>
    transmute(
      SiteName       = label_popup,
      Lat            = latitude,
      Lon            = longitude,
      State          = state,
      DoiRegion      = doi_region,
      Huc6           = huc6,
      DataUnits      = unit,
      DataValue      = value,
      DataDate       = format(date, "%m/%d/%Y"),
      DateQueried    = format(Sys.Date(), "%m/%d/%Y"),
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
      StatsPeriod    = stats_period,
      MaxCapacity    = capacity,
      ActiveCapacity = active_capacity,
      PctFull        = pct_full,
      TeacupUrl      = NA_character_,
      DataUrl        = NA_character_,
      Comment        = "backfill"
    ) |>
    # Drop any row with no usable value — the loader skips empty DataValue anyway,
    # and this keeps both callers (daily new-location backfill and backfill_history.R)
    # symmetric. In practice observations are pre-filtered to non-NA upstream.
    filter(!is.na(DataValue))
}

################################################################################
# FULL-HISTORY FETCH (shared by daily auto-backfill and backfill_history.R)
################################################################################

#' Fetch the entire available daily record for a single location.
#'
#' Pulls every available daily observation from `baseline_start` through
#' `backfill_end` for one location, dispatching to the correct source-specific
#' API based on the location's Source field. This is the authoritative
#' full-history fetcher used both by the daily script (when it detects a new
#' location) and by the standalone backfill routine, so the two stay in sync on
#' identifiers, parsing, and unit handling.
#'
#' @param loc A one-row data frame / list with at least: location_id, source,
#'   data_type, rise_param_id, usgs_param_code (as produced when loading
#'   locations.geojson in the daily script).
#' @param baseline_start Date — earliest date to request.
#' @param backfill_end Date — latest date to request (typically Sys.Date()).
#' @param wwdh_api_base Base URL for the WWDH/RISE EDR API.
#' @return tibble(location_id, date, value, unit) of observations, or NULL if
#'   nothing could be retrieved.
fetch_full_history <- function(loc, baseline_start, backfill_end,
                               wwdh_api_base = WWDH_API_BASE) {
  loc_id        <- loc$location_id
  data_type_str <- if (!is.na(loc$data_type)) loc$data_type else "Storage"
  src_type      <- classify_source(loc$source)

  # Internal aliases so the source-specific blocks below read identically to
  # the surrounding pipeline code.
  BASELINE_START <- baseline_start
  BACKFILL_END   <- backfill_end
  WWDH_API_BASE  <- wwdh_api_base

  hist_data <- NULL

  if (src_type == "rise") {
    # RISE: fetch full range via WWDH API (CoverageJSON) in 5-year chunks.
    # A single 1990->today request times out on the WWDH endpoint for
    # long-record reservoirs, so we paginate and concatenate responses.
    # The endpoint also now requires parameter-name=<id> (the API returns
    # HTTP 500 without it); the id comes from the RISE Parameter ID column.
    pid <- if (is.na(loc$rise_param_id)) "" else trimws(as.character(loc$rise_param_id))
    if (pid == "" || toupper(pid) == "TBD") {
      message(sprintf("    Skipping RISE backfill: no parameter id for location %s", loc_id))
      return(NULL)
    }

    chunk_starts <- seq(BASELINE_START, BACKFILL_END, by = "5 years")
    all_rows <- list()
    raw_unit <- "af"
    unit_found <- FALSE

    for (ci in seq_along(chunk_starts)) {
      cs <- chunk_starts[ci]
      ce <- min(cs + years(5), BACKFILL_END + 1)
      url <- paste0(
        WWDH_API_BASE,
        "/collections/rise-edr/locations/", loc_id,
        "?parameter-name=", pid,
        "&limit=50000",
        "&datetime=", cs, "/", ce,
        "&f=json"
      )

      tryCatch({
        response <- request(url) |>
          req_timeout(300) |>
          req_retry(max_tries = 5, backoff = ~ 5,
                    is_transient = ~ resp_status(.x) %in% c(408, 425, 429, 500, 502, 503, 504)) |>
          req_perform()

        if (resp_status(response) == 200) {
          body <- resp_body_json(response)
          coverages <- body$coverages
          if (!is.null(coverages) && length(coverages) > 0) {
            # Find storage parameter key from response
            storage_key <- NULL
            bp <- body$parameters
            if (!is.null(bp)) {
              for (pk in names(bp)) {
                label <- tryCatch(bp[[pk]]$observedProperty$label$en, error = function(e) "")
                if (!is.null(label) && grepl("Storage", label, ignore.case = TRUE) &&
                    !grepl("Change In Storage|Bank Storage", label, ignore.case = TRUE)) {
                  storage_key <- pk
                  break
                }
              }
            }
            if (is.null(storage_key)) storage_key <- "3"

            if (!unit_found) {
              raw_unit <- tryCatch({
                u <- body$parameters[[storage_key]]$unit$symbol
                if (is.null(u)) "af" else u
              }, error = function(e) "af")
              unit_found <- TRUE
            }

            for (cov in coverages) {
              if (!is.null(cov$isModeled) && isTRUE(cov$isModeled)) next
              t_vals <- cov$domain$axes$t$values
              ranges <- cov$ranges
              if (is.null(t_vals) || length(t_vals) == 0 || is.null(ranges) || length(ranges) == 0) next
              if (!storage_key %in% names(ranges)) next
              raw_values <- ranges[[storage_key]]$values
              if (is.null(raw_values) || length(raw_values) == 0) next
              for (j in seq_along(raw_values)) {
                if (!is.null(raw_values[[j]])) {
                  all_rows[[length(all_rows) + 1]] <- list(
                    date  = as.Date(substr(t_vals[[j]], 1, 10)),
                    value = as.numeric(raw_values[[j]])
                  )
                }
              }
            }
          }
        }
      }, error = function(e) {
        message(sprintf("    Error fetching RISE chunk %s..%s: %s", cs, ce, conditionMessage(e)))
      })

      Sys.sleep(0.5)
    }

    if (length(all_rows) > 0) {
      hist_data <- bind_rows(all_rows) |>
        mutate(location_id = loc_id, unit = raw_unit) |>
        filter(!is.na(value)) |>
        distinct(date, .keep_all = TRUE)
    }

  } else if (src_type == "usace_cda") {
    # USACE: parse provider/ts_name and fetch
    id_str <- as.character(loc_id)
    slash_pos <- str_locate(id_str, "/")[1, "start"]

    if (!is.na(slash_pos)) {
      provider <- str_sub(id_str, 1, slash_pos - 1)
      ts_name  <- str_sub(id_str, slash_pos + 1)

      begin_str <- paste0(format(BASELINE_START, "%Y-%m-%dT00:00:00"), ".000Z")
      end_str   <- paste0(format(BACKFILL_END + 1, "%Y-%m-%dT00:00:00"), ".000Z")

      url <- sprintf(
        "https://water.usace.army.mil/cda/reporting/providers/%s/timeseries?name=%s&begin=%s&end=%s&format=csv",
        provider, URLencode(ts_name, reserved = TRUE), begin_str, end_str
      )

      tryCatch({
        response <- request(url) |>
          req_timeout(300) |>
          req_retry(max_tries = 3, backoff = ~ 10) |>
          req_perform()

        body <- resp_body_string(response)
        body <- str_replace_all(body, "\r", "")
        lines <- str_split(body, "\n")[[1]]

        unit_line <- lines[str_starts(lines, "##unit:")]
        unit_val <- if (length(unit_line) > 0) trimws(str_remove(unit_line[1], "##unit:")) else "ac-ft"
        if (tolower(unit_val) == "ac-ft") unit_val <- "af"

        data_lines <- lines[!str_starts(lines, "##") & nchar(trimws(lines)) > 0]

        if (length(data_lines) > 0) {
          hist_data <- tibble(raw = data_lines) |>
            mutate(
              datetime = str_extract(raw, "^[^,]+"),
              value = as.numeric(str_extract(raw, "[^,]+$")),
              date = as.Date(str_sub(datetime, 1, 10))
            ) |>
            filter(!is.na(value), !is.na(date)) |>
            group_by(date) |>
            summarize(value = last(value), .groups = "drop") |>
            mutate(location_id = loc_id, unit = unit_val) |>
            select(location_id, date, value, unit)
        }
      }, error = function(e) {
        message(sprintf("    Error fetching USACE historical: %s", conditionMessage(e)))
      })
    }

  } else if (src_type == "usgs") {
    # USGS: fetch via OGC API
    # Handle elevation vs storage data types
    site_no <- as.character(loc_id)

    # Honor per-location USGS parameter override (e.g. "00065" for Lake
    # Tahoe gage height). When set, we skip the elevation fallback chain
    # below and just use this exact parameter code.
    usgs_param_override <- loc$usgs_param_code
    has_usgs_override <- !is.na(usgs_param_override) &&
                         nzchar(trimws(as.character(usgs_param_override)))

    # Select parameter code based on data type
    if (has_usgs_override) {
      param_code <- trimws(as.character(usgs_param_override))
    } else if (tolower(data_type_str) == "elevation") {
      param_code <- "62614"  # Elevation (NGVD29)
    } else {
      param_code <- "00054"  # Storage (acre-feet)
    }

    url <- sprintf(
      "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items?f=json&monitoring_location_id=USGS-%s&parameter_code=%s&time=%s/%s&limit=50000",
      site_no, param_code, BASELINE_START, BACKFILL_END
    )

    tryCatch({
      response <- request(url) |>
        req_timeout(300) |>
        req_retry(max_tries = 3, backoff = ~ 10) |>
        req_perform()

      body <- resp_body_string(response)
      data <- jsonlite::fromJSON(body, simplifyVector = FALSE)

      features <- data$features

      # If elevation query returned nothing, try alternate elevation parameters
      # Priority: 72275 (USBR datum, e.g. Klamath Basin) > 62615 (NAVD88) > 62614 (NGVD29)
      # Skip the fallback chain when caller specified an explicit override.
      if (length(features) == 0 && tolower(data_type_str) == "elevation" && !has_usgs_override) {
        for (alt_param in c("72275", "62615")) {
          message(sprintf("    Trying alternate elevation parameter %s...", alt_param))
          url <- sprintf(
            "https://api.waterdata.usgs.gov/ogcapi/v0/collections/daily/items?f=json&monitoring_location_id=USGS-%s&parameter_code=%s&time=%s/%s&limit=50000",
            site_no, alt_param, BASELINE_START, BACKFILL_END
          )
          response <- request(url) |>
            req_timeout(300) |>
            req_retry(max_tries = 3, backoff = ~ 10) |>
            req_perform()
          body <- resp_body_string(response)
          data <- jsonlite::fromJSON(body, simplifyVector = FALSE)
          features <- data$features
          if (length(features) > 0) {
            message(sprintf("    Found %d records with parameter %s", length(features), alt_param))
            break
          }
        }
      }

      if (length(features) > 0) {
        hist_data <- tibble(
          location_id = loc_id,
          date = as.Date(sapply(features, function(f) f$properties$time)),
          value = as.numeric(sapply(features, function(f) f$properties$value)),
          unit = sapply(features, function(f) f$properties$unit_of_measure)
        ) |>
          filter(!is.na(value)) |>
          distinct(date, .keep_all = TRUE)

        # Convert elevation to storage if needed
        if (tolower(data_type_str) == "elevation") {
          message(sprintf("    Converting %d elevation readings to storage...", nrow(hist_data)))
          hist_data <- hist_data |>
            rowwise() |>
            mutate(
              storage = elevation_to_storage(location_id, value)
            ) |>
            ungroup() |>
            filter(!is.na(storage)) |>
            mutate(
              value = storage,
              unit = "af"
            ) |>
            select(-storage)
          message(sprintf("    Successfully converted %d readings", nrow(hist_data)))
        } else {
          hist_data <- hist_data |>
            mutate(unit = ifelse(tolower(unit) == "acre-ft", "af", tolower(unit)))
        }
      }
    }, error = function(e) {
      message(sprintf("    Error fetching USGS historical: %s", conditionMessage(e)))
    })

  } else if (src_type == "cdec") {
    # CDEC: fetch via CSV servlet
    station <- as.character(loc_id)
    url <- sprintf(
      "https://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=%s&SensorNums=15&dur_code=D&Start=%s&End=%s",
      station, BASELINE_START, BACKFILL_END
    )

    tryCatch({
      response <- request(url) |>
        req_timeout(300) |>
        req_retry(max_tries = 3, backoff = ~ 10) |>
        req_perform()

      body <- resp_body_string(response)
      data <- read_csv(I(body), show_col_types = FALSE,
                       col_types = cols(`DATE TIME` = col_character(),
                                        `OBS DATE` = col_character(),
                                        .default = col_guess()))

      if (nrow(data) > 0 && "VALUE" %in% names(data)) {
        unit_val <- if ("UNITS" %in% names(data)) tolower(data$UNITS[1]) else "af"

        hist_data <- data |>
          mutate(
            date = as.Date(str_sub(`DATE TIME`, 1, 8), format = "%Y%m%d"),
            value = as.numeric(VALUE)
          ) |>
          filter(!is.na(value)) |>
          transmute(location_id = loc_id, date, value, unit = unit_val) |>
          distinct(date, .keep_all = TRUE)
      }
    }, error = function(e) {
      message(sprintf("    Error fetching CDEC historical: %s", conditionMessage(e)))
    })
  }

  hist_data
}
