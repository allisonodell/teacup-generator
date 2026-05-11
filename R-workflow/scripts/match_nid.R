library(dplyr)
library(readr)
library(stringr)
library(sf)

# Load locations geojson
locs_sf <- st_read("config/locations.geojson", quiet = TRUE)
cat("Locations:", nrow(locs_sf), "\n")

# Load NID
nid <- read_csv("data/reference/nid.csv", skip = 1, show_col_types = FALSE)
nid_valid <- nid |>
  filter(!is.na(Latitude), !is.na(Longitude)) |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

cat("NID dams with coords:", nrow(nid_valid), "\n\n")

# For each location, find all NID dams within 10km.
# Among those, prefer: (1) name match with largest storage, (2) largest storage overall.
# If nothing within 10km, use nearest feature but flag it.

MAX_DIST <- 10000  # 10km threshold

locs_sf$nid_dam_name <- NA_character_
locs_sf$nid_owner_names <- NA_character_
locs_sf$nid_owner_type <- NA_character_
locs_sf$nid_distance_km <- NA_real_
locs_sf$nid_id <- NA_character_
locs_sf$nid_max_storage <- NA_real_

for (i in seq_len(nrow(locs_sf))) {
  loc <- locs_sf[i, ]
  loc_name <- tolower(loc$Name)

  # Find all NID dams within 10km
  within_idx <- st_is_within_distance(loc, nid_valid, dist = MAX_DIST)[[1]]

  if (length(within_idx) > 0) {
    nearby <- nid_valid[within_idx, ]
    loc_rep <- loc[rep(1, nrow(nearby)), ]
    nearby$dist_km <- as.numeric(st_distance(loc_rep, nearby, by_element = TRUE)) / 1000

    # Check for name matches among nearby dams
    nearby$name_match <- str_detect(tolower(nearby$`Dam Name`), fixed(loc_name)) & !is.na(nearby$`Dam Name`)

    # Prefer: name match with largest storage > largest storage nearby
    if (any(nearby$name_match)) {
      best <- nearby |>
        filter(name_match) |>
        arrange(desc(`Max Storage (Acre-Ft)`)) |>
        slice(1)
    } else {
      best <- nearby |>
        arrange(desc(`Max Storage (Acre-Ft)`)) |>
        slice(1)
    }
  } else {
    # Nothing within 10km - use nearest but flag
    nearest_idx <- st_nearest_feature(loc, nid_valid)
    best <- nid_valid[nearest_idx, ]
    best$dist_km <- as.numeric(st_distance(loc, best)) / 1000
  }

  locs_sf$nid_dam_name[i] <- best$`Dam Name`
  locs_sf$nid_owner_names[i] <- best$`Owner Names`
  locs_sf$nid_owner_type[i] <- best$`Primary Owner Type`
  locs_sf$nid_distance_km[i] <- round(best$dist_km, 2)
  locs_sf$nid_id[i] <- best$`NID ID`
  locs_sf$nid_max_storage[i] <- best$`Max Storage (Acre-Ft)`
}

cat("Distance distribution (km):\n")
print(summary(locs_sf$nid_distance_km))

cat("\nMatches within 5km:", sum(locs_sf$nid_distance_km <= 5), "\n")
cat("Matches > 5km (review):", sum(locs_sf$nid_distance_km > 5), "\n\n")

# Show all matches > 5km
far <- locs_sf |>
  st_drop_geometry() |>
  filter(nid_distance_km > 5) |>
  select(Name, nid_dam_name, nid_distance_km, nid_owner_names)
if (nrow(far) > 0) {
  cat("Far matches (>5km):\n")
  for (j in seq_len(nrow(far))) {
    cat(sprintf("  %s -> %s (%.1f km) [%s]\n",
        far$Name[j], far$nid_dam_name[j], far$nid_distance_km[j], far$nid_owner_names[j]))
  }
}

cat("\nOwner type breakdown:\n")
print(table(locs_sf$nid_owner_type, useNA = "ifany"))

# Write for review
locs_sf |>
  st_drop_geometry() |>
  select(Name, nid_dam_name, nid_id, nid_distance_km, nid_owner_names, nid_owner_type, nid_max_storage) |>
  write_csv("output/nid_matches.csv")
cat("\nWrote output/nid_matches.csv\n")
