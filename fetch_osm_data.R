library(tidyverse)
library(sf)
library(arrow)
library(tigris)

options(tigris_use_cache = TRUE)

STREETS_PARQUET <- "../Street_Type_Divergence/data/processed/streets.parquet"

# ── Study area bounding box ───────────────────────────────────────────────────
# Hamilton County (Cincinnati) + Boone/Kenton/Campbell (NKy core)
BBOX <- list(xmin = -84.82, xmax = -84.20, ymin = 38.95, ymax = 39.35)

# ── Load and filter existing streets parquet ──────────────────────────────────
message("Loading streets parquet...")
streets <- read_parquet(STREETS_PARQUET)

roads <- streets |>
  filter(
    state %in% c("ohio", "kentucky"),
    centroid_lon >= BBOX$xmin, centroid_lon <= BBOX$xmax,
    centroid_lat >= BBOX$ymin, centroid_lat <= BBOX$ymax,
    highway %in% c(
      "motorway", "motorway_link",
      "trunk", "trunk_link",
      "primary", "primary_link",
      "secondary", "secondary_link",
      "tertiary", "tertiary_link",
      "residential", "unclassified",
      "living_street", "pedestrian"
    )
  )

message(paste("Filtered to", nrow(roads), "road segments in study area"))

# Convert to sf using centroids (geometry-lite approach — sufficient for
# LTS scoring and regional comparison; swap to PBF extract if line geometry needed)
roads_sf <- roads |>
  st_as_sf(coords = c("centroid_lon", "centroid_lat"), crs = 4326)

st_write(roads_sf, "data/osm_roads_raw.gpkg", delete_dsn = TRUE)
message("Saved: data/osm_roads_raw.gpkg")

# ── Municipality / county boundaries ─────────────────────────────────────────
message("Fetching place boundaries...")

oh_places <- places(state = "OH", cb = TRUE) |>
  st_transform(4326) |>
  filter(NAME == "Cincinnati") |>
  select(NAME, GEOID, geometry) |>
  mutate(state = "OH")

ky_places <- places(state = "KY", cb = TRUE) |>
  st_transform(4326) |>
  filter(NAME %in% c(
    "Covington", "Newport", "Florence", "Erlanger",
    "Fort Mitchell", "Fort Wright", "Edgewood",
    "Bellevue", "Dayton", "Cold Spring", "Highland Heights",
    "Wilder", "Alexandria", "Elsmere"
  )) |>
  select(NAME, GEOID, geometry) |>
  mutate(state = "KY")

oh_county <- counties(state = "OH", cb = TRUE) |>
  st_transform(4326) |>
  filter(NAME == "Hamilton") |>
  select(NAME, GEOID, geometry) |>
  mutate(region = "Cincinnati (Hamilton Co.)")

ky_counties <- counties(state = "KY", cb = TRUE) |>
  st_transform(4326) |>
  filter(NAME %in% c("Boone", "Kenton", "Campbell")) |>
  select(NAME, GEOID, geometry) |>
  mutate(region = "Northern Kentucky")

all_places   <- bind_rows(oh_places, ky_places)
counties_study <- bind_rows(oh_county, ky_counties)

st_write(all_places,    "data/places.gpkg",  delete_dsn = TRUE)
st_write(counties_study, "data/counties.gpkg", delete_dsn = TRUE)
message("Saved: data/places.gpkg, data/counties.gpkg")

message("Done.")
