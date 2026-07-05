library(tidyverse)
library(sf)

roads    <- st_read("data/osm_roads_raw.gpkg",   quiet = TRUE)
counties <- st_read("data/counties.gpkg",         quiet = TRUE)

# ── Assign region ─────────────────────────────────────────────────────────────
roads_proj    <- st_transform(roads, 3857)
counties_proj <- st_transform(counties, 3857)
region_join   <- st_join(roads_proj |> select(osm_id), counties_proj |> select(region))

roads <- roads |>
  left_join(st_drop_geometry(region_join), by = "osm_id") |>
  mutate(region = replace_na(region, "Outside Study Area"))

# ── Load DOT sources ──────────────────────────────────────────────────────────

# ODOT: Hamilton County road inventory (has geometry)
odot <- st_read("data/odot_inventory.gpkg", quiet = TRUE) |>
  st_transform(3857) |>
  select(
    odot_speed  = SPEED_LIMIT_NBR,
    odot_lanes  = LANES_NBR,
    odot_width  = LANE_WIDTH_NBR,
    odot_bike   = BIKE_LANE_TYPE_CD,
    odot_adt    = ADT_TOTAL_NBR,
    odot_fc     = FUNCTION_CLASS_CD
  )

# KYTC speed: tabular LRS — join to road segments via nearest-centroid spatial match
# Convert centroids of KYTC speed segments from the speed CSV (no geometry stored)
# Instead use the KYTC LRS shapefile centroids for spatial join
kytc_speed_csv <- read_csv("data/kytc_speed.csv", show_col_types = FALSE) |>
  select(RT_UNIQUE, CNTY_NAME, D_GOV_LEVEL, SPEEDLIM, MILES)

kytc_lanes <- st_read("data/kytc_lanes_nky.gpkg", quiet = TRUE) |>
  st_transform(3857) |>
  select(RT_UNIQUE, CNTY_NAME, LANES, LANEWID) |>
  left_join(kytc_speed_csv |> select(RT_UNIQUE, SPEEDLIM), by = "RT_UNIQUE") |>
  st_centroid()

# ── Spatial join: snap each OSM point to nearest DOT segment ─────────────────
roads_proj <- st_transform(roads, 3857)

# ODOT → Cincinnati segments (nearest within 50 m)
odot_join <- st_join(
  roads_proj |> filter(region == "Cincinnati (Hamilton Co.)") |> select(osm_id),
  odot,
  join = st_nearest_feature
)

# Distance filter — only accept matches within 50 m
nn_dist <- as.numeric(st_distance(
  roads_proj |> filter(region == "Cincinnati (Hamilton Co.)"),
  odot[st_nearest_feature(
    roads_proj |> filter(region == "Cincinnati (Hamilton Co.)"), odot
  ), ],
  by_element = TRUE
))
odot_join$dist_m <- nn_dist
odot_join <- odot_join |> filter(dist_m <= 50) |> st_drop_geometry()

# KYTC → NKy segments (nearest within 50 m)
kytc_join <- st_join(
  roads_proj |> filter(region == "Northern Kentucky") |> select(osm_id),
  kytc_lanes |> select(LANES, LANEWID, SPEEDLIM),
  join = st_nearest_feature
)
nn_dist_ky <- as.numeric(st_distance(
  roads_proj |> filter(region == "Northern Kentucky"),
  kytc_lanes[st_nearest_feature(
    roads_proj |> filter(region == "Northern Kentucky"), kytc_lanes
  ), ],
  by_element = TRUE
))
kytc_join$dist_m <- nn_dist_ky
kytc_join <- kytc_join |> filter(dist_m <= 50) |> st_drop_geometry()

message(paste("ODOT matched:", nrow(odot_join), "of",
              nrow(roads |> filter(region == "Cincinnati (Hamilton Co.)")), "Cincinnati segments"))
message(paste("KYTC matched:", nrow(kytc_join), "of",
              nrow(roads |> filter(region == "Northern Kentucky")), "NKy segments"))

# ── Merge DOT data back onto roads ────────────────────────────────────────────
roads <- roads |>
  left_join(odot_join |> select(osm_id, odot_speed, odot_lanes, odot_width,
                                 odot_bike, odot_adt, odot_fc, dist_m),
            by = "osm_id") |>
  left_join(kytc_join |> select(osm_id, kytc_lanes = LANES,
                                 kytc_lanewid = LANEWID, kytc_speed = SPEEDLIM,
                                 kytc_dist = dist_m),
            by = "osm_id")

# ── Build best-available speed / lanes / width ────────────────────────────────
# Priority: official DOT > OSM observed > highway-type imputation

parse_speed <- function(x) as.numeric(str_replace_all(x, "[^0-9.]", ""))

speed_imp <- tribble(
  ~highway,          ~sp,
  "motorway",        65, "motorway_link", 55,
  "trunk",           55, "trunk_link",    45,
  "primary",         45, "primary_link",  35,
  "secondary",       35, "secondary_link",30,
  "tertiary",        30, "tertiary_link", 25,
  "unclassified",    25, "residential",   25,
  "living_street",   15, "pedestrian",    10
)
lane_imp <- tribble(
  ~highway,          ~ln,
  "motorway",        4L, "motorway_link", 2L,
  "trunk",           4L, "trunk_link",    2L,
  "primary",         4L, "primary_link",  2L,
  "secondary",       2L, "secondary_link",2L,
  "tertiary",        2L, "tertiary_link", 1L,
  "unclassified",    1L, "residential",   1L,
  "living_street",   1L, "pedestrian",    1L
)

roads <- roads |>
  left_join(speed_imp, by = "highway") |>
  left_join(lane_imp,  by = "highway") |>
  mutate(
    osm_speed  = parse_speed(maxspeed),
    osm_lanes  = as.integer(lanes),

    # Best speed: DOT official > OSM observed > type imputation
    speed_final = case_when(
      !is.na(odot_speed)  ~ as.numeric(odot_speed),
      !is.na(kytc_speed)  ~ as.numeric(kytc_speed),
      !is.na(osm_speed)   ~ osm_speed,
      TRUE                ~ sp
    ),
    speed_source = case_when(
      !is.na(odot_speed)  ~ "ODOT",
      !is.na(kytc_speed)  ~ "KYTC",
      !is.na(osm_speed)   ~ "OSM",
      TRUE                ~ "imputed"
    ),

    # Best lanes: DOT > OSM > imputed
    lanes_final = case_when(
      !is.na(odot_lanes)  ~ as.integer(odot_lanes),
      !is.na(kytc_lanes)  ~ as.integer(kytc_lanes),
      !is.na(osm_lanes)   ~ osm_lanes,
      TRUE                ~ ln
    ),
    lanes_source = case_when(
      !is.na(odot_lanes)  ~ "ODOT",
      !is.na(kytc_lanes)  ~ "KYTC",
      !is.na(osm_lanes)   ~ "OSM",
      TRUE                ~ "imputed"
    ),

    # Lane width — DOT only (OSM doesn't carry this)
    width_ft = coalesce(odot_width, as.numeric(kytc_lanewid)),

    # Bike infrastructure present
    has_bike = case_when(
      !is.na(odot_bike) & odot_bike != "" ~ TRUE,
      sidewalk %in% c("both","yes","left","right") ~ TRUE,
      TRUE ~ FALSE
    )
  )

# ── Component scores (0 = friendly, 1 = hostile) ──────────────────────────────
highway_score <- tribble(
  ~highway,          ~hw_score,
  "motorway",        1.00, "motorway_link",  0.90,
  "trunk",           0.90, "trunk_link",     0.80,
  "primary",         0.75, "primary_link",   0.65,
  "secondary",       0.60, "secondary_link", 0.50,
  "tertiary",        0.45, "tertiary_link",  0.35,
  "unclassified",    0.25, "residential",    0.20,
  "living_street",   0.05, "pedestrian",     0.00
)

roads <- roads |>
  left_join(highway_score, by = "highway") |>
  mutate(
    # Speed: 10 mph → 0, 65 mph → 1
    speed_score = (pmin(speed_final, 65) - 10) / 55,

    # Lanes: 1 → 0, 6+ → 1
    lanes_score = pmin((lanes_final - 1) / 5, 1),

    # Lane width: narrow (≤10 ft) → 0, wide (≥14 ft) → 1
    # Wide lanes encourage higher speed, less comfortable for pedestrians
    width_score = case_when(
      !is.na(width_ft) ~ pmin(pmax((width_ft - 10) / 4, 0), 1),
      TRUE             ~ NA_real_
    ),

    # Sidewalk (OSM)
    sidewalk_score = case_when(
      sidewalk %in% c("both","yes","left","right") ~ 0,
      sidewalk %in% c("no","none")                 ~ 1,
      TRUE                                         ~ NA_real_
    ),

    # Bike infrastructure reduces pedestrian hostility
    bike_score = if_else(has_bike, 0, NA_real_),

    # Surface (OSM)
    surface_score = case_when(
      surface %in% c("paved","asphalt","concrete","sett","paving_stones") ~ 0.1,
      surface %in% c("gravel","unpaved","dirt","ground","grass","mud")     ~ 0.9,
      surface %in% c("cobblestone","unhewn_cobblestone")                   ~ 0.6,
      TRUE ~ NA_real_
    )
  )

# ── Composite: weighted mean over non-null components ─────────────────────────
W <- c(hw = 0.25, speed = 0.30, lanes = 0.20, width = 0.10,
       sidewalk = 0.08, bike = 0.04, surface = 0.03)

roads <- roads |>
  rowwise() |>
  mutate(
    scores = list(c(
      hw       = hw_score      * W["hw"],
      speed    = speed_score   * W["speed"],
      lanes    = lanes_score   * W["lanes"],
      width    = if (!is.na(width_score))    width_score    * W["width"]    else NA_real_,
      sidewalk = if (!is.na(sidewalk_score)) sidewalk_score * W["sidewalk"] else NA_real_,
      bike     = if (!is.na(bike_score))     bike_score     * W["bike"]     else NA_real_,
      surface  = if (!is.na(surface_score))  surface_score  * W["surface"]  else NA_real_
    )),
    wts = list(c(
      hw       = W["hw"],
      speed    = W["speed"],
      lanes    = W["lanes"],
      width    = if (!is.na(width_score))    W["width"]    else NA_real_,
      sidewalk = if (!is.na(sidewalk_score)) W["sidewalk"] else NA_real_,
      bike     = if (!is.na(bike_score))     W["bike"]     else NA_real_,
      surface  = if (!is.na(surface_score))  W["surface"]  else NA_real_
    )),
    hostility_index = sum(scores, na.rm = TRUE) / sum(wts, na.rm = TRUE),
    components_used = sum(!is.na(scores))
  ) |>
  ungroup() |>
  select(-scores, -wts)

# ── Data source coverage report ───────────────────────────────────────────────
coverage <- roads |>
  st_drop_geometry() |>
  filter(region != "Outside Study Area") |>
  group_by(region) |>
  summarise(
    n                  = n(),
    pct_speed_dot      = mean(speed_source %in% c("ODOT","KYTC")) * 100,
    pct_speed_osm      = mean(speed_source == "OSM") * 100,
    pct_speed_imputed  = mean(speed_source == "imputed") * 100,
    pct_lanes_dot      = mean(lanes_source %in% c("ODOT","KYTC")) * 100,
    pct_lanes_osm      = mean(lanes_source == "OSM") * 100,
    pct_lanes_imputed  = mean(lanes_source == "imputed") * 100,
    pct_width          = mean(!is.na(width_ft)) * 100,
    pct_sidewalk       = mean(!is.na(sidewalk_score)) * 100,
    pct_bike           = mean(!is.na(bike_score)) * 100,
    .groups = "drop"
  )

# ── Summary stats ─────────────────────────────────────────────────────────────
region_summary <- roads |>
  st_drop_geometry() |>
  filter(region != "Outside Study Area") |>
  group_by(region) |>
  summarise(
    n                  = n(),
    mean_hostility     = mean(hostility_index, na.rm = TRUE),
    median_hostility   = median(hostility_index, na.rm = TRUE),
    pct_high           = mean(hostility_index > 0.6, na.rm = TRUE) * 100,
    mean_speed         = mean(speed_final, na.rm = TRUE),
    mean_lanes         = mean(lanes_final, na.rm = TRUE),
    mean_width_ft      = mean(width_ft, na.rm = TRUE),
    length_km          = sum(way_length_m, na.rm = TRUE) / 1000,
    .groups = "drop"
  )

# By highway type
type_summary <- roads |>
  st_drop_geometry() |>
  filter(
    region != "Outside Study Area",
    highway %in% c("primary","secondary","tertiary","residential","unclassified")
  ) |>
  group_by(region, highway) |>
  summarise(
    n              = n(),
    mean_hostility = mean(hostility_index, na.rm = TRUE),
    mean_speed     = mean(speed_final, na.rm = TRUE),
    mean_lanes     = mean(lanes_final, na.rm = TRUE),
    .groups        = "drop"
  )

# ── Save ──────────────────────────────────────────────────────────────────────
st_write(roads, "data/roads_hostility.gpkg", delete_dsn = TRUE)
write_csv(coverage,       "data/coverage_summary.csv")
write_csv(region_summary, "data/hostility_summary.csv")
write_csv(type_summary,   "data/type_summary.csv")

message("Saved: data/roads_hostility.gpkg")

print(coverage)
print(region_summary)
print(type_summary)
