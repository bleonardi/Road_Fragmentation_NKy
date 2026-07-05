library(tidyverse)
library(sf)

roads <- st_read("data/osm_roads_raw.gpkg", quiet = TRUE)
counties <- st_read("data/counties.gpkg", quiet = TRUE)

# ── Parse speed and lane fields ───────────────────────────────────────────────
parse_speed <- function(x) {
  as.numeric(str_replace_all(x, "[^0-9.]", ""))
}

roads <- roads |>
  mutate(
    speed_mph = parse_speed(maxspeed),
    speed_mph = case_when(
      is.na(speed_mph) & highway %in% c("motorway","motorway_link")    ~ 65,
      is.na(speed_mph) & highway %in% c("trunk","trunk_link")          ~ 55,
      is.na(speed_mph) & highway %in% c("primary","primary_link")      ~ 45,
      is.na(speed_mph) & highway %in% c("secondary","secondary_link")  ~ 35,
      is.na(speed_mph) & highway %in% c("tertiary","tertiary_link")    ~ 30,
      is.na(speed_mph) & highway %in% c("residential","unclassified")  ~ 25,
      is.na(speed_mph) & highway == "living_street"                     ~ 15,
      TRUE ~ speed_mph
    ),
    lane_count = as.integer(lanes),
    lane_count = case_when(
      is.na(lane_count) & highway %in% c("motorway")                       ~ 4L,
      is.na(lane_count) & highway %in% c("trunk","primary")               ~ 4L,
      is.na(lane_count) & highway %in% c("secondary","tertiary")          ~ 2L,
      is.na(lane_count) & highway %in% c("residential","unclassified",
                                          "living_street")                 ~ 1L,
      TRUE ~ lane_count
    )
  )

# ── LTS assignment (Mekuria et al. 2012, simplified for OSM attributes) ───────
roads <- roads |>
  mutate(
    lts = case_when(
      highway %in% c("pedestrian","living_street")                                            ~ 1L,
      highway %in% c("residential","unclassified") & speed_mph <= 25                         ~ 1L,
      highway %in% c("residential","unclassified") & speed_mph <= 30                         ~ 2L,
      highway %in% c("residential","unclassified")                                            ~ 3L,
      highway %in% c("tertiary","tertiary_link")   & speed_mph <= 25 & lane_count <= 2       ~ 2L,
      highway %in% c("tertiary","tertiary_link")   & speed_mph <= 30 & lane_count <= 2       ~ 3L,
      highway %in% c("tertiary","tertiary_link")                                              ~ 4L,
      highway %in% c("secondary","secondary_link") & speed_mph <= 30 & lane_count <= 2       ~ 3L,
      highway %in% c("secondary","secondary_link")                                            ~ 4L,
      highway %in% c("primary","primary_link","trunk","trunk_link",
                     "motorway","motorway_link")                                              ~ 4L,
      TRUE ~ 3L
    ),
    lts_label = case_when(
      lts == 1 ~ "LTS 1 — Low stress",
      lts == 2 ~ "LTS 2 — Moderate",
      lts == 3 ~ "LTS 3 — High stress",
      lts == 4 ~ "LTS 4 — Very high stress"
    )
  )

# ── Assign region ─────────────────────────────────────────────────────────────
roads_proj   <- st_transform(roads, 3857)
counties_proj <- st_transform(counties, 3857)

region_join <- st_join(roads_proj |> select(osm_id), counties_proj |> select(region))

roads <- roads |>
  left_join(st_drop_geometry(region_join), by = "osm_id") |>
  mutate(region = replace_na(region, "Outside Study Area"))

# ── Stroad classification ─────────────────────────────────────────────────────
roads <- roads |>
  mutate(
    is_stroad = highway %in% c("primary","secondary","tertiary") &
                speed_mph >= 35 &
                lane_count >= 4
  )

st_write(roads, "data/roads_lts.gpkg", delete_dsn = TRUE)
message("Saved: data/roads_lts.gpkg")

# ── Summary tables ────────────────────────────────────────────────────────────
lts_summary <- roads |>
  st_drop_geometry() |>
  filter(region != "Outside Study Area") |>
  group_by(region, lts_label) |>
  summarise(
    segments  = n(),
    length_km = sum(way_length_m, na.rm = TRUE) / 1000,
    .groups   = "drop"
  ) |>
  group_by(region) |>
  mutate(pct_length = length_km / sum(length_km) * 100) |>
  ungroup()

stroad_summary <- roads |>
  st_drop_geometry() |>
  filter(region != "Outside Study Area") |>
  group_by(region) |>
  summarise(
    total_segments  = n(),
    stroad_segments = sum(is_stroad, na.rm = TRUE),
    stroad_pct      = stroad_segments / total_segments * 100,
    .groups         = "drop"
  )

write_csv(lts_summary,   "data/lts_summary.csv")
write_csv(stroad_summary, "data/stroad_summary.csv")
message("Saved: data/lts_summary.csv, data/stroad_summary.csv")

print(lts_summary)
print(stroad_summary)
