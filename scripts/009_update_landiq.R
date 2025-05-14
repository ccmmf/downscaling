library(tidyverse)

load("~/ccmmf/LandIQ_data/crops_all_years.RData")

dwr_2018 <- terra::vect(
  "~/ccmmf/LandIQ_data/LandIQ_shapefiles/i15_Crop_Mapping_2018_SHP/i15_Crop_Mapping_2018.shp"
)  |> 
  terra::project("epsg:3310")
  

design_points <- read.csv("data/design_points.csv") |>
  select(-UniqueID)

design_points_vect <- design_points |>
  terra::vect(geom = c("lon", "lat"), crs = "epsg:4326") |>
  terra::project("epsg:3310")

design_points_ids <- terra::intersect(dwr_2018, design_points_vect) |>
  # Project to WGS84 (decimal degrees) before converting to dataframe
  terra::project("epsg:4326") |>
  as.data.frame(geom = "xy") |> 
  select(contains("id"), x, y) |>
  rename(lon = x, lat = y) |>
  mutate(
    lon = round(lon, 5),
    lat = round(lat, 5),
    match_type = "intersect"
  )

unmatched <- anti_join(design_points, design_points_ids, by = "site_id") |>
  distinct()

# Find nearest polygons for unmatched points
# Then append to the design_points_ids
if (nrow(unmatched) > 0) {
  unmatched_vect <- terra::vect(
    unmatched, geom = c("lon", "lat"), crs = "epsg:4326"
  ) |> terra::project("epsg:3310")
  
  # Calculate distance matrix and find nearest polygons
  nearest_data <- tibble(
    site_id = unmatched$site_id,
    point_index = seq_len(nrow(unmatched))
  ) |>
    mutate(
      nearest_info = purrr::map(point_index, function(i) {
        distances <- terra::distance(unmatched_vect[i], dwr_2018)
        min_idx <- which.min(distances)
        nearest_poly <- dwr_2018[min_idx]
        # Get the point coordinates in decimal degrees
        point_wgs84 <- terra::project(unmatched_vect[i], "epsg:4326")
        coords <- terra::crds(point_wgs84)
        tibble(
          UniqueID = nearest_poly$UniqueID,
          distance = min(distances),
          lon = round(coords[1], 5),
          lat = round(coords[2], 5)
        )
      })
    ) |>
    tidyr::unnest(nearest_info) |>
    select(-point_index) |>
    mutate(match_type = "nearest")


  PEcAn.logger::logger.info("Design points joined to nearest polygon:")
  nearest_data |>
    knitr::kable(digits = 5) 
  
  # Combine intersected points with nearest points
  design_points_ids_updated <- bind_rows(design_points_ids, nearest_data)
}
  

write.csv(design_points_ids_updated |>
  select(UniqueID, site_id, lat, lon) |>
  left_join(design_points |> 
      select(site_id, pft),
    by = "site_id") |>
  arrange(pft, lat, lon),
  "data/design_points.csv",
  row.names = FALSE
)

### Data Summaries
crops_all |>
  filter(year == 2018) |>
  group_by(COUNTY) |>
  summarize(
    n_fields = n_distinct(UniqueID),
    n_classes = n_distinct(CLASS),
    .groups = "drop"
  ) |>
  arrange(desc(n_fields)) |>
  print(n = 58)

# now for each class count the number of counties it appears in
crops_all |>
  filter(year == 2018) |>
  group_by(CLASS) |>
  summarize(
    n_counties = n_distinct(COUNTY),
    .groups = "drop"
  ) |>
  arrange(desc(n_counties)) |>
  print(n = 15)

### 

grass_fields <- crops_all |>
  filter(CLASS == "P", MULTIUSE == "S", season == 2) |>
  mutate(
    centx = round(as.numeric(centx)),
    centy = round(as.numeric(centy))
  ) |>
  group_by(centx, centy) |>
  filter(n_distinct(year) == 7) |>
  summarize(
    crops = paste0(unique(CLASS), unique(SUBCLASS), collapse = ", "),
    ids = paste(unique(UniqueID), collapse = ",")
  ) |>
  ungroup() |>
  sample_n(10) |>
  rename(lat = centy, lon = centx) |>
  terra::vect(crs = "epsg:3857") |>
  terra::project("epsg:4269") |>
  terra::as.data.frame(geom = "xy")
