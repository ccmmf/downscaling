# This script is used to explore LandIQ data 
# and reconcile design points generated in previous iterations 
# with updated LandIQ data. 
# Will be obsolete once we re-generate new design points in phase 3

## One off code used to fix LandIQ data has been moved to a gist:.groups
##  https://gist.github.com/dlebauer/c7e16a0f8c741a85c8ff7794c002a558


## Make sure that LandIQ data in data_raw/ is up to date
## rsync -av --exclude='crops_all_years.RData' --exclude='crops_all_years.csv' ~/ccmmf/LandIQ_data/ ~/ccmmf/data_raw/cadwr_land_use/
## rsync -av ~/ccmmf/LandIQ_data/crops_all_years.csv ~/ccmmf/data/cadwr_land_use/

library(tidyverse)
library(multidplyr)
cl <- new_cluster(parallel::detectCores() - 1)
cluster_library(cl, "dplyr")

## Load Configuration
##   later can be replaced w/ config.yml or pecan.xml
config_file <- here::here("000-config.R") 
if (file.exists(config_file)) {
  source(config_file)
} else {
  PEcAn.logger::logger.error(
    "Config file not found, are you in the correct directory?", getwd()
  )
}

crops_all <- data.table::fread(
  file.path(data_dir, "cadwr_land_use", "crops_all_years.csv")
) |>
  filter(!is.na(CLASS)) |>
  mutate(
    SUBCLASS = replace_na(SUBCLASS, 0)
  )   
  
crop_pft_map <- readr::read_csv(
  file.path(raw_data_dir, "cadwr_land_use", "CARB_PFTs_table.csv")
) |>
  filter(pft_group %in% c("herbaceous", "woody"))

# Look at # records per CLASS each year
PEcAn.logger::logger.info(
  "\nNumber of records per CLASS each year:\n"
)
crops_all |>
  filter(CLASS %in% crop_pft_map$crop_type) |>
  group_by(year, CLASS) |>
  summarize(
    n = n()
  ) |>
  arrange(year, CLASS) |>
  pivot_wider(
    names_from = year,
    values_from = n,
    values_fill = 0
  )
  
# Perform join to map PFTs
crops_all_pft <- crops_all |>
  left_join(
    crop_pft_map,
    by = c(
      "CLASS"    = "crop_type",
      "SUBCLASS" = "crop_code"
    )
  )  
# Split into multidplyr_df by year and season
# For parallel dplyr
crops_all_pft_x <- crops_all_pft |>
  as_tibble() |>
  group_by(year, season) |>
  partition(cluster = cl)

# 1) How many rows lost pft_group (i.e. join failures) by year/season?
crops_all_pft_x |>
  summarise(n_total = n(),
            n_missing = sum(is.na(pft_group)),
            pct_missing = round(n_missing / n_total * 100)) |>
  collect() |>
  print(n = Inf)

# Which CLASS/SUBCLASS combinations never matched?
missing_keys <- crops_all |>
  distinct(CLASS, SUBCLASS) |>
  anti_join(
    crop_pft_map,
    by = c("CLASS" = "crop_type", "SUBCLASS" = "crop_code")
  )
PEcAn.logger::logger.info(
  "\nUnmatched CLASS/SUBCLASS pairs:\n",
  "Expect YP, X, U, NR, I, UL (non-croplands)",
  wrap = FALSE
)
knitr::kable(missing_keys)


# How common are those bad combinations in the raw data?
crops_all |>
  semi_join(missing_keys, by = c("CLASS", "SUBCLASS")) |>
  count(CLASS, SUBCLASS, name = "freq") |>
  arrange(desc(freq)) |>
  print(n = Inf)

# count the number of records in each value of MULTIUSE
crops_all |>
  group_by(MULTIUSE) |>
  summarize(
    n = n(),
    .groups = "drop"
  ) |>
  arrange(desc(n))

# Unique fields with MULTIUSE == "I"
crops_all |>
  filter(MULTIUSE %in% c("M", "I")) |>
  group_by(MULTIUSE) |>
  summarize(n_unique_id = n_distinct(UniqueID))

# Find fields with multiple PFTs

# Summarize only those fields that have both woody & herbaceous PFTs in the same year & season
summary_df <- crops_all_pft_x |>
  filter(!is.na(pft_group)) |>
  group_by(UniqueID, year, season) |>
  summarize(
    has_woody      = any(pft_group == "woody"),
    has_herbaceous = any(pft_group == "herbaceous")
  ) |>
  filter(has_woody & has_herbaceous) |>
  group_by(year, season) |>
  summarize(
    n_pft     = 2,
    pft_types = "herbaceous, woody",
    n_fields  = n_distinct(UniqueID)
  )

res <- collect(summary_df)

res |>
  arrange(year, season, desc(n_fields))

woody_herb_summary <- crops_all_pft_x |>
  filter(!is.na(pft_group)) |>
  # group at the field <U+00D7> year <U+00D7> season level
  group_by(UniqueID, year, season) |>
  summarise(
    n_pft = n_distinct(pft_group),
    woody_pcnt = sum(PCNT[pft_group == "woody"], na.rm = TRUE),
    herb_pcnt = sum(PCNT[pft_group == "herbaceous"], na.rm = TRUE)
  ) |>
  ungroup() |> 
  filter(n_pft == 2)  
  
z <- woody_herb_summary |>
  collect() |>
  #filter(woody_pcnt > 0, herb_pcnt > 0) |>
  mutate(
    total_pcnt = woody_pcnt + herb_pcnt
  )

z |>
  group_by(woody_pcnt, herb_pcnt) |>
  summarize(
    n_occurances = n()
  ) |>
  arrange(desc(n_occurances)) |>
  print(n = 50)

# Find all fields with both woody and herbaceous PFTs in the same year and season
woody_herb_fields_by_year_season <- crops_all_pft_x |>
  filter(!is.na(pft_group)) |>
  group_by(UniqueID, year, season) |>
  summarize(
    has_woody = any(pft_group == "woody"),
    has_herbaceous = any(pft_group == "herbaceous")
  ) |>
  filter(has_woody & has_herbaceous) |>
  select(-has_woody, -has_herbaceous) |>
  collect()

# To see how many unique fields per year/season:
woody_herb_fields_by_year_season |>
  group_by(year, season) |>
  summarize(n_fields = n_distinct(UniqueID)) |>
  arrange(year, season)

### Trying reconcile 2016 and 2018 LandIQ data 

crops_all_2016 <- crops_all |>
  filter(year == 2016)

crops_all_2018 <- crops_all |>
  filter(year == 2018)

dwr_2018 <- terra::vect(
  file.path(raw_data_dir, "cadwr_land_use", "LandIQ_shapefiles", "i15_Crop_Mapping_2018_SHP", "i15_Crop_Mapping_2018.shp")
)  |> 
  terra::project("epsg:3310")

dwr_2016 <- terra::vect(
  file.path(raw_data_dir, "cadwr_land_use", "LandIQ_shapefiles", "i15_Crop_Mapping_2016_SHP", "i15_Crop_Mapping_2016.shp")
) |>
  terra::project("epsg:3310")

## Get overlapping points
dwr_x <- terra::intersect(dwr_2018, dwr_2016)

## Goal is to reconcile dwr_2016 Unique_ID with dwr_2018 UniqueID
## join them by geometry, creating id_2016 and id_2018
## find where UniqueID has changed (or not)
## where 2016 is missing from 2018, find nearest polygon and calculate distance 
dwr_merged <- terra::intersect(dwr_2018, dwr_2016) |>
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
  here::here("data/design_points.csv"),
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
