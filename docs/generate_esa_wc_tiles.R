library(sf)
library(dplyr)
library(readr)
library(stringr)

f <- aws.s3::get_bucket(
  bucket = "esa-worldcover",
  region = "eu-central-1",
  max = Inf
)
f_ <- tibble::tibble(
  key = vapply(f, function(x) x[["Key"]], character(1)),
)
wc_tiles <- data.frame(f_)
wc_tiles <- str_split_fixed(wc_tiles$key, "\\.", 2)
wc_tiles <- wc_tiles[wc_tiles[,2] =='tif', ]
wc_tiles <- as.data.frame(wc_tiles)
split_wc_tiles <- str_split_fixed(wc_tiles$V1, "/", 2)
v100_wc_tiles <- as.data.frame(split_wc_tiles[split_wc_tiles[,1] =='v100', ])
v200_wc_tiles <- as.data.frame(split_wc_tiles[split_wc_tiles[,1] =='v200', ])

# Convert tile name like "N42W084" to coordinates
parse_tile <- function(tile) {
  texts <- strsplit(tile, split = "_")[[1]]
  if (texts[length(texts)] == 'Map') {
    tile <- strsplit(tile, split = "_")[[1]][6]
    lat_dir <- substr(tile, 1, 1)
    lat_val <- as.numeric(substr(tile, 2, 3))
    lon_dir <- substr(tile, 4, 4)
    lon_val <- as.numeric(substr(tile, 5, 7))

    lat <- ifelse(lat_dir == "N", lat_val, -lat_val)
    lon <- ifelse(lon_dir == "E", lon_val, -lon_val)

    tibble(tile = tile,
           lon_min = lon,
           lon_max = lon + 3,
           lat_min = lat,
           lat_max = lat + 3)
  }
}

# Create bounding boxes for all tiles
grid_info_v100 <- bind_rows(lapply(v100_wc_tiles$V2, parse_tile))
grid_info_v200 <- bind_rows(lapply(v200_wc_tiles$V2, parse_tile))

# Create polygons
tile_polygons_v100 <- grid_info_v100 %>%
  rowwise() %>%
  mutate(geometry = list(st_polygon(list(matrix(
    c(
      lon_min, lat_min,
      lon_max, lat_min,
      lon_max, lat_max,
      lon_min, lat_max,
      lon_min, lat_min
    ), ncol = 2, byrow = TRUE))))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

tile_polygons_v200 <- grid_info_v200 %>%
  rowwise() %>%
  mutate(geometry = list(st_polygon(list(matrix(
    c(
      lon_min, lat_min,
      lon_max, lat_min,
      lon_max, lat_max,
      lon_min, lat_max,
      lon_min, lat_min
    ), ncol = 2, byrow = TRUE))))) %>%
  ungroup() %>%
  st_as_sf(crs = 4326)

# View result
plot(tile_polygons_v100$geometry)
plot(tile_polygons_v200$geometry)
