#' Generate Density Grid
#' @description
#' Divides a greenspace raster into a regular grid and computes
#' per-cell mean change or point density.
#' @param r SpatRaster. A single-layer raster representing greenspace change.
#' @param stat Character. One of `'max'`, `'min'`, `'mean'`, or `'sum'`.
#' Determines which summary statistic is returned.
#' @return An `sf` object with grid polygons and the requested summary statistic in the column `stat_value`.
#' @export
#'
#' @examples
#'
#' grid <- grid_density(r)
#' plot(grid["stat_value"])
#' @export
grid_density <- function(r, grid_size = NULL, stat = "mean", threshould = 0.5) {
  stopifnot(inherits(r, "SpatRaster"), terra::nlyr(r) == 1)

  # Estimate grid size if not provided
  ext <- terra::ext(r)
  width <- ext[2] - ext[1]
  height <- ext[4] - ext[3]

  if (is.null(grid_size)) {
    # Aim for ~40-60 grid cells total
    max_dim <- max(width, height)
    grid_size <- max_dim / 8  # Adjust divisor to control coarseness
  }

  # Create grid as sf
  r_bbox <- sf::st_as_sfc(sf::st_bbox(r))
  grid <- sf::st_make_grid(r_bbox, cellsize = grid_size, square = TRUE)
  grid_sf <- sf::st_sf(grid_id = seq_along(grid), geometry = grid)

  # Convert raster to points
  pts_df <- terra::as.data.frame(r, xy = TRUE)
  colnames(pts_df)[3] <- "value"
  pts_df <- pts_df[!is.na(pts_df$value), ]
  if (nrow(pts_df) == 0) stop("No valid data points found.")
  pts_sf <- sf::st_as_sf(pts_df, coords = c("x", "y"), crs = terra::crs(r))

  # split grid based on value statistics



  grid_stats$stat_value <- dplyr::case_when(
    stat == "sum" ~ grid_stats$sum,
    stat == "mean" ~ grid_stats$mean,
    TRUE ~ NA_real_
  )

  return(grid_stats)
}

#' Human Exposure to Greenspace
#' @description
#' A short description...
#'
#' @param r A SpatRaster with single/multiple greenspace layer(s).
#' @param year numeric. Year of the GHSL dataset to use.
#' Must be one of: 1975, 1980, 1985, 1990, 1995, 2000, 2005,
#' 2010, 2015, 2020, 2025, or 2030. Default is 2020.
#'
#' @references
#' Chen, B., Wu, S., Song, Y. et al. Contrasting inequality in human exposure to
#' greenspace between cities of Global North and Global South. Nat Commun 13,
#' 4636 (2022). https://doi.org/10.1038/s41467-022-32258-4
#'
#' @importFrom terra project
#' @importFrom  utils unzip
#' @export
human_etg <- function(r = NULL, year = 2020) {
  bbox <- as.vector(terra::ext(r))
  d_mode <- 'auto'
  # check os
  os <- Sys.info()[["sysname"]]
  d_mode <- if (Sys.info()[["sysname"]] == "Windows") 'wb' else 'auto'
  years <- c(2030, 2025, 2020, 2015, 2010, 2005, 2000, 1995, 1990, 1985, 1980, 1975)
  result_list <- list()
  temp_paths <- c()
  on.exit(unlink(temp_paths, recursive = TRUE), add = TRUE)

  cli::cli_alert_info('Start downloading population density data from the GHSL (Global Human Settlement Layer) dataset ...')
  if (year %in% years) {
    intersected_tiles <- ghsl_tiles[sf::st_intersects(ghsl_tiles, bbox, sparse = FALSE), ]
    for (i in seq_len(nrow(intersected_tiles))) {
      temp_zip <- tempfile(fileext = ".zip")
      url_ <- get_GHSurl(year, intersected_tiles$tile_id[i], 'pop')
      utils::download.file(url_,
                           destfile = temp_zip,
                           mode = d_mode,
                           quiet = TRUE)
      unzip_dir <- tempfile()
      utils::unzip(temp_zip, exdir = unzip_dir)
      tif_files <- list.files(unzip_dir, pattern = "\\.tif$", full.names = TRUE)
      if (length(tif_files) == 0) next
      rast_data <- terra::rast(tif_files[1])
      result_list[[length(result_list) + 1]] <- rast_data
      temp_paths <- c(temp_paths, temp_zip, unzip_dir)
    }
    if (length(result_list) == 0) {
      base::warning("No population rasters downloaded. Returning original polygons.")
      return(projected_poly)
    }
    cli::cli_alert_success('Finished downloading population data')

    # Combine all into one terra raster object
    r <- if (length(result_list) == 1) result_list[[1]] else do.call(terra::merge, result_list)
    r <- terra::project(r, paste0('EPSG:', 4326), method = 'near')

    # Aggregate 10-m greenspace values to 100-m population raster

  }
}

