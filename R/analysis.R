#' vis_change
#' @description
#' Statistical analysis of changes of greenspace over years or time during a year.
#' @param method
#' @param change_threshould description
#'
#' @return list
#'

vis_change <- function(method = NULL) {

}

#' Generate Greenspace Density Grid
#' @description
#' Divides a greenspace raster into a regular grid and computes
#' per-cell mean change or point density.
#' @param r SpatRaster. A single-layer raster representing greenspace change.
#' @param stat Character. One of `'mean'` or `'sum'`.
#' Determines which summary statistic is returned.
#' @return An `sf` object with grid polygons and the requested summary statistic in the column `stat_value`.
#' @export
#'
#' @examples
#'
#' grid <- grid_density(r)
#' plot(grid["stat_value"])
#' @export
grid_density <- function(r, grid_size = NULL, stat = c("mean", "sum"), threshould = 0.5) {
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
    stat == "density" ~ grid_stats$density,
    stat == "mean" ~ grid_stats$mean_change,
    TRUE ~ NA_real_
  )

  return(grid_stats)
}

