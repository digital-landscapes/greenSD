#' exposure
#' @description
#' Computes population-weighted greenspace fraction or human exposure to
#' greenspace based on a population-weighted exposure model (Chen et al., 2022),
#' using population data from the Global Human Settlement Layer (GHSL;
#' Pesaresi et al., 2024).
#' See **Details** for the underlying method and assumptions.
#'
#' @param r A SpatRaster with single/multiple greenspace layer(s), typically
#' the output from [get_gsdc()], [get_esa_wc()], or [get_s2a_ndvi()].
#' @param inputtype character. Data type for greenspace. Must be either `"fb"` (default),
#' `"ndvi"`. If `"fb"`, either fractional or binary (where non-green = 0 and green = 1)
#' will be taken. If `"ndvi"`, NDVI will be used to approximate greenspace.
#' @param res numeric vector of length 2. The actual spatial resolution (in meters).
#' Default is `c(10, 10)`.
#' @param threshold numeric vector. Threshold(s) for classify greenspace
#' based on NDVI layer(s), defaulting to `c(0.2, 0.2, 0.2)`. The length of the thresholds
#' depends the number of NDVI layers in the input raster `r`. See details.
#' @param pop_year numeric. Year of the GHSL dataset to use.
#' Must be one of: 2015, 2020, 2025, or 2030. Default is 2020.
#' @param radius numeric. Buffer radius (in meters) used for local averaging.
#' Default is `500`.
#' @param grid_size numeric. Optional. If provided, output is aggregated to grid cells
#' of this size (in meters) and returned as an `sf` object.
#' @param height logical. Whether to compute greenspace volume for population-weighted
#' greenspace fraction or human exposure to greenspace using Meta's global canopy
#' height map (Tolan et al., 2024). (The default is FALSE)
#' @param pop_out logical. Whether return population layer.
#'
#' @seealso [get_gsdc_data()], [get_ndvi_data()]
#'
#' @return SpatRaster or sf. A `SpatRaster` (if `grid_size` is `NULL`) with
#' layers `pwgf_*`, or an `sf` object with columns `pwge_*` representing
#' population-weighted greenspace exposure values aggregated to each grid polygon.
#'
#' @details
#' When `type = "ndvi"`, the hard threshold of NDVI values (`"threshold"`)
#' will be used to classify the rsaster into vegetation and non-vegetation categories
#' for each cell.
#' This function implements the population-weighted greenspace exposure (PWGE) model:
#'
#' \enumerate{
#'   \item Start with a population raster. Each pixel \( i \) has a population value \( P_i \).
#'   \item Create a circular buffer of radius \( d \) around each pixel center.
#'   \item For each buffer, calculate greenspace fraction:
#'     \deqn{G_i^d = \frac{\text{Area of greenspace within buffer}}{\text{Total buffer area}}}
#'   \item Repeat for all \( i = 1, 2, ..., N \) grid cells.
#'   \item Compute overall exposure:
#'     \deqn{GE^d = \frac{\sum_i P_i \cdot G_i^d}{\sum_i P_i}}
#' }
#'
#' @references
#' Chen, B., Wu, S., Song, Y. et al. Contrasting inequality in human exposure to
#' greenspace between cities of Global North and Global South. Nat Commun 13,
#' 4636 (2022). https://doi.org/10.1038/s41467-022-32258-4
#'
#' Pesaresi, M., Schiavina, M., Politis, P., Freire, S., Krasnodębska, K.,
#' Uhl, J. H., … Kemper, T. (2024). Advances on the Global Human Settlement
#' Layer by joint assessment of Earth Observation and population survey data.
#' International Journal of Digital Earth, 17(1).
#' https://doi.org/10.1080/17538947.2024.2390454
#'
#' Tolan, J., Yang, H. I., Nosarzewski, B., Couairon, G., Vo, H. V., Brandt,
#' J., ... & Couprie, C. (2024). Very high resolution canopy height maps from
#' RGB imagery using self-supervised vision transformer and convolutional
#' decoder trained on aerial lidar. Remote Sensing of Environment, 300, 113888.
#'
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' pwgf <- compute_exposure(
#'   # r = sample_data,
#'   source = 'gsdc',
#'   pop_year = 2020,
#'   radius = 1500
#' )
#'
#' @importFrom terra lapp buffer as.points zonal crs res as.polygons
#' @importFrom terra rasterize names set.names ifel ext centroids project
#' @importFrom utils unzip
#' @export
compute_exposure <- function(r = NULL,
                    inputtype = 'fb',
                    res = c(10,10),
                    threshold = c(0.2, 0.2, 0.2),
                    pop_year = 2020,
                    radius = 500,
                    grid_size = NULL,
                    height = FALSE,
                    pop_out = FALSE) {
  if (is.null(r)) {
    return(NULL)
  }

  bbox <- sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(r))))
  sf::st_crs(bbox) <- 4326
  bbox <- sf::st_transform(bbox, crs = 4326)

  # download population layer
  pop <- download_GHSL(bbox, pop_year)
  pop <- terra::crop(pop, terra::vect(bbox))

  cli::cli_alert_info('Computing greenspace area')
  # compute greenspace area
  if (inputtype == 'ndvi') {
    # Approximate binary greenspace indicator
    for (ind in 1:length(threshold)) {
      r[[ind]] <- terra::ifel(r[[ind]] > threshold[ind], 1, 0)
    }
  }

  # calculate greenspace area
  r <- r * res[1] * res[2]

  # download chm and compute average height in each cell
  if (height) {
    cli::cli_alert_info('Downloading canopy height data ...')
    bbox_vct <- as.vector(sf::st_bbox(bbox))
    chm <- suppressMessages(
      dsmSearch::get_dsm_30(bbox = bbox_vct,
                            datatype = 'metaCHM')
    )
    chm <- terra::project(chm, 'EPSG:4326', method = 'near')
    avg_chm <- terra::resample(x = chm, y = r, method = 'average')
    r <- r * avg_chm
  }

  cli::cli_alert_info('Calculating the average greenspace fraction')
  # Within the buffer, calculate the average greenspace fraction
  # Area of greenspace within buffer / Total area of buffer
  pop_pts <- terra::as.points(pop, values = TRUE)
  buffers <- terra::buffer(pop_pts, width = radius + 50)
  buffer_area <- pi * (radius + 50)^2
  zonal_buffers <- terra::zonal(x = r, z = buffers, fun = "sum", as.polygons = TRUE)
  n_name <- length(terra::names(zonal_buffers))
  terra::set.names(zonal_buffers,
                   c('population', paste('g_area_', 1:(n_name - 1), sep = "")))
  zonal_buffers_sf <- sf::st_as_sf(terra::centroids(zonal_buffers))

  cli::cli_alert_info('Weighted based on population')
  # population-weighted
  buffer_green_fraction <- zonal_buffers_sf[, -c(1)]
  for (i in 1:(ncol(buffer_green_fraction) - 1)) {
    buffer_green_fraction[, i] <- buffer_green_fraction[[i]] / buffer_area * zonal_buffers_sf[[1]]
  }

  # to raster
  buffer_green_fraction_vect <- terra::vect(sf::st_as_sf(buffer_green_fraction))
  temp_raster <- terra::rast(terra::ext(pop),
                             resolution = terra::res(pop),
                             crs = terra::crs(pop),
                             nlyrs = n_name - 1)
  layer_names <- names(buffer_green_fraction_vect)
  raster_list <- lapply(layer_names, function(colname) {
    temp_r <- terra::rasterize(buffer_green_fraction_vect,
                               temp_raster,
                               field = colname)
    terra::set.names(temp_r, colname)
    return(temp_r)
  })
  r_stack <- terra::rast(raster_list)

  if (is.null(grid_size)) {
    cli::cli_alert_info('Return the population-weighted greenspace fraction (pwgf)')
    # return the population-weighted greenspace fraction
    out <- r_stack
    terra::set.names(out, paste('pwgf_', 1:(n_name - 1), sep = ""))
    return(out)
  } else {
    cli::cli_alert_info('Computing population-weighted greenspace exposure (pwge)')
    # generate a grid over layers
    r_proj <- terra::project(temp_raster, "EPSG:3857")
    template <- terra::rast(terra::ext(r_proj),
                            resolution = grid_size,
                            crs = terra::crs(r_proj))
    grid <- terra::as.polygons(template)
    # grid$id <- 1:nrow(grid)
    grid_wgs84 <- terra::project(grid, "EPSG:4326")

    # zonal statistics
    sum_pop <- terra::zonal(x = pop, z = grid_wgs84, fun = "sum",
                            as.polygons = TRUE, na.rm = TRUE)
    sum_pwgf <- terra::zonal(x = r_stack, z = grid_wgs84, fun = "sum",
                             as.polygons = TRUE, na.rm = TRUE)
    terra::set.names(sum_pop, 'population')
    terra::set.names(sum_pwgf, paste('pwge_', 1:(n_name - 1), sep = ""))
    sum_pop_sf <- sf::st_as_sf(sum_pop)
    sum_pwgf_sf <- sf::st_as_sf(sum_pwgf)
    sum_pop_sf$population[sum_pop_sf$population < 1e-6] <- 1.0000001
    for (i in 1:(ncol(sum_pwgf_sf)-1)) {
      sum_pwgf_sf[, i] <- sum_pwgf_sf[[i]] / sum_pop_sf[[1]]
    }
    return(sum_pwgf_sf)
  }
}

#' ndvi_to_sem
#' @description
#' Convert ndvi raster data into semantic vegetation areas
#' @param r A SpatRaster with single greenspace layer, typically
#' the output from [get_esa_wc()], or [get_s2a_ndvi()].
#' @param threshold numeric vector of two. Thresholds, defaulting to `c(0.2, 0.5)`,
#' for classify two types of vegetation areas according to Hashim et al. (2019):
#' (1) Non-vegetation (Development and bare land): NDVI values generally below `0.2`.
#' (2) Low vegetation (Shrub and grassland): NDVI values generally between `0.2` and `0.5`.
#' (2) High vegetation (Temperate and Tropical urban forest ): NDVI values generally
#' between `0.5` and `1.0`.
#'
#' @return SpatRaster
#'
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' seg <- ndvi_to_sem(sample_data$`25_NDVI`, threshold = c(0.2, 0.6))
#'
#' @references
#' Hashim, H., Abd Latif, Z., & Adnan, N. A. (2019). Urban vegetation classification
#' with NDVI threshold value method with very high resolution (VHR) Pleiades imagery.
#' The International Archives of the Photogrammetry, Remote Sensing and Spatial
#' Information Sciences, 42, 237-240.
#'
#' @export
ndvi_to_sem <- function(r, threshold = c(0.2, 0.5)) {
  if (is.null(r)) {
    return(NULL)
  }
  r <- terra::ifel(r > threshold[1], r, 0)
  r <- terra::ifel(r >= threshold[1] & r <= threshold[2], 10, r)
  r <- terra::ifel(r > threshold[2] & r <= 1, 20, r)
  r <- r/10
  return(r)
}

#' compute_morphology
#' @description
#' Compute greenspace morphology metrics at patch or grid level,
#' including average size (AREA_MN), fragmentation (PD), connectedness (COHESION),
#' aggregation (AI), and complexity of the shape (SHAPE_AM), related to public health
#' (Wang et al., 2024)
#' @param p SpatRaster or list of SpatRaster.
#' @param directions numeric. The number of directions in which patches should be
#' connected: 4 (default) or 8.
#' @references
#' Wang, H., & Tassinary, L. G. (2024). Association between greenspace morphology
#' and prevalence of non-communicable diseases mediated by air pollution and physical
#' activity. Landscape and Urban Planning, 242, 104934.
#'
#' @importFrom landscapemetrics lsm_p_perim lsm_p_area
#' @export
compute_morphology <- function(p = NULL, directions = 4, grid_size = NULL) {
  # generate a grid over patches
  if (!is.null(grid_size)) {
    template <- terra::rast(terra::ext(p[[1]]),
                            resolution = grid_size,
                            crs = terra::crs(r_proj))
    grid <- terra::as.polygons(template)
    grid_wgs84 <- terra::project(grid, "EPSG:4326")
  }

  bbox <- sf::st_as_sfc(sf::st_bbox(as.vector(terra::ext(p[[1]]))))
  sf::st_crs(bbox) <- 4326
  bbox <- sf::st_transform(bbox, crs = 4326)

  # # compute average height for each patch
  # cli::cli_alert_info('Downloading canopy height data ...')
  # bbox_vct <- as.vector(sf::st_bbox(bbox))
  # chm <- suppressMessages(
  #   dsmSearch::get_dsm_30(bbox = bbox_vct,
  #                         datatype = 'metaCHM')
  # )
  # chm <- terra::project(chm, 'EPSG:4326', method = 'near')
  # avg_chm <- terra::resample(x = chm, y = p, method = 'average')

  PERIM <- landscapemetrics::lsm_p_perim(p, directions = 4)
  AREA <- landscapemetrics::lsm_p_area(p, directions = 4)

}
