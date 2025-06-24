#' Human Exposure to Greenspace
#' @description
#' Computes population-weighted greenspace fraction or human exposure to
#' greenspace based on a population-weighted exposure model (Chen et al., 2022),
#' using population data from the Global Human Settlement Layer (GHSL;
#' Pesaresi et al., 2024).
#' See **Details** for the underlying method and assumptions.
#'
#' @param r A SpatRaster with single/multiple greenspace layer(s), typically
#' the output from [get_gsdc_data()] or [get_ndvi_data()].
#' @param source character. Data source for greenspace. Must be either `"gsdc"` (default)
#' or `"esa"`. If `"esa"`, NDVI percentiles are used to approximate greenspace fraction.
#' @param res numeric vector of length 2. The actual spatial resolution (in meters).
#' Default is `c(10, 10)`.
#' @param weights numeric vectorof length 3. Weights for combining NDVI percentiles
#' to estimate greenspace fraction, defaulting to `c(0.75, 0.2, 0.05)`
#' for p90, p50, and p10.
#' @param pop_year numeric. Year of the GHSL dataset to use.
#' Must be one of: 2015, 2020, 2025, or 2030. Default is 2020.
#' @param radius numeric. Buffer radius (in meters) used for local averaging.
#' Default is `500`.
#' @param grid_size numeric. Optional. If provided, output is aggregated to grid cells of
#' this size (in meters) and returned as an `sf` object.
#'
#' @seealso [get_gsdc_data()], [get_ndvi_data()]
#'
#' @return SpatRaster or sf. A `SpatRaster` (if `grid_size` is `NULL`) with
#' layers `pwgf_*`, or an `sf` object with columns `pwge_*` representing
#' population-weighted greenspace exposure values aggregated to each grid polygon.
#'
#' @details
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
#' When `source = "esa"`, greenspace fraction \( G_i \) for pixel \( i \) is estimated
#' from NDVI percentiles as:
#'
#' \deqn{
#' G_i = \min\left( \max\left( w_1 \cdot \text{NDVI}_{p90} + w_2 \cdot \text{NDVI}_{p50} + w_3 \cdot \text{NDVI}_{p10},\ 0 \right),\ 1 \right)
#' }
#'
#' where \( w_1, w_2, w_3 \) are the user-defined weights (default: 0.75, 0.2, 0.05).
#' This gives more weight to persistent greenness (p90) and mid-season vegetation (p50),
#' and less to transient low values (p10). Output is bounded within [0, 1].
#'
#' The greenspace fraction \eqn{G_i} for each pixel \eqn{i} is estimated by
#' combining the NDVI percentiles using a weighted linear model:
#'
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
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' pwgf <- pop_weg(
#'   # r = sample_data,
#'   source = 'gsdc',
#'   pop_year = 2020,
#'   radius = 1500
#' )
#'
#'
#' @importFrom terra lapp buffer as.points zonal crs res as.polygons
#' @importFrom terra rasterize names set.names
#' @importFrom utils unzip
#' @export
pop_weg <- function(r = NULL,
                      source = 'gsdc',
                      res = c(10,10),
                      weights = c( 0.75, 0.2, 0.05),
                      pop_year = 2020,
                      radius = 500,
                      grid_size = NULL) {
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
  if (source == 'esa') {
    greenspace_fraction_raster <- terra::lapp(r, fun = function(p90, p50, p10) {
      # Weighted combination with emphasis on median and peak greenness
      gs <- weights[1] * p90 + weights[2] * p50 + weights[3] * p10
      gs[gs < 0] <- 0
      return(pmin(pmax(gs, 0), 1))  # constrain between 0 and 1
    })
    r <- greenspace_fraction_raster * res[1] * res[2]
  } else if (source == 'gsdc') {
    r <- r * res[1] * res[2]
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
    terra:set.names(out, paste('pwgf_', 1:(n_name - 1), sep = ""))
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

