#' @title Download Greenspace Seasonality Data Cube
#' @name get_gsds_data
#' @description download Greenspace Seasonality Data Cube for a city.
#' Retrieves high-resolution greenspace seasonality data from the Sentinel-2-based
#' global dataset developed by Wu et al. (2024). Users can define a city of interest
#' using a bounding box, place name, coordinates, or unique city ID (UID).
#'
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place`, `location`, or `UID` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")). This can be ignored if `location` is specified.
#' @param location vector or sf point. A point of interest.
#' Ignored if `UID` is specified.
#' @param UID numeric. City ID. To check the ID of an available city,
#' use [check_available_urban()]
#' @param year numeric. (required) The year of interest.
#' @param time Character vector of length 2. (optional) Start and end dates in `"MM-DD"` format
#' (e.g., `c("03-20", "10-15")`). Used to subset the 10-day interval data cube by time.
#' @param mask logical (optional). Default is `FALSE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place` if it is specified.
#' @return A `SpatRaster` object containing the greenspace seasonality data.
#'
#' @details
#' The Greenspace Data Cube is organized into 36 bands per year,
#' each representing a 10-day interval.
#'
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#'
#' @note
#' Use [check_available_cities()] and [check_city_boundary()] to see supported
#' cities and their boundaries.
#'
#' @examples
#' result <- get_gsds_data(UID = 0,
#'                         year = 2022
#'                        )
#'
#' @importFrom sf st_sfc st_transform st_bbox st_as_sfc st_point
#' @importFrom nominatimlite geo_lite_sf
#' @importFrom terra mask crop vect
#' @export
get_gsds_data <- function(bbox = NULL, place = NULL, location = NULL, UID = NULL,
                       year = NULL, time = NULL, mask = FALSE) {
  if (inherits(year, 'NULL')) {
    cli::cli_alert_info("`year` is missing.")
    return(NULL)
  }

  if (inherits(bbox, 'NULL') && inherits(place, 'NULL') && inherits(location, 'NULL') && inherits(UID, 'NULL')) {
    base::warning('Area/point of interest is missing.')
    return(NULL)
  }

  start_time <- Sys.time()
  urls <- NULL

  # find the city with a corresponding uid
  if(!inherits(UID, 'NULL')) {
    urls <- get_data_with_uid(uid, year)
    greenspace <- download_data(urls)
    if (!is.null(time)) {
      start_band_index <- get_band_index_by_time(time[1], year)
      end_band_index <- get_band_index_by_time(time[2], year)
      greenspace <- greenspace[[start_band_index:end_band_index]]
    }
    report_time(start_time)
    return(greenspace)
  }

  # find intersected area with a spatial point
  if (!inherits(location, 'NULL')) {
    # check type of point
    if (is.numeric(location) && length(location) == 2){
      location <- sf::st_sfc(sf::st_point(location), crs = 4326)
    }else {
      location <- sf::st_transform(location, crs = 4326)
    }
    # find the UID of overlapped city
    uid <- check_overlap(location)
    greenspace <- download_data(urls)
    if (!is.null(time)) {
      start_band_index <- get_band_index_by_time(time[1], year)
      end_band_index <- get_band_index_by_time(time[2], year)
      greenspace <- greenspace[[start_band_index:end_band_index]]
    }
    report_time(start_time)
    return(greenspace)
  }

  if (!inherits(bbox, 'NULL') || !inherits(place, 'NULL')) {
    if (!inherits(place, 'NULL')) {
      city <- suppressWarnings(nominatimlite::geo_lite_sf(place, points_only = FALSE))
      city <- sf::st_transform(city, crs = 4326)
      bbox <- city$geometry
    } else if (!inherits(bbox, 'NULL')) {
      if (is.numeric(bbox) && length(bbox) == 4) {
        bbox <- sf::st_as_sfc(
          sf::st_bbox(
            c(xmin = bbox[1],
              ymin = bbox[2],
              xmax = bbox[3],
              ymax = bbox[4]),
            crs = 4326
          )
        )
        bbox <- sf::st_transform(bbox, 4326)
      }
    }
    location <- sf::st_centroid(bbox)
    uid <- check_overlap(location)
    urls <- get_data_with_uid(id = as.numeric(uid), y = year)
    greenspace <- download_data(urls)

    if (!is.null(time)) {
      start_band_index <- get_band_index_by_time(time[1], year)
      end_band_index <- get_band_index_by_time(time[2], year)
      greenspace <- greenspace[[start_band_index:end_band_index]]
    }

    if (mask) {
      if (!inherits(bbox, "SpatVector")) {
        bbox_vect <- terra::vect(bbox)
      } else {
        bbox_vect <- bbox
      }
      greenspace <- terra::mask(greenspace, bbox_vect)
      greenspace <- terra::crop(greenspace, bbox_vect)
    }
    report_time(start_time)
    return(greenspace)
  }

  if (inherits(urls, 'NULL')) {
    base::warning("No urban areas intersect with the area/point of interest.")
    return(NULL)
  }
}

#' @title Sample Greenspace Values from Data Cube
#' @name sample_values
#' @description Samples values by locatoins from the Greenspace Seasonality Data Cube
#' global dataset developed by Wu et al. (2024).
#' @param samples A list, matrix, `data.frame`, or `sf` object of point locations.
#' Can be a list of length-2 numeric vectors (`list(c(lon, lat))`),
#' a 2-column matrix or data.frame, or an `sf` object with POINT geometry in any CRS.
#' @param year numeric. The year of interest for the greenspace seasonality data cube.
#' @return A `data.frame` containing greenspace values extracted at each point
#' across all 36 bands. Each row corresponds to a sample location;
#' columns represent band values.
#' @note
#' `samples` must be located within the same boundary of an available city in the data cube.
#'  Use [check_available_urban()] and [check_urban_boundary()] to see supported
#'  cities and their boundaries.
#' @examples
#' # see supported urban areas and their boundaries
#' check_available_urban()
#' boundary <- check_urban_boundary(uid = 11)
#'
#' # sample locations with in the boundary
#' samples <- sf::st_sample(boundary, size = 20)
#'
#' # extract values
#' gs_samples <- sample_values(samples,
#'                             # year = 2022
#'                            )
#'
#' @details
#' The Greenspace Data Cube is organized into 36 bands per year,
#' each representing a 10-day interval.
#'
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#'
#' @importFrom sf st_drop_geometry
#' @importFrom terra extract vect
#' @export
sample_values <- function(samples = NULL, year = NULL) {
  if (is.null(year)) {
    cli::cli_alert_info("`year` is missing.")
    return(NULL)
  }

  # Convert samples to sf POINTs
  if (inherits(samples, "sf")) {
    sf_points <- sf::st_transform(samples, 4326)
  } else if (is.list(samples)) {
    coords <- do.call(rbind, samples)
    sf_points <- sf::st_as_sf(data.frame(x = coords[,1], y = coords[,2]),
                              coords = c("x", "y"), crs = 4326)
  } else if (is.matrix(samples) || is.data.frame(samples)) {
    if (ncol(samples) != 2) stop("`samples` must have two columns: lon and lat.")
    sf_points <- sf::st_as_sf(data.frame(x = samples[,1], y = samples[,2]),
                              coords = c("x", "y"), crs = 4326)
  } else {
    stop("`samples` must be a list, matrix, data.frame, or sf POINT object.")
  }

  # Get bounding box and pad slightly to ensure coverage
  bbox <- as.numeric(sf::st_bbox(sf_points)) + c(-0.01, -0.01, 0.01, 0.01)

  # Retrieve raster data
  raster_data <- get_gsds_data(bbox = bbox, year = year)

  if (is.null(raster_data)) {
    cli::cli_alert_warning("No raster data found for the specified location/year.")
    return(NULL)
  }

  # Extract values at point locations
  values <- terra::extract(raster_data, terra::vect(sf_points))

  # Combine with coordinates (omit ID column)
  result <- sf::st_drop_geometry(sf_points)
  result <- cbind(result, values[,-1])

  return(result)
}



