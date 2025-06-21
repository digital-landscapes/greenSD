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
#' use [check_available_cities()]
#' @param year numeric. (required) The year of interest.
#' @param time Character vector of length 2. (optional) Start and end dates in `"MM-DD"` format
#' (e.g., `c("03-20", "10-15")`). Used to subset the 10-day interval data cube by time.
#' @param mask logical (optional). Default is `FALSE`. If `TRUE`, masks the
#' raster data using the given `bbox` if it is specified.
#' @return A `SpatRaster` object containing the greenspace seasonality data.
#'
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#'
#' @examples
#' result <- get_city_data(UID = 0,
#'                         year = 2022
#'                        )
#'
#' @export
get_city_data <- function(bbox = NULL, place = NULL, location = NULL, UID = NULL,
                       year = NULL, time = NULL, mask = NULL) {
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
    if ((inherits(bbox, 'NULL') && !inherits(place, 'NULL')) || (!inherits(bbox, 'NULL') && !inherits(place, 'NULL'))) {
      city <- nominatimlite::geo_lite_sf(place, points_only = FALSE)
      city <- sf::st_transform(city, crs = 4326)
      bbox <- city$geometry
    } else if (!inherits(bbox, 'NULL') && inherits(place, 'NULL') ) {
      # check type of bbox
      if (is.numeric(bbox) && length(bbox) == 4) {
        # convert bbox to sf when it is not a sf polygon
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
    urls <- get_data_with_uid(uid, year)
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
    }
    report_time(start_time)
    return(greenspace)
  }

  if (inherits(urls, 'NULL')) {
    base::warning("No urban areas intersect with the area/point of interest.")
    return(NULL)
  }
}

#' @description Samples values from Greenspace Seasonality Data Cube by locatoins
#' @param samples list or matrix. A list of points
#' @return
#' @export
sample_values <- function(samples = NULL, years = NULL) {
  # convert samples in to sf points

  # get multi-band green space raster by the bbox of samples using 'get_city_data()'

  # extract data from multi-band green space raster using  the coordinates of samples

  #
}


