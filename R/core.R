#' @title Download Greenspace Seasonality Data Cube
#' @name get_gsdc
#'
#' @description download Greenspace Seasonality Data Cube for an urban area.
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
#' @param UID numeric. Urban area ID. To check the ID of an available urban area,
#' use [check_available_urban()]
#' @param year numeric. (required) The year of interest.
#' @param time Character vector of length 2. (optional) Start and end dates in `"MM-DD"` format
#' (e.g., `c("03-20", "10-15")`). Used to subset the 10-day interval data cube by time.
#' @param mask logical (optional). Default is `FALSE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place` if it is specified.
#'
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
#' Use [check_available_urban()] and [check_urban_boundary()] to see supported
#' cities and their boundaries.
#'
#' @examples
#' result <- get_gsdc(UID = 0,
#'                    # year = 2022
#'                   )
#'
#' @importFrom sf st_sfc st_transform st_bbox st_as_sfc st_point
#' @importFrom nominatimlite geo_lite_sf
#' @importFrom terra mask crop vect
#' @export
get_gsdc <- function(bbox = NULL, place = NULL, location = NULL, UID = NULL,
                       year = NULL, time = NULL, mask = FALSE) {
  if (inherits(year, 'NULL')) {
    cli::cli_alert_info("`year` is missing.")
    return(NULL)
  }

  if (inherits(bbox, 'NULL') && inherits(place, 'NULL') && inherits(location, 'NULL') && inherits(UID, 'NULL')) {
    cli::cli_alert_info('Area/point of interest is missing.')
    return(NULL)
  }

  if (!as.numeric(year) %in% c(2019, 2020, 2021, 2022)) {
    stop("`year` has to be 2019, 2020, 2021, or 2022")
  }

  start_time <- Sys.time()
  urls <- NULL

  # find the city with a corresponding uid
  if(!inherits(UID, 'NULL')) {
    urls <- get_data_with_uid(UID, year)
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
      }
    }
    bbox <- sf::st_transform(bbox, 4326)
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

#' @title Download NDVI Data from ESA WorldCover 10m Annual Composites Dataset
#' @name get_esa_ndvi
#'
#' @description download 3-band NDVI Data (NDVI p90, NDVI p50, NDVI p10).
#' Users can define an area of interest using a bounding box or place name.
#'
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param year numeric. The year of interest: `2020` or `2021`. The default is `2021`.
#' @param mask logical (optional). Default is `TRUE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place`.
#'
#' @return A `SpatRaster` object containing NDVI yearly percentiles composite
#' (NDVI p90, NDVI p50, NDVI p10)
#'
#' @examples
#' result <- get_ndvi_data(
#'   # place = 'New York'
#' )
#'
#' @references
#' Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N.,
#' Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud, S.,
#' Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S.,
#' Herold, M., Li, L., Tsendbazar, N.-E., … Arino, O. (2021).
#' ESA WorldCover 10 m 2020 v100 (Version v100).
#' Zenodo. https://doi.org/10.5281/zenodo.5571936
#'
#' Zanaga, D., Van De Kerchove, R., Daems, D., De Keersmaecker, W., Brockmann,
#' C., Kirches, G., Wevers, J., Cartus, O., Santoro, M., Fritz, S., Lesiv, M.,
#' Herold, M., Tsendbazar, N.-E., Xu, P., Ramoino, F., & Arino, O. (2022).
#' ESA WorldCover 10 m 2021 v200 (Version v200).
#' Zenodo. https://doi.org/10.5281/zenodo.7254221
#' @importFrom aws.s3 get_bucket save_object
#' @export
get_esa_ndvi <- function(bbox = NULL, place = NULL, year = 2021, mask = TRUE) {
  if (!as.numeric(year) %in% c(2020, 2021)) {
    stop("`year` has to be 2020 or 2021")
  }

  start_time <- Sys.time()

  if (!inherits(bbox, 'NULL') || !inherits(place, 'NULL')) {
    if (!inherits(place, 'NULL')) {
      pla <- suppressWarnings(nominatimlite::geo_lite_sf(place, points_only = FALSE))
      pla <- sf::st_transform(pla, crs = 4326)
      bbox <- pla$geometry
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
      }
    }
  } else {
    return(NULL)
  }
  bbox <- sf::st_transform(bbox, 4326)
  bbox_coords <- sf::st_bbox(bbox)

  # List ESA Tile Names by bbox
  tiles <- get_esa_tile_names(
    lat_min = bbox_coords["ymin"], lat_max = bbox_coords["ymax"],
    lon_min = bbox_coords["xmin"], lon_max = bbox_coords["xmax"]
  )

  # get tiles
  keys <- c()
  for (i in 1:length(tiles)) {
    t <- tiles[i]
    f <- aws.s3::get_bucket(
      bucket = "esa-worldcover-s2",
      region = "eu-central-1",
      prefix = paste0('ndvi/',
                      year, '/',
                      base::strsplit(t, "W")[[1]][1],
                      '/ESA_WorldCover_10m_',
                      year, '_v200_', t, '_NDVI'),
      max = Inf
    )

    f <- tibble::tibble(
      key = vapply(f, function(x) x[["Key"]], character(1)),
    )

    keys <- c(keys, as.character(f$key))
  }

  # download data
  result_list <- list()
  temp_paths <- c()
  original_timeout <- getOption('timeout')
  options(timeout=9999)
  on.exit({
    options(timeout = original_timeout)
    unlink(temp_paths, recursive = TRUE)
  }, add = TRUE)
  cli::cli_alert_info('Start downloading data ...')
  for (i in 1:length(keys)) {
    k <- keys[i]
    temp_tif <- tempfile(fileext = ".tif")
    aws.s3::save_object(k,
                        bucket = "esa-worldcover-s2",
                        region = "eu-central-1",
                        file = temp_tif)
    rast_data <- terra::rast(temp_tif)
    result_list[[length(result_list) + 1]] <- rast_data
    temp_paths <- c(temp_paths, temp_tif)
  }
  cli::cli_alert_success('Finished downloading data')

  # merge if there are multiple tiles
  if (length(result_list) == 1) {
    ndvi_data <- result_list[[1]]
  } else {
    cli::cli_alert_info('Merging multiple tiles ...')
    ndvi_data <- do.call(terra::merge, result_list)
  }

  # crop the raster
  if (mask) {
    cli::cli_alert_info('Masking and cropping data ...')
    if (!inherits(bbox, "SpatVector")) {
      bbox_vect <- terra::vect(bbox)
    } else {
      bbox_vect <- bbox
    }
    ndvi_data <- terra::mask(ndvi_data, bbox_vect)
    ndvi_data <- terra::crop(ndvi_data, bbox_vect)
  }
  names(ndvi_data) <- c("NDVI_p90", "NDVI_p50", "NDVI_p10")
  cli::cli_alert_success("Data successfully processed.")
  report_time(start_time)
  return(ndvi_data)
}

#' @title Retrieve Sentinel-2-l2a images to compute NDVI
#' @name get_s2a_ndvi
#' @description download Sentinel-2-l2a imagery data and compute NDVI.
#' Users can define an area of interest using a bounding box or place name.
#'
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param datetime numeric vector of 2. The time of interest such as
#' `c("2020-08-01", "2020-09-01")`.
#' @param cloud_cover numeric. The percentage of cloud coverage.
#' @param vege_perc numeric. The percentage of cloud coverage.
#' @param select character. one of "latest", "earliest", "all". The default
#' is "latest".
#' @param method character. A method for mosaicing layers: one of "mean",
#' "median", "min", "max", "modal", "sum", "first", "last".
#' @param mask logical (optional). Default is `TRUE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place`.
#'
#' @return
#' A `SpatRaster` object containing (multiple) NDVI layer(s) (for different
#' period of time) `select = "latest"` or `select = "first"`
#' (or if `mask = TRUE` and `select = "all"`)
#'
#' A `List` of NDVI rasters if `mask = FALSE` and `select = "all"`.
#'
#' @examples
#' result <- get_s2a_ndvi(
#'   # place = 'New York',
#'   datetime = c("2020-08-01", "2020-09-01")
#' )
#' @export
get_s2a_ndvi <- function(bbox = NULL, place = NULL, datetime = c(),
                         cloud_cover = 10, vege_perc = 0, select = "latest",
                         method = 'first', mask = TRUE) {
  start_time <- Sys.time()

  if (!inherits(bbox, 'NULL') || !inherits(place, 'NULL')) {
    if (!inherits(place, 'NULL')) {
      pla <- suppressWarnings(nominatimlite::geo_lite_sf(place, points_only = FALSE))
      pla <- sf::st_transform(pla, crs = 4326)
      bbox <- pla$geometry
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
      }
    }
  } else {
    return(NULL)
  }
  bbox <- sf::st_transform(bbox, 4326)

  if (length(datetime) < 1) {
    stop("missing `datetime`")
  }

  cli::cli_alert_info('Start downloading data ...')
  feafures <- download_sentinel(bbox, datetime[1], datetime[2],
                                cloud_cover = cloud_cover, vege_perc = vege_perc)

  dates <- c()
  for (i in 1:length(features)) {
    dates <- c(dates, strsplit(features[[i]]$properties$datetime, split = "T")[1])
  }
  ndvi_list <- list()
  dates <- unique(dates)
  for (d in dates) {
    ndvi_list[[d]] <- list()
  }

  cli::cli_alert_info('Importing B4 and B8 bads for each period ...')
  for (i in 1:length(features)) {
    this_date <- strsplit(features[[i]]$properties$datetime, split = "T")[1]
    signed_item <- rstac::sign_planetary_computer()(features[[i]])
    b4_url <- signed_item$assets$B04$href
    b8_url <- signed_item$assets$B08$href
    b04_rast <- terra::rast(b04_url)
    b08_rast <- terra::rast(b08_url)
    ndvi <- compute_ndvi(b04_rast, b08_rast)
    ndvi_list[[this_date]][[length(ndvi_list[[this_date]])+1]] <- ndvi
  }
  cli::cli_alert_info(if (mask) 'Mosaicing, masking and cropping ...' else 'Mosaicing ')
  if (mask) {bbox_vect <- terra::vect(bbox)}
  for (d in dates) {
    ndvi_collection <- terra::sprc(ndvi_list[[d]])
    ndvi_mosaic <- terra::mosaic(ndvi_collection, fun = method)
    ndvi_list[[d]] <- terra::project(ndvi_mosaic, 'EPSG:4326', method = 'near')
    if (mask) {
      ndvi_list[[d]] <- terra::mask(ndvi_list[[d]], bbox_vect)
      ndvi_list[[d]] <- terra::crop(ndvi_list[[d]], bbox_vect)
    }
  }
  if (mask) {
    ndvi_list <- terra::rast(ndvi_list)
  }
  cli::cli_alert_success("Data successfully processed.")
  report_time(start_time)
  return(ndvi_list)
}


#' @title Sample Greenspace Values from Greenspace Seasonality Data Cube or
#' ESA WorldCover 10m Annual Composites Dataset
#' @name sample_values
#'
#' @description Samples values by locatoins from the Greenspace Seasonality Data Cube
#' developed by Wu et al. (2024) or ESA WorldCover 10m Annual Composites Dataset
#' by Zanaga et al. (2021).
#'
#' @param samples A list, matrix, `data.frame`, or `sf` object of point locations.
#' Can be a list of length-2 numeric vectors (`list(c(lon, lat))`),
#' a 2-column matrix or data.frame, or an `sf` object with POINT geometry in any CRS.
#' @param year numeric. The year of interest. See Detail.
#' @param source character. The data source for extracting greenspace values:
#' `gsdc` for Greenspace Seasonality Data Cube and `esa` for ESA WorldCover 10m
#' Annual Composites Dataset. The default is `gsdc`.
#'
#' @return A `data.frame` containing greenspace values extracted at each point
#' across all bands. Each row corresponds to a sample location;
#' columns represent band values.
#'
#' @details
#' `year`: For the greenspace seasonality data cube, only years from 2019 to 2022
#'  are availabe. For ESA WorldCover 10m Annual Composites Dataset, only 2020
#'  and 2021 are available.
#'
#' @note
#' For sampling data from Greenspace Seasonality Data Cube `samples` must be
#' located within the same boundary of an available city in the data cube.
#' Use [check_available_urban()] and [check_urban_boundary()] to see supported
#' cities and their boundaries.
#'
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
#' Zanaga, D., Van De Kerchove, R., De Keersmaecker, W., Souverijns, N.,
#' Brockmann, C., Quast, R., Wevers, J., Grosu, A., Paccini, A., Vergnaud, S.,
#' Cartus, O., Santoro, M., Fritz, S., Georgieva, I., Lesiv, M., Carter, S.,
#' Herold, M., Li, L., Tsendbazar, N.-E., … Arino, O. (2021).
#' ESA WorldCover 10 m 2020 v100 (Version v100).
#' Zenodo. https://doi.org/10.5281/zenodo.5571936
#'
#' Zanaga, D., Van De Kerchove, R., Daems, D., De Keersmaecker, W., Brockmann,
#' C., Kirches, G., Wevers, J., Cartus, O., Santoro, M., Fritz, S., Lesiv, M.,
#' Herold, M., Tsendbazar, N.-E., Xu, P., Ramoino, F., & Arino, O. (2022).
#' ESA WorldCover 10 m 2021 v200 (Version v200).
#' Zenodo. https://doi.org/10.5281/zenodo.7254221
#'
#' @importFrom sf st_drop_geometry
#' @importFrom terra extract vect
#' @export
sample_values <- function(samples = NULL, year = NULL, source = 'gsdc') {
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
  if (source == 'gsdc') {
    raster_data <- get_gsdc(bbox = bbox, year = year)
  } else if (source == 'esa') {
    raster_data <- get_esa_ndvi(bbox = bbox, year = year)
  }

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



