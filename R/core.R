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
#' @param time Character vector of length 2 or character. (optional) Start and end dates in
#' `"MM-DD"` format (e.g., `c("03-20", "10-15")` or `"07-10"`). Used to subset the 10-day
#' interval data cube by time.
#' @param mask logical (optional). Default is `TRUE`. If `TRUE`, masks the
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
                       year = NULL, time = NULL, mask = TRUE) {
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
      if (length(time) != 1) {
        start_band_index <- get_band_index_by_time(time[1], year)
        end_band_index <- get_band_index_by_time(time[2], year)
        greenspace <- greenspace[[start_band_index:end_band_index]]
      } else {
        greenspace <- greenspace[[get_band_index_by_time(time, year)]]
      }
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
      if (length(time) != 1) {
        start_band_index <- get_band_index_by_time(time[1], year)
        end_band_index <- get_band_index_by_time(time[2], year)
        greenspace <- greenspace[[start_band_index:end_band_index]]
      } else {
        greenspace <- greenspace[[get_band_index_by_time(time, year)]]
      }
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
      if (length(time) != 1) {
        start_band_index <- get_band_index_by_time(time[1], year)
        end_band_index <- get_band_index_by_time(time[2], year)
        greenspace <- greenspace[[start_band_index:end_band_index]]
      } else {
        greenspace <- greenspace[[get_band_index_by_time(time, year)]]
      }
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

#' @title Download landcover or NDVI Data from ESA WorldCover
#' 10m Annual Dataset
#' @name get_esa_wc
#'
#' @description download 11-class landcover or 3-band NDVI Data
#' (NDVI p90, NDVI p50, NDVI p10). Users can define an area of interest
#' using a bounding box or place name.
#'
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param datatype character. One of "landcover" and "ndvi".
#' @param year numeric. The year of interest: `2020` or `2021`. The default is `2021`.
#' @param mask logical (optional). Default is `TRUE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place`.
#'
#' @return A `SpatRaster` object containing NDVI yearly percentiles composite
#' (NDVI p90, NDVI p50, NDVI p10)
#'
#' @examples
#' result <- get_esa_wc(
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
#'
#' @importFrom aws.s3 get_bucket save_object
#' @export
get_esa_wc <- function(bbox = NULL, place = NULL,
                       datatype = "landcover",
                       year = 2021, mask = TRUE) {
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
  tiles <- if (datatype == 'landcover') {
    esa_wc_tiles[sf::st_intersects(esa_wc_tiles, bbox, sparse = FALSE), ]$tile
  } else if (datatype == 'ndvi') {
    get_esa_tile_names(
      lat_min = bbox_coords["ymin"], lat_max = bbox_coords["ymax"],
      lon_min = bbox_coords["xmin"], lon_max = bbox_coords["xmax"]
    )
  }

  # get tiles
  keys <- c()
  for (i in 1:length(tiles)) {
    t <- tiles[i]
    if (datatype == 'landcover') {
      f <- paste0(if (year == 2020) "v100/2020/" else "v200/2021/",
                  'map/ESA_WorldCover_10m_',
                  if (year == 2020) '2020_v100_' else '2021_v200_',
                  t, "_Map.tif")
      keys <- c(keys, f)
    } else if (datatype == 'ndvi') {
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
  cli::cli_alert_info(
    paste0("Start downloading ",
           if (datatype == 'landcover') 'land cover ' else 'NDVI ',
           "data ...")
  )
  for (i in 1:length(keys)) {
    k <- keys[i]
    temp_tif <- tempfile(fileext = ".tif")
    if (datatype == 'landcover') {
      aws.s3::save_object(
        object = k,
        bucket = "esa-worldcover",
        region = "eu-central-1",
        file = temp_tif
      )
    } else if (datatype == 'ndvi') {
      aws.s3::save_object(k,
                          bucket = "esa-worldcover-s2",
                          region = "eu-central-1",
                          file = temp_tif)
    }
    rast_data <- terra::rast(temp_tif)
    result_list[[length(result_list) + 1]] <- rast_data
    temp_paths <- c(temp_paths, temp_tif)
  }
  cli::cli_alert_success('Finished downloading data')

  # merge if there are multiple tiles
  if (length(result_list) == 1) {
    out_data <- result_list[[1]]
  } else {
    cli::cli_alert_info('Merging multiple tiles ...')
    out_data <- do.call(terra::merge, result_list)
  }

  # crop the raster
  if (mask) {
    cli::cli_alert_info('Masking and cropping data ...')
    if (!inherits(bbox, "SpatVector")) {
      bbox_vect <- terra::vect(bbox)
    } else {
      bbox_vect <- bbox
    }
    out_data <- terra::mask(out_data, bbox_vect)
    out_data <- terra::crop(out_data, bbox_vect)
  }
  if (datatype == "ndvi") names(out_data) <- c("NDVI_p90", "NDVI_p50", "NDVI_p10")
  cli::cli_alert_success("Data successfully processed.")
  report_time(start_time)
  return(out_data)
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
#' @param cloud_cover numeric. Threshold for the percentage of cloud coverage.
#' Desfault is 10.
#' @param vege_perc numeric. Threshold for the percentage of vegetation coverage.
#' Desfault is 0.
#' @param select character. one of "latest", "earliest", "all". The default
#' is "latest".
#' @param method character. A method for mosaicing layers: one of "mean",
#' "median", "min", "max", "modal", "sum", "first", "last". The default
#' is "first".
#' @param mask logical (optional). Default is `TRUE`. If `TRUE`, masks the
#' raster data using the given `bbox` or `place`.
#' @param output_bands vector. A list of band names (`c('B04', 'B08')`).
#' The default is `NULL`. If `output_bands` is specified, NDVI will not
#' be computed and only the specified bands will be returned.
#' All available bands can be found [here](https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/#available-bands-and-data)
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
                         method = 'first', mask = TRUE, output_bands = NULL) {
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

  if (length(datetime) <= 1) {
    stop("missing `datetime`")
  }

  cli::cli_alert_info('Start downloading data ...')
  features <- download_sentinel(bbox, datetime[1], datetime[2],
                                cloud_cover = cloud_cover, vege_perc = vege_perc)

  dates <- c()
  for (i in 1:length(features)) {
    dates <- c(dates, strsplit(features[[i]]$properties$datetime, split = "T")[[1]][1])
  }
  dates <- unique(dates)

  select_date <- NULL
  ndvi_list <- list()
  band_list <- list()
  bands <- list()
  for (b in output_bands) {
    bands[[b]] <- list()
  }
  for (d in dates) {
    ndvi_list[[d]] <- list()
    band_list[[d]] <- bands
  }
  if (select == "latest" || select == "earliest") {
    select_date <- get_the_date(select, dates)
  }
  cli::cli_alert_info('Importing bands ...')
  for (i in 1:length(features)) {
    this_date <- strsplit(features[[i]]$properties$datetime, split = "T")[[1]][1]
    if (!is.null(select_date) && isTRUE(this_date != select_date)) {
      next
    }
    signed_item <- rstac::sign_planetary_computer()(features[[i]])
    if (!is.null(output_bands)) {
      for (b in output_bands) {
        url <- signed_item$assets[[b]]$href
        temp <- terra::rast(url)
        names(temp) <- b
        band_list[[this_date]][[b]][[length(band_list[[this_date]][[b]])+1]] <- temp
      }
    } else {
      b4_url <- signed_item$assets$B04$href
      b8_url <- signed_item$assets$B08$href
      b04_rast <- terra::rast(b4_url)
      b08_rast <- terra::rast(b8_url)
      ndvi <- compute_ndvi(b04_rast, b08_rast)
      names(ndvi) <- paste0('NDVI_', this_date)
      ndvi_list[[this_date]][[length(ndvi_list[[this_date]])+1]] <- ndvi
    }
  }
  cli::cli_alert_info(if (mask) 'Mosaicing, masking and cropping ...' else 'Mosaicing ...')
  if (mask) {bbox_vect <- terra::vect(bbox)}
  for (d in dates) {
    if (!is.null(select_date) && isTRUE(d != select_date)) {
      next
    }
    if (!is.null(output_bands)) {
      for (b in output_bands) {
        band_collection <- terra::sprc(band_list[[d]][[b]])
        band_mosaic <- terra::mosaic(band_collection, fun = method)
        band_list[[d]][[b]] <- terra::project(band_mosaic, 'EPSG:4326', method = 'bilinear')
        if (mask) {
          band_list[[d]][[b]] <- terra::mask(band_list[[d]][[b]], bbox_vect)
          band_list[[d]][[b]] <- terra::crop(band_list[[d]][[b]], bbox_vect)
        }
      }
      ndvi_list <- band_list
    } else {
      ndvi_collection <- terra::sprc(ndvi_list[[d]])
      ndvi_mosaic <- terra::mosaic(ndvi_collection, fun = method)
      ndvi_list[[d]] <- terra::project(ndvi_mosaic, 'EPSG:4326', method = 'near')
      if (mask) {
        ndvi_list[[d]] <- terra::mask(ndvi_list[[d]], bbox_vect)
        ndvi_list[[d]] <- terra::crop(ndvi_list[[d]], bbox_vect)
      }
    }
  }
  if (!is.null(select_date)) {
    cli::cli_alert_success("Data successfully processed.")
    report_time(start_time)
    return(ndvi_list[[select_date]])
  }
  if (mask) {
    ndvi_list <- terra::rast(ndvi_list)
  }
  cli::cli_alert_success("Data successfully processed.")
  report_time(start_time)
  return(ndvi_list)
}


#' @title Sample greenspace-realted data from Greenspace Seasonality Data Cube,
#' ESA WorldCover 10m Annual Composites Dataset, or Sentinel-2-l2a images.
#' @name sample_values
#'
#' @description Samples values by locatoins from the Greenspace Seasonality Data Cube
#' developed by Wu et al. (2024), ESA WorldCover 10m Annual Composites Dataset
#' by Zanaga et al. (2021), or Sentinel-2-l2a images.
#'
#' @param samples A list, matrix, `data.frame`, or `sf` object of point locations.
#' Can be a list of length-2 numeric vectors (`list(c(lon, lat))`),
#' a 2-column matrix or data.frame, or an `sf` object with POINT geometry in any CRS.
#' @param time numeric or vector. The time of interest. See Detail.
#' @param source character. The data source for extracting greenspace values:
#' `gsdc` for Greenspace Seasonality Data Cube (also see [get_gsdc()]]),
#' `esa_ndvi`or `esa_landcover` for ESA WorldCover 10m Annual Dataset
#' (also see [get_esa_wc()]]), and `s2a_ndvi` or `s2a_bands` for
#' Sentinel-2-l2a image data (also see [get_s2a_ndvi()]]). The default is `gsdc`.
#' @param output_bands vector. A list of band names (`c('B04', 'B08')`).
#' The default is `NULL`. (Only required, when `source = "s2a_bands"`)
#' All available bands can be found [here](https://docs.sentinel-hub.com/api/latest/data/sentinel-2-l2a/#available-bands-and-data)
#' @param cloud_cover numeric. The percentage of cloud coverage for retrieving
#' Sentinel-2-l2a images. (Only required, when `source = "s2a_ndvi"` or `source = "s2a_bands"`)
#' @param vege_perc numeric. The percentage of cloud coverage for retrieving
#' Sentinel-2-l2a images. (Only required, when `source = "s2a_ndvi"` or `source = "s2a_bands"`)
#' @param select character. one of "latest", "earliest", "all". The default
#' is "latest".
#' @param method character. A method for mosaicing layers: one of "mean",
#' "median", "min", "max", "modal", "sum", "first", "last". The default
#' is "first".
#'
#' @return A `data.frame` containing greenspace values extracted at each point
#' across all bands. Each row corresponds to a sample location;
#' columns represent band values.
#'
#' @details
#' `time`: For the greenspace seasonality data cube, only years from 2019 to 2022
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
#'                             # time = 2022
#'                            )
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
sample_values <- function(samples = NULL, time = NULL,
                          source = 'gsdc', output_bands = NULL,
                          cloud_cover = 10, vege_perc = 0,
                          select = "latest", method = 'first') {
  if (is.null(time)) {
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
    raster_data <- get_gsdc(bbox = bbox, year = time, mask = FALSE)
  } else if (source == 'esa_ndvi') {
    raster_data <- get_esa_wc(bbox = bbox, datatype = 'ndvi', year = time, mask = FALSE)
  } else if (source == 'esa_landcover') {
    raster_data <- get_esa_wc(bbox = bbox, datatype = 'landcover', year = time, mask = FALSE)
  } else if (source == 's2a_ndvi') {
    raster_data <- get_s2a_ndvi(bbox = bbox, datetime = time,
                 cloud_cover = cloud_cover,
                 vege_perc = vege_perc,
                 select = select,
                 method = method,
                 mask = FALSE)
  } else if (source == 's2a_bands')
    raster_data <- get_s2a_ndvi(bbox = bbox, datetime = time,
                 cloud_cover = cloud_cover,
                 vege_perc = vege_perc,
                 select = select,
                 method = method,
                 mask = FALSE,
                 output_bands = output_bands)
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

#' @title Classify greenspace based on map tile images
#' @name lc_sem_seg
#' @description
#' Generate high-resolution greenspace segmentation using WorldImagery map
#' tiles provided by esri and Sentinel-2 cloudless mosaic tiles provided
#' by EOX.
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param zoom numeric. Zoom level of map tile. The default is `17`. To
#' exclude RGB-based map tiles in training random forest model,
#' use `zoom = NULL`.
#' @param provider character. One of "esri" and "eox".
#' @param year integer. The desired year for Sentinel-2 cloudless mosaic
#' tiles. (This is required when `provider = "eox"`)
#'
#' @return
#' A list of two rasters including: greenspace segmentation (where 1 is
#' green and 0 is non-green) and original map tiles
#'
#' @note
#' The data derived from Esri WorldImagery may need to include appropriate
#' Esri copyright notice.
#'
#' Using Google Maps data for non-visualization use cases like image analysis,
#' machine interpretation, object detection, or geodata extraction is not permitted.
#'
#' @examples
#' g <- get_tile_green(
#'  # bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#'  zoom = 15
#' )
#'
#' @importFrom maptiles create_provider get_tiles
#' @importFrom terra as.array
#' @export
get_tile_green <- function(bbox = NULL,
                           place = NULL,
                           zoom = 17,
                           provider = 'esri',
                           year = NULL) {
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
  if (provider == "eox" & is.null(year)) stop("`year` is missing.")
  cli::cli_alert_info(paste0('Downloading ',
                             if (provider == "eox") "Sentinel-2 cloudless mosaic " else "Esri.WorldImagery "
                             ,'map tiles ...'))
  if (provider == "eox") {
    xml_text <- write_eox_wms_xml(bbox = sf::st_bbox(bbox), year = year, zoom = zoom)
    temp_xml <- tempfile(fileext = ".xml")
    on.exit(unlink(temp_xml))
    writeLines(xml_text, temp_xml)
    m <- terra::rast(temp_xml)
    mat <- terra::as.array(m)
    m <- terra::rast(mat, extent = terra::ext(m), crs = terra::crs(m))
  } else {
    m <- maptiles::get_tiles(bbox,
                             provider = if (provider == "eox") p else "Esri.WorldImagery",
                             zoom = zoom,
                             crop = TRUE)
  }

  names(m) <- c('r', 'g', 'b')
  t <- m/255

  # The code below was done by Xiaojiang Li,
  # Ian Seiferling, Marwa Abdulhai, Senseable City Lab, MIT
  redThreImgU <- t[[1]] < 0.6
  greenThreImgU <- t[[2]] < 0.9
  blueThreImgU <- t[[3]] < 0.6
  shadowRedU <- t[[1]] < 0.3
  shadowGreenU <- t[[2]] < 0.3
  shadowBlueU <- t[[3]] < 0.3

  greenImg1 <- redThreImgU * blueThreImgU * greenThreImgU
  greenImgShadow1 <- shadowRedU * shadowGreenU * shadowBlueU

  threImgU <-  redThreImgU * blueThreImgU * greenThreImgU
  imgShadow <- shadowRedU * shadowGreenU * shadowBlueU

  g_r_dif <- t[[2]] - t[[1]]
  g_b_dif <- t[[2]] - t[[3]]
  ExG <- g_r_dif + g_b_dif
  diffImg <- g_r_dif * g_b_dif

  threshold <- 0.5
  greenImg3 <- diffImg > 0.0
  greenImg4 <- g_r_dif > 0

  threshold <- graythresh(ExG, 0.1)
  if (threshold > 0.1) {
    threshold <- 0.1
  } else if (threshold < 0.05) {
    threshold <- 0.05
  }

  greenImg2 <- ExG > threshold
  greenImgShadow2 = ExG > 0.05
  greenImg <- greenImg1*greenImg2 + greenImgShadow2*greenImgShadow1
  greenImg <- terra::ifel(greenImg != 0, 1, 0)
  names(greenImg) <- "green"
  map_mask <- terra::ifel(greenImg == 0, NA, m)
  output <- list(green = greenImg,
                 map = m)
  report_time(start_time)
  return(output)
}

#' @title Classify land cover based on multi-source imagery datasets
#' @name lc_sem_seg
#' @description
#' Generate semantic land cover segmentation based on map tiles and bands
#' of Sentinel-2-l2a images by sampling data over the landcover of ESA WorldCover
#' dataset and using random forest. The semantic segmentation includes the same classes
#' as ESA WorldCover dataset with additional class of building (See details).
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. Optional if `place` is provided.
#' @param place character or vector. (optional) A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param label_year numeric. The year of interest: `2020` or `2021` for retrieving
#' land cover from ESA WorldCover dataset. The default is `2021`.
#' @param tiles character or vector of character. One of `"eox"` and
#' `c("eox","esri")` (default).
#' @param tile_year numeric. The year (available from 2018 to 2024) of map tiles
#' provided by EOX.
#' @param zoom numeric. Zoom level of map tiles. The default is `17`.
#' @param s2a character. One of "all_bands", "ndvi", and NULL.
#' If `s2a = "all_bands"`, all bands of Sentinel-2-l2a will downloaded.
#' If `s2a = "ndvi"`, only band 4 and band 8 will be downloaded to calculate
#' NDVI to train randomforest model.
#' If `s2a = NULL` (default), Sentinel-2-l2a will not be included.
#' @param datetime numeric vector of 2. The time of interest such as
#' `c("2020-08-01", "2020-09-01")` for retrieving Sentinel-2-l2a images.
#' (Only required, when `bands = "all_bands"` or `bands = "ndvi"`)
#' @param cloud_cover numeric. The percentage of cloud coverage for retrieving
#' Sentinel-2-l2a images. (Only required, when `bands = "all_bands"` or `bands = "ndvi"`)
#' @param vege_perc numeric. The percentage of cloud coverage for retrieving
#' Sentinel-2-l2a images. (Only required, when `bands = "all_bands"` or `bands = "ndvi"`)
#' @param sample_size numeric. The total number of locations to be sampled
#' over land cover classes. The default is `10000`.
#' @param trian_split numeric. The percentage of samples used for training model.
#' The default is `0.7`.
#' @param test logical. Whether to test the performance of randomforests
#' model. The default is `TRUE`.
#' @param cores logical. Whether parallel processing will be used to
#' generate land cover.
#'
#' @return
#' If `test = FALSE`, a land cover raster.
#' If `test = TRUE`, a list includes a land cover raster, model performance table,
#' and predictor importance plot.
#'
#' @details
#' This function uses ESA WorldCover classification with additional class of building :
#'
#' | Code | Land Cover Class         | RGB Color        | Description                                                                                     |
#' |------|--------------------------|------------------|-------------------------------------------------------------------------------------------------|
#' | 10   | Tree cover               | (0, 100, 0)       | Geographic areas dominated by trees with a cover of 10% or more. Includes plantations, mangroves. |
#' | 20   | Shrubland                | (255, 187, 34)    | Areas dominated by shrubs with cover ≥10%. Includes evergreen/deciduous.                         |
#' | 30   | Grassland                | (255, 255, 76)    | Dominated by herbaceous plants with cover ≥10%. Includes savannas, pastures, steppes.           |
#' | 40   | Cropland                 | (240, 150, 255)   | Areas with annual crops sown/planted and harvested within 12 months.                            |
#' | 50   | Built-up                 | (250, 0, 0)       | Built environment: buildings, roads, other man-made structures.                                 |
#' | 60   | Bare/sparse vegetation   | (180, 180, 180)   | Lands with exposed soil/sand/rock and <10% vegetation.                                          |
#' | 70   | Snow and Ice             | (240, 240, 240)   | Areas permanently covered by snow or glaciers.                                                  |
#' | 80   | Permanent water bodies   | (0, 100, 200)     | Lakes, rivers, reservoirs covered with water >9 months/year.                                    |
#' | 90   | Herbaceous wetland       | (0, 150, 160)     | Wetlands with permanent/seasonal flooding and herbaceous vegetation.                            |
#' | 95   | Mangroves                | (0, 207, 117)     | Salt-tolerant trees/shrubs in coastal intertidal zones.                                         |
#' | 100  | Moss and lichen          | (250, 230, 160)   | Lichen/moss-dominated lands, often with symbiotic fungi and algae.                              |
#' | 105  | Buildings                | (0, 0, 0)         | Buildings                                                                                       |
#'
#' @examples
#' sem <- lc_sem_seg(
#'    # bbox = c(-83.087174,42.333373,-83.042542,42.358748),
#'    tiles = c('esri', 'eox'),
#'    label_year = 2021,
#'    tile_year = 2024
#' )
#'
#' @importFrom terra spatSample rasterize extract compareGeom subst as.int levels predict
#' @importFrom overturemapsr record_batch_reader
#' @importFrom caret train createDataPartition confusionMatrix varImp
#' @importFrom dplyr inner_join
#' @importFrom purrr reduce
#' @importFrom parallel detectCores
#' @importFrom grDevices rgb
#' @importFrom stats predict
#'
#' @export
lc_sem_seg <- function(bbox = NULL,
                       place = NULL,
                       label_year = NULL,
                       tiles = c('esri', 'eox'),
                       tile_year = NULL,
                       zoom = 17,
                       s2a = NULL,
                       datetime = c(),
                       cloud_cover = 10,
                       vege_perc = 0,
                       sample_size = 10000,
                       trian_split = 0.7,
                       test = TRUE,
                       cores = TRUE) {
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

  ## donwload datasets
  lc <- get_esa_wc(bbox = bbox, year = label_year, datatype = "landcover")

  building <- overturemapsr::record_batch_reader("building", bbox = as.numeric(sf::st_bbox(bbox)))
  building <- terra::rasterize(terra::vect(building$geometry), lc)
  building_ <- terra::ifel(is.na(building), 0, 100)

  lc_ <- building_ + lc
  lc_ <- terra::ifel(lc_ > 100, 105, lc_)

  sat_eox <- NULL
  sat_esri <- NULL
  green_eox <- NULL
  green_esri <- NULL
  if (length(tiles) == 1 & tiles[1] == "eox") {
    tile_set <- get_tile_green(bbox, provider = "eox",
                               zoom = zoom, year = tile_year)
    sat_eox <- tile_set$map
    names(sat_eox) <- c('eox_r', 'eox_g', 'eox_b')
    green_eox <- tile_set$green
    names(green_eox) <- c("green_eox")
  } else if (length(tiles) == 2 & "eox" %in% tiles & "esri" %in% tiles) {
    tile_eox <- get_tile_green(bbox, provider = "eox",
                               zoom = zoom, year = tile_year)
    tile_esri <- get_tile_green(bbox, provider = "esri",
                                zoom = zoom)
    sat_eox <- tile_eox$map
    sat_esri <- tile_esri$map
    names(sat_eox) <- c('eox_r', 'eox_g', 'eox_b')
    names(sat_esri) <- c('esri_r', 'esri_g', 'esri_b')
    green_eox <- tile_eox$green
    tile_esri <- tile_esri$green
    names(green_eox) <- c("green_eox")
    names(green_esri) <- c("green_esri")
  }

  bands <- NULL
  ndvi <- NULL
  if (is.null(s2a)) {
    if (s2a == "ndvi") {
      ndvi <- get_s2a_ndvi(bbox = bbox,
                           datetime = datetime,
                           cloud_cover = cloud_cover,
                           vege_perc = vege_perc)
    } else if (s2a == "all_bands") {
      band_names <- c('B01', 'B02', 'B03', 'B04', 'B05', 'B06',
                      'B07', 'B08', 'B09', 'B11', 'B12')
      bands <- get_s2a_ndvi(bbox = bbox,
                            datetime = datetime,
                            cloud_cover = cloud_cover,
                            vege_perc = vege_perc,
                            output_bands = band_names)
      ndvi <- compute_ndvi(bands$B04, bands$B08)
    }
    names(ndvi) <- 'ndvi'
  }

  # sample ground truth and predictors
  samples_vect <- suppressWarnings(
    terra::spatSample(lc_,
                      size = as.integer(sample_size/length(terra::unique(lc_)[[1]])),
                      method = "stratified",
                      as.points = TRUE)
  )
  label_set <- terra::extract(lc_, samples_vect, method = 'simple')

  if (!is.null(sat_eox)) eox_rgb_set <- terra::extract(sat_eox, samples_vect, method = 'simple')
  if (!is.null(green_eox)) eox_green_set <- terra::extract(green_eox, samples_vect, method = 'simple')
  if (!is.null(sat_esri)) esri_rgb_set <- terra::extract(esri_eox, samples_vect, method = 'simple')
  if (!is.null(green_esri)) esri_green_set <- terra::extract(green_esri, samples_vect, method = 'simple')

  if (!is.null(bands)) {
    bands_set <- terra::extract(terra::sprc(bands), samples_vect, method = 'simple')
    bands_set <- bands_set %>% purrr::reduce(dplyr::inner_join, by = "ID")
    colnames(bands_set) <- c("ID", band_names)
  }
  if (!is.null(ndvi)) ndvi_set <- terra::extract(ndvi, samples_vect, method = 'simple')

  # prepare training and testing set
  dataset_df <- list(label_set)
  if (!is.null(sat_eox)) dataset_df[[length(dataset_df)+1]] <- eox_rgb_set
  if (!is.null(green_eox)) dataset_df[[length(dataset_df)+1]] <- eox_green_set
  if (!is.null(sat_esri)) dataset_df[[length(dataset_df)+1]] <- esri_rgb_set
  if (!is.null(green_esri)) dataset_df[[length(dataset_df)+1]] <- esri_green_set
  if (!is.null(bands)) dataset_df[[length(dataset_df)+1]] <- bands_set
  if (!is.null(ndvi)) dataset_df[[length(dataset_df)+1]] <- ndvi_set

  dataset_df <- dataset_df %>% purrr::reduce(dplyr::inner_join, by = "ID")
  dataset_df <- dataset_df[,-c(1)]
  dataset_df$layer <- as.factor(dataset_df$layer)
  dataset_df <- na.omit(dataset_df)

  train_ind <- caret::createDataPartition(dataset_df$layer,
                                          p = trian_split,
                                          list = FALSE)
  data_train <- dataset_df[train_ind,]
  if (test) {data_test <- dataset_df[-train_ind,]}

  # random forest
  cli::cli_alert_info('Training model ...')
  cli::cli_alert_info('This process may take 15 minutes or longer. You can grab a cup of tea or coffee now.')
  rf <- caret::train(layer ~ .,
                     method = 'rf',
                     data = data_train)

  # test and model performance (optional)
  if (test) {
    cli::cli_alert_info('Testing model ...')
    x_test <- data_test[,2:ncol(data_test)]
    y_test <- data_test[,1]
    predictions <- stats::predict(rf, x_test)
    cm <- caret::confusionMatrix(predictions, y_test)
    importance <- caret::varImp(rf)
  }

  # predict
  stack <- list()
  if (!is.null(sat_eox)) stack[[length(stack)+1]] <- sat_eox
  if (!is.null(green_eox)) stack[[length(stack)+1]] <- green_eox
  if (!is.null(sat_esri)) stack[[length(stack)+1]] <- sat_esri
  if (!is.null(green_esri)) stack[[length(stack)+1]] <- green_esri
  if (!is.null(bands)) stack[[length(stack)+1]] <- bands
  if (!is.null(ndvi)) stack[[length(stack)+1]] <- ndvi

  cli::cli_alert_info('Predicting ...')
  cli::cli_alert_info('You can take a break now and come back later ...')

  resolutions <- sapply(stack, function(x) prod(terra::res(x)))
  best_res_index <- which.min(resolutions)
  ref_raster <- stack[[best_res_index]]

  rasters_resampled <- lapply(stack, function(x) {
    if (!terra::compareGeom(x, ref_raster, stopOnError = FALSE)) {
      terra::resample(x, ref_raster, method = "bilinear")
    } else {
      x
    }
  })
  predictor_stack <- terra::rast(rasters_resampled)
  names(predictor_stack) <- colnames(data_train)[2:length(colnames(data_train))]
  classify <- terra::predict(object = predictor_stack,
                             model = rf, na.rm = TRUE,
                             cores = if (!cores) 1 else parallel::detectCores() -1)
  landcover_colors <- c(
    "10" = grDevices::rgb(0, 100, 0, maxColorValue = 255),       # Tree cover
    "20" = grDevices::rgb(255, 187, 34, maxColorValue = 255),    # Shrubland
    "30" = grDevices::rgb(255, 255, 76, maxColorValue = 255),    # Grassland
    "40" = grDevices::rgb(240, 150, 255, maxColorValue = 255),   # Cropland
    "50" = grDevices::rgb(250, 0, 0, maxColorValue = 255),       # Built-up
    "60" = grDevices::rgb(180, 180, 180, maxColorValue = 255),   # Bare/sparse vegetation
    "70" = grDevices::rgb(240, 240, 240, maxColorValue = 255),   # Snow and Ice
    "80" = grDevices::rgb(0, 100, 200, maxColorValue = 255),     # Permanent water bodies
    "90" = grDevices::rgb(0, 150, 160, maxColorValue = 255),     # Herbaceous wetland
    "95" = grDevices::rgb(0, 207, 117, maxColorValue = 255),     # Mangroves
    "100" = grDevices::rgb(250, 230, 160, maxColorValue = 255),  # Moss and lichen
    "105" = grDevices::rgb(0, 0, 0, maxColorValue = 255)         # Buildings
  )
  lvls <- terra::levels(classify)[[1]]
  classified_numeric <- terra::subst(terra::as.int(classify),
                                     from = lvls$value,
                                     to = as.integer(lvls$class))
  levels_df <- data.frame(
    value = as.numeric(names(landcover_colors)),
    col = landcover_colors
  )
  terra::coltab(classified_numeric) <- levels_df
  # output
  report_time(start_time)
  output <- if (test) list(landcover=classified_numeric, confusion_matrix=cm, importance=importance) else classified_numeric
  return(output)
}

