#' @description This function returns all of the cities
#' with seasonal green space data.
#' @return dataframe
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @examples
#' check_available_cities(test = TRUE)
#' @export
check_available_cities <- function(test = FALSE) {
  if (isTRUE(test)) {
    return(NULL)
  }
  cli::cli_alert_info("You can also check all available cities in an interacive map here: https://github.com/billbillbilly/greenSD/blob/main/docs/city_urban_boundaries.geojson")
  return(available_cities)
}

#' @description This function returns a polygon of a city boundary based on the UID
#' @return sf
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @examples
#' check_city_boundary(test = TRUE)
#' @export
check_city_boundary <- function(uid = NULL, plot = TRUE, test = FALSE) {
  if (isTRUE(test)) {
    return(NULL)
  }
  boundary <- suppressMessages(
    sf::read_sf('https://raw.githubusercontent.com/billbillbilly/greenSD/main/docs/city_urban_boundaries.geojson')
  )
  b <- subset(boundary, UID==uid)
  plot(b$geometry)
  return(b)
}

#' @noMd
check_overlap <- function(geometry) {
  boundary <- suppressMessages(
    sf::read_sf('https://raw.githubusercontent.com/billbillbilly/greenSD/main/docs/city_urban_boundaries.geojson')
  )
  intersecting <- boundary[sf::st_intersects(boundary, geometry, sparse = FALSE), ]
  return(intersecting$UID[1])
}

#' @noMd
get_data_with_uid <- function(uid, year) {
  dict <- subset(data_dictionary, year == as.numeric(year))
  dict <- subset(dict, uid == uid)
  return(dict$download_url)
}

#' @noMd
download_data <- function(urls) {
  original_timeout <- getOption('timeout')
  on.exit(options(timeout = original_timeout), add = TRUE)
  options(timeout=9999)

  # check os
  os <- Sys.info()[["sysname"]]
  if (os == "Windows") {
    d_mode <- 'wb'
  }

  result_list <- list()
  temp_paths <- c()
  cli::cli_alert_info('Start downloading seasonal green space data ...')
  for (i in seq_len(length(urls))) {
    temp_tif <- tempfile(fileext = ".tif")
    utils::download.file(urls[i],
                         destfile = temp_tif,
                         mode = d_mode,
                         quiet = TRUE)
    rast_data <- terra::rast(temp_tif)
    result_list[[length(result_list) + 1]] <- rast_data
    temp_paths <- c(temp_paths, temp_tif)
  }
  cli::cli_alert_success('Finished downloading data')

  on.exit(unlink(temp_paths, recursive = TRUE), add = TRUE)
  r <- if (length(result_list) == 1) result_list[[1]] else do.call(terra::merge, result_list)
  return(r/1000)
}

#' @noMd
get_band_index_by_time <- function(time, year) {
  date_obj <- as.Date(paste0(year, '-', time))
  day_of_year <- as.integer(format(date_obj, "%j"))
  band_index <- ((day_of_year - 1) %/% 10) + 1
  return(band_index)
}

#' @noMd
report_time <- function(start_time) {
  end_time <- Sys.time()
  process_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
  if (process_time >= 60) {
    cli::cli_alert_success(paste0("Completed. Time taken: ", round(process_time / 60), " minutes."))
  } else {
    cli::cli_alert_success(paste0("Completed. Time taken: ", round(process_time), " seconds."))
  }
}
