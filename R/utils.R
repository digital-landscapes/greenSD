#' @importFrom sf read_sf st_intersects
#' @importFrom utils download.file
#' @importFrom terra rast merge nlyr rev map.pal
#' @importFrom cli cli_alert_info cli_alert_success cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom magick image_read image_animate


#' @title Get all of the urban areas in the Greenspace Seasonality Data Cube
#' @name check_available_urban
#' @description This function returns all of the urban areas
#' in the Greenspace Seasonality Data Cube dataset.
#' @return dataframe
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @note
#' You can explore all available urban areas in an interacive map at:
#' \url{https://github.com/billbillbilly/greenSD/blob/main/docs/city_urban_boundaries.geojson}
#' @examples
#' check_available_urban(test = TRUE)
#' @export
check_available_urban <- function(test = FALSE) {
  if (isTRUE(test)) {
    return(NULL)
  }
  cli::cli_alert_info("You can also check all available cities in an interacive map here: https://github.com/billbillbilly/greenSD/blob/main/docs/city_urban_boundaries.geojson")
  return(available_cities)
}

#' @title Get an urban area boundary based on the UID
#' @name check_urban_boundary
#' @description This function returns a polygon of a city boundary based on the UID
#' @return sf
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @examples
#' check_urban_boundary(test = TRUE)
#' @export
check_urban_boundary <- function(uid = NULL, plot = TRUE, test = FALSE) {
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

#' @title Convert A Multi-layer Raster to GIF
#' @description Export a multi-layer raster (`SpatRaster`) to an animated GIF.
#' @param r A SpatRaster with multiple layers.
#' @param fps Frames per second (default 5).
#' @param width Width of output GIF in pixels.
#' @param height Height of output GIF in pixels.
#' @return An animated magick image object (GIF).
#' @export
to_gif <- function (r, fps = 5, width = 600, height = 600, dir = '.') {
  temp_dir <- tempdir()
  img_paths <- character()

  on.exit(unlink(temp_paths, recursive = TRUE), add = TRUE)

  for (i in 1:terra::nlyr(r)) {
    png_file <- file.path(temp_dir, sprintf("frame_%02d.png", i))
    png(png_file, width = width, height = height)
    terra::plot(r[[i]],
                col = terra::rev(terra::map.pal('viridis', 100)),
                main = paste("Day", i*10)
    )
    dev.off()
    img_paths[i] <- png_file
  }

  frames <- magick::image_read(img_paths)
  animation <- magick::image_animate(frames, fps = 5)
  return(animation)
}

#' @title Get band index based on time period
#' @name get_band_index_by_time
#' @description
#' Converts a date string in `"MM-DD"` format to the corresponding band index
#' for the Greenspace Seasonality Data Cube, which contains 36 bands representing
#' 10-day intervals over a year.
#'
#' @param year numeric. (required) The year of interest.
#' @param time Character vector of length 2. (optional) Start and end dates in `"MM-DD"` format
#' (e.g., `c("03-20", "10-15")`). Used to subset the 10-day interval data cube by time.
#' @examples
#' get_band_index_by_time(c("03-20", "10-15"), year = 2020)
#'
#' @details
#' The Greenspace Data Cube is organized into 36 bands per year, each representing a 10-day interval.
#' This function calculates which of those bands a given date falls into by converting the MM-DD
#' string into the day-of-year (DOY) and dividing by 10 (rounded up).
#' @export
get_band_index_by_time <- function(time, year) {
  if (isTRUE(test)) {
    return(NULL)
  }
  date_obj <- as.Date(paste0(year, '-', time))
  day_of_year <- as.integer(format(date_obj, "%j"))
  band_index <- ((day_of_year - 1) %/% 10) + 1
  return(band_index)
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
get_data_with_uid <- function(id, y) {
  dict <- data_dictionary[data_dictionary$year == y, ]
  dict <- dict[dict$uid == id, ]
  return(dict$download_url)
}

#' @noMd
download_data <- function(urls) {
  original_timeout <- getOption('timeout')
  options(timeout=9999)

  # check os
  d_mode <- if (Sys.info()[["sysname"]] == "Windows") "wb" else "auto"

  result_list <- list()
  temp_paths <- c()
  on.exit({
    options(timeout = original_timeout)
    unlink(temp_paths, recursive = TRUE)
  }, add = TRUE)

  cli::cli_alert_info('Start downloading seasonal greenspace data ...')
  if (length(urls) >= 2) {
    cli::cli_alert_info(
      "There are {length(urls)} tiles to download and process. This may take more than 5 minutes."
    )
  }
  cli::cli_progress_bar("Downloading", total = length(urls))
  for (i in seq_len(length(urls))) {
    cli::cli_progress_update()
    temp_tif <- tempfile(fileext = ".tif")
    utils::download.file(urls[i],
                         destfile = temp_tif,
                         mode = d_mode,
                         quiet = TRUE)
    rast_data <- terra::rast(temp_tif)
    result_list[[length(result_list) + 1]] <- rast_data
    temp_paths <- c(temp_paths, temp_tif)
  }
  cli::cli_progress_done()
  cli::cli_alert_success('Finished downloading data')

   if (length(result_list) == 1) {
     r <- result_list[[1]]
  } else {
    cli::cli_alert_info('Merging multiple tiles ...')
    r <- do.call(terra::merge, result_list)
  }
  cli::cli_alert_success("Data successfully processed.")
  return(r/1000)
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

#' @noMd
plot_change <- function(change_threshould) {

}
