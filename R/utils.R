#' @importFrom sf read_sf st_intersects
#' @importFrom utils download.file
#' @importFrom terra rast merge nlyr rev map.pal
#' @importFrom cli cli_alert_info cli_alert_success cli_progress_bar cli_progress_update cli_progress_done
#' @importFrom magick image_read image_animate
#' @importFrom dplyr "%>%" mutate pull
#' @importFrom stringr str_pad


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
#' @param fps numeric. Frames per second (default 5).
#' @param width numeric. Width of output GIF in pixels.
#' @param height numeric. Height of output GIF in pixels.
#' @param axes logical. Draw axes?
#' @param title_prefix character or character vector.
#' Optional prefix or per-frame titles.
#' @return An animated magick image object (GIF).
#' @examples
#' sample_data <- terra::rast(system.file("extdata", "detroit_gs.tif", package = "greenSD"))
#' gif <- to_gif(sample_data)
#'
#' @export
to_gif <- function (r, fps = 5, width = 600, height = 600,
                    axes = TRUE, title_prefix = NULL) {
  stopifnot(inherits(r, "SpatRaster"), terra::nlyr(r) > 1)

  temp_dir <- tempdir()
  img_paths <- character()

  on.exit(unlink(img_paths, recursive = TRUE), add = TRUE)

  for (i in 1:terra::nlyr(r)) {
    png_file <- file.path(temp_dir, sprintf("frame_%02d.png", i))
    # Handle dynamic or static title
    title <- if (is.null(title_prefix)) {
      paste("Day", i * 10)
    } else if (length(title_prefix) == 1) {
      paste(title_prefix, i)
    } else {
      title_prefix[i]
    }
    png(png_file, width = width, height = height)
    terra::plot(r[[i]],
                col = terra::rev(terra::map.pal('viridis', 100)),
                main = title
    )
    dev.off()
    img_paths[i] <- png_file
  }

  frames <- magick::image_read(img_paths)
  animation <- magick::image_animate(frames, fps = fps)
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
  date_obj <- as.Date(paste0(year, '-', time))
  day_of_year <- as.integer(format(date_obj, "%j"))
  band_index <- ((day_of_year - 1) %/% 10) + 1
  return(as.integer(band_index))
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
get_esa_tile_names <- function(lat_min, lat_max, lon_min, lon_max) {
  lat_range <- floor(lat_min):floor(lat_max)
  lon_range <- floor(lon_min):floor(lon_max)

  base::expand.grid(lat = lat_range, lon = lon_range) %>%
    dplyr::mutate(
      lat_label = ifelse(lat >= 0, paste0("N", stringr::str_pad(lat, 2, pad = "0")),
                         paste0("S", stringr::str_pad(abs(lat), 2, pad = "0"))),
      lon_label = ifelse(lon >= 0, paste0("E", stringr::str_pad(lon, 3, pad = "0")),
                         paste0("W", stringr::str_pad(abs(lon), 3, pad = "0"))),
      tile_name = paste0(lat_label, lon_label)
    ) %>%
    dplyr::pull(tile_name)
}

#' @noMd
get_GHSurl <- function(year, id, type) {
  if (type == 'pop') {
    # source: https://human-settlement.emergency.copernicus.eu/download.php?ds=pop
    return(
      paste0(
        'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_POP_E',
        year,
        '_GLOBE_R2023A_54009_100_V1_0_',
        id,
        '.zip'
      )
    )
  } else if (type == 'b_surf') {
    return(
      list(
        paste0(
          'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_E',
          year,
          '_GLOBE_R2023A_54009_100_V1_0_',
          id,
          '.zip'
        ),
        paste0(
          'https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_BUILT_S_GLOBE_R2023A/GHS_BUILT_S_NRES_E2025_GLOBE_R2023A_54009_100/V1-0/tiles/GHS_BUILT_S_NRES_E',
          year,
          '_GLOBE_R2023A_54009_100_V1_0_',
          id,
          '.zip'
        )
      )
    )
  }
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
