#' @description This function returns all of the cities
#' with seasonal green space data.
#' @return dataframe
#' @references
#' Wu, S., Song, Y., An, J. et al. High-resolution greenspace dynamic
#' data cube from Sentinel-2 satellites over 1028 global major cities.
#' Sci Data 11, 909 (2024). https://doi.org/10.1038/s41597-024-03746-7
#' @examples
#' library(greenSD)
#' check_available_cities()
#' @export
check_available_cities <- function() {
  return(available_cities)
}

#' @noMd
check_overlap <- function() {

}

#' @noMd
get_data_with_uid <- function(uid, year) {
  dict <- subset(data_dictionary, year == as.numeric(year))
  dict <- subset(dict, uid == uid)
  return(dict$download_url)
}
