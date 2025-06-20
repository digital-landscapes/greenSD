#' @description download Greenspace Seasonality Data Cube
#' @param bbox `sf`, `sfc`, or a numeric vector (xmin, ymin, xmax, ymax)
#' defining the area of interest. This can be ignored if `place` is specified.
#' @param place vector (optional). A single line address,
#' e.g. ("1600 Pennsylvania Ave NW, Washington") or a vector of addresses
#' (c("Madrid", "Barcelona")).
#' @param location vector.
#' @param UID numeric.
#' @param year numeric.
#' @param mask logical (optional). Default is `FALSE`. If `TRUE`, masks the
#' raster data using the given `bbox`.

seach_city <- function(bbox = NULL, place = NULL, location = NULL, year = NULL, mask = NULL) {
  if(!inherits(UID, 'NULL')) {

  }

  # find intersected area with a spatial point
  if (!inherits(location, 'NULL')) {

  }

  if (inherits(bbox, 'NULL') && inherits(place, 'NULL')) {
    base::warning('Area of interest is missing: boox or place')
    return(NULL)
  }

  start_time <- Sys.time()

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
    }
  }

  # Ensure input is in WGS84
  bbox <- sf::st_transform(bbox, 4326)

  # find all areas of spatial grid that intersect with bbox
  intersecting <- city_urban_boundaries[sf::st_intersects(metadata, bbox, sparse = FALSE), ]

  if (nrow(intersecting) == 0) {
    base::warning("No urban areas intersect with the provided bbox/location/place.")
    return(NULL)
  }

}
