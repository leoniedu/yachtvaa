# ------------------------------------------------------------------
# GPS record processing: raw records to projected sf points
# ------------------------------------------------------------------

#' Convert GPS records to projected sf points
#'
#' Takes a data frame of GPS records (from treinusr) and converts to an sf
#' object projected to a planar CRS. Handles both decimal degree coordinates
#' and Garmin/ANT+ semicircle format.
#'
#' @param records A data frame with GPS records containing at minimum
#'   `position_lat`, `position_long`, and `timestamp` columns.
#' @param crs Target projected CRS (integer EPSG code). Default 31984
#'   (SIRGAS 2000 / UTM 24S).
#' @param semicircles Logical. If TRUE (default), assumes `position_lat` and
#'   `position_long` are in semicircle units (Garmin/ANT+ format) and converts
#'   to degrees. Set to FALSE if coordinates are already in decimal degrees.
#' @return An sf object with POINT geometry in the target CRS.
#' @export
records_to_sf <- function(records, crs = CRS_UTM24S, semicircles = TRUE) {
  records <- dplyr::filter(records, !is.na(.data$position_long))

  if (semicircles) {
    records <- dplyr::mutate(
      records,
      lat = .data$position_lat * SEMICIRCLE_TO_DEG,
      lon = .data$position_long * SEMICIRCLE_TO_DEG
    )
  } else {
    records <- dplyr::mutate(
      records,
      lat = .data$position_lat,
      lon = .data$position_long
    )
  }

  records <- dplyr::mutate(records, timestamp = as.POSIXct(.data$timestamp, tz = "UTC"))

  records |>
    sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326L) |>
    sf::st_transform(crs = crs)
}
