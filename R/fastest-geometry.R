# ------------------------------------------------------------------
# LINESTRING geometry builder for fastest-distance results
# ------------------------------------------------------------------

#' Build sf LINESTRING geometry from fastest-distance results
#'
#' Converts the start/end coordinate columns from [fastest_straight_distance()]
#' into an sf object with LINESTRING geometry.
#'
#' @param result_dt A data.table or data.frame from [fastest_straight_distance()]
#'   containing `start_x`, `start_y`, `end_x`, `end_y` columns.
#' @param crs Coordinate reference system (integer EPSG code or sf crs object).
#'   Must match the CRS used in the original analysis.
#' @return An sf object with LINESTRING geometry.
#' @export
fastest_straight_geometry <- function(result_dt, crs = CRS_UTM24S) {
  if (nrow(result_dt) == 0L) {
    return(sf::st_sf(
      result_dt,
      geometry = sf::st_sfc(crs = crs)
    ))
  }

  lines <- mapply(
    function(x1, y1, x2, y2) {
      sf::st_linestring(
        matrix(c(x1, y1, x2, y2), ncol = 2, byrow = TRUE)
      )
    },
    result_dt$start_x,
    result_dt$start_y,
    result_dt$end_x,
    result_dt$end_y,
    SIMPLIFY = FALSE
  )

  sf::st_as_sf(
    result_dt,
    geometry = sf::st_sfc(lines, crs = crs)
  )
}
