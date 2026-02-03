# ------------------------------------------------------------------
# Package-wide constants
# ------------------------------------------------------------------

#' Default projected CRS: SIRGAS 2000 / UTM zone 24S (EPSG:31984)
#' Appropriate for Salvador, Bahia, Brazil
#' @noRd
CRS_UTM24S <- 31984L

#' SIMCOSTA buoy ID for Yacht Club da Bahia area
#' @noRd
BUOY_ID_DEFAULT <- 515L

#' Semicircle to degrees conversion factor (Garmin/ANT+ GPS format)
#' @noRd
SEMICIRCLE_TO_DEG <- 180 / 2^31

#' Default target distances for fastest-distance analysis (meters)
#' @noRd
DEFAULT_DISTANCES_M <- c(100, 500, 1000)

#' Padding (seconds) added to buoy data fetch window around session times
#' @noRd
BUOY_TIME_PAD_SEC <- 3600L

#' Study-area bounding box around SIMCOSTA buoy 515 / Salvador, Bahia (WGS84)
#' Approx 16 km x 22 km covering Todos os Santos Bay entrance
#' @export
BBOX_SSA <- c(
  xmin = -38.61380,
  ymin = -13.00741,
  xmax = -38.46308,
  ymax = -12.81308
)
