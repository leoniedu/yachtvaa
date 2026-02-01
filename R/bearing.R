# ------------------------------------------------------------------
# Bearing and angle utilities for projected (UTM) coordinates
# ------------------------------------------------------------------

#' Compute bearing from projected coordinates
#'
#' Calculates the bearing (azimuth) from point (x1, y1) to point (x2, y2)
#' in projected coordinates. Returns degrees clockwise from North (0-360).
#'
#' @param x1,y1 Start point coordinates (easting, northing in meters).
#' @param x2,y2 End point coordinates (easting, northing in meters).
#' @return Numeric bearing in degrees \[0, 360).
#' @export
compute_bearing <- function(x1, y1, x2, y2) {
  dx <- x2 - x1
  dy <- y2 - y1
  bearing <- atan2(dx, dy) * 180 / pi
  bearing %% 360
}

#' Signed angle between two bearings
#'
#' Returns the smallest signed angle from `bearing_from` to `bearing_to`,
#' in the range (-180, 180\].
#'
#' @param bearing_from,bearing_to Numeric bearings in degrees \[0, 360).
#' @return Numeric angle in degrees (-180, 180\]. Positive = clockwise.
#' @export
angle_between <- function(bearing_from, bearing_to) {
  diff <- (bearing_to - bearing_from) %% 360
  ifelse(diff > 180, diff - 360, diff)
}

#' Classify wind angle relative to vessel heading
#'
#' Wind direction uses meteorological convention: direction wind blows FROM.
#' A wind FROM the same direction the vessel is heading means headwind.
#' When `wind_speed` is provided and equals zero, the class is `"calm"`.
#'
#' @param vessel_bearing Vessel heading in degrees \[0, 360).
#' @param wind_direction Wind direction (FROM) in degrees \[0, 360).
#' @param wind_speed Optional numeric vector of wind speeds. When zero, the
#'   classification is `"calm"` regardless of direction.
#' @return A list with:
#'   - `angle_deg`: absolute angle (0 = headwind, 180 = tailwind)
#'   - `class`: one of `"calm"`, `"headwind"`, `"tailwind"`,
#'     `"crosswind_left"`, `"crosswind_right"`
#' @export
classify_wind_angle <- function(vessel_bearing, wind_direction,
                                wind_speed = NULL) {
  # Wind blows FROM wind_direction, so it arrives at the vessel from that
  # direction. The angle between vessel heading and wind source tells us
  # if it's a headwind (same direction) or tailwind (opposite).
  angle <- angle_between(vessel_bearing, wind_direction)
  abs_angle <- abs(angle)

  is_calm <- if (!is.null(wind_speed)) {
    wind_speed == 0
  } else {
    rep(FALSE, length(abs_angle))
  }

  class <- dplyr::case_when(
    is_calm ~ "calm",
    abs_angle <= 45 ~ "headwind",
    abs_angle >= 135 ~ "tailwind",
    angle > 0 ~ "crosswind_right",
    TRUE ~ "crosswind_left"
  )

  list(angle_deg = abs_angle, class = class)
}

#' Classify current angle relative to vessel heading
#'
#' Current direction uses oceanographic convention: direction current flows TO.
#' A current flowing in the same direction as the vessel = following current.
#'
#' @param vessel_bearing Vessel heading in degrees \[0, 360).
#' @param current_direction Current direction (TO) in degrees \[0, 360).
#' @return A list with:
#'   - `angle_deg`: absolute angle (0 = following, 180 = opposing)
#'   - `class`: one of "following", "opposing", "cross_left", "cross_right"
#' @export
classify_current_angle <- function(vessel_bearing, current_direction) {
  angle <- angle_between(vessel_bearing, current_direction)
  abs_angle <- abs(angle)

  class <- dplyr::case_when(
    abs_angle <= 45 ~ "following",
    abs_angle >= 135 ~ "opposing",
    angle > 0 ~ "cross_right",
    TRUE ~ "cross_left"
  )

  list(angle_deg = abs_angle, class = class)
}
