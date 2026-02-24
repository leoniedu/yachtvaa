# ------------------------------------------------------------------
# Wind and current analysis relative to vessel heading
# ------------------------------------------------------------------

#' Compute relative wind angle and classification
#'
#' Given a vessel bearing and meteorological wind direction (FROM convention),
#' computes the relative angle and classifies wind as headwind, tailwind, or
#' crosswind. When `wind_speed` is provided and equals zero, the class is
#' set to `"calm"` regardless of direction.
#'
#' @param vessel_bearing Numeric vector of vessel headings in degrees \[0, 360).
#' @param wind_direction Numeric vector of wind directions (FROM) in degrees.
#' @param wind_speed Optional numeric vector of wind speeds. When zero, the
#'   classification is `"calm"`.
#' @return A tibble with columns `wind_angle_deg` and `wind_class`.
#' @export
relative_wind <- function(vessel_bearing, wind_direction, wind_speed = NULL) {
  result <- classify_wind_angle(vessel_bearing, wind_direction,
                                wind_speed = wind_speed)
  tibble::tibble(
    wind_angle_deg = result$angle_deg,
    wind_class = result$class
  )
}

#' Compute relative current angle and classification
#'
#' Given a vessel bearing and oceanographic current direction (TO convention),
#' computes the relative angle and classifies current as following, opposing,
#' or cross.
#'
#' @param vessel_bearing Numeric vector of vessel headings in degrees \[0, 360).
#' @param current_direction Numeric vector of current directions (TO) in degrees.
#' @return A tibble with columns `current_angle_deg` and `current_class`.
#' @export
relative_current <- function(vessel_bearing, current_direction) {
  result <- classify_current_angle(vessel_bearing, current_direction)
  tibble::tibble(
    current_angle_deg = result$angle_deg,
    current_class = result$class
  )
}

#' Add apparent condition columns to segments with buoy data
#'
#' Takes a data frame of segments already matched with buoy data (via
#' [match_buoy_to_segments()]) and adds relative wind/current angles,
#' classifications, and along-track components.
#'
#' Required input columns:
#' - `bearing_deg`: vessel heading from [fastest_straight_distance()]
#' - `wind_direction_deg`: meteorological wind direction (FROM)
#' - `wind_speed_kmh`: wind speed in km/h
#' - `current_direction_deg`: oceanographic current direction (TO)
#' - `current_speed_kmh`: current speed in km/h
#'
#' When `impute_missing_wind = TRUE`, missing wind columns
#' (`wind_direction_deg`, `wind_speed_kmh`) or columns that are entirely
#' `NA` are replaced with zeros and a warning is issued. This allows the
#' pipeline to run when buoy wind data is unavailable.
#'
#' @param segments A data frame with segment + buoy columns.
#' @param impute_missing_wind If `TRUE`, treat missing or all-`NA` wind data
#'   as zero (no wind) and emit a warning. Default `FALSE`.
#' @return The input with additional columns:
#'   `wind_angle_deg`, `wind_class`, `wind_component_kmh`,
#'   `current_angle_deg`, `current_class`, `current_component_kmh`.
#'
#'   Component signs: positive = assisting (reducing effort),
#'   negative = opposing (increasing effort).
#' @export
apparent_conditions <- function(segments, impute_missing_wind = FALSE) {
  segments <- tibble::as_tibble(segments)
  segments <- .maybe_impute_wind(segments, impute_missing_wind)

  wind_rel <- relative_wind(segments$bearing_deg, segments$wind_direction_deg,
                            wind_speed = segments$wind_speed_kmh)
  current_rel <- relative_current(
    segments$bearing_deg,
    segments$current_direction_deg
  )

  # Along-track component: cos(angle) projection
  # For wind: headwind (angle=0) -> cos(0)=1, but wind is FROM so this
  # means the wind opposes the vessel. We negate: component = -cos(angle)
  # Result: negative = headwind (opposing), positive = tailwind (assisting)
  wind_angle_rad <- wind_rel$wind_angle_deg * pi / 180
  wind_component <- -segments$wind_speed_kmh * cos(wind_angle_rad)

  # For current: following (angle=0) -> cos(0)=1, current is TO so this
  # means the current assists the vessel. No negation needed.
  # Result: positive = following (assisting), negative = opposing
  current_angle_rad <- current_rel$current_angle_deg * pi / 180
  current_component <- segments$current_speed_kmh * cos(current_angle_rad)

  dplyr::mutate(
    segments,
    wind_angle_deg = wind_rel$wind_angle_deg,
    wind_class = wind_rel$wind_class,
    wind_component_kmh = wind_component,
    current_angle_deg = current_rel$current_angle_deg,
    current_class = current_rel$current_class,
    current_component_kmh = current_component
  )
}

#' Impute missing wind columns if requested
#'
#' @param segments Data frame of segments.
#' @param impute Logical flag.
#' @return `segments`, possibly with wind columns added or zeroed.
#' @noRd
.maybe_impute_wind <- function(segments, impute) {
  has_dir <- "wind_direction_deg" %in% names(segments)
  has_spd <- "wind_speed_kmh" %in% names(segments)

  dir_all_na <- has_dir && all(is.na(segments$wind_direction_deg))
  spd_all_na <- has_spd && all(is.na(segments$wind_speed_kmh))

  needs_impute <- !has_dir || !has_spd || dir_all_na || spd_all_na

  if (!needs_impute) {
    return(segments)
  }

  if (!impute) {
    missing <- character()
    if (!has_dir) missing <- c(missing, "wind_direction_deg")
    if (!has_spd) missing <- c(missing, "wind_speed_kmh")
    if (dir_all_na) missing <- c(missing, "wind_direction_deg (all NA)")
    if (spd_all_na) missing <- c(missing, "wind_speed_kmh (all NA)")
    cli::cli_abort(c(
      "Wind data is missing or entirely {.code NA}: {.field {missing}}.",
      "i" = "Set {.arg impute_missing_wind = TRUE} to treat missing wind as zero."
    ))
  }

  cli::cli_warn(c(
    "Wind data is missing or entirely {.code NA}; imputing as zero (no wind).",
    "i" = "Wind-related outputs will show no wind effect."
  ))

  if (!has_dir || dir_all_na) {
    segments$wind_direction_deg <- 0
  }
  if (!has_spd || spd_all_na) {
    segments$wind_speed_kmh <- 0
  }

  segments
}
