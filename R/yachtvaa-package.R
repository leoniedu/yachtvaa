#' @keywords internal
"_PACKAGE"

#' @import data.table
#' @importFrom rlang .data .env enquo enquos sym syms
#' @importFrom sf st_as_sf st_coordinates st_crs st_drop_geometry st_linestring
#'   st_sfc st_transform
NULL

utils::globalVariables(c(
  ".", ".N", ".SD",
  "x", "y", "time_num", "segment_id",
  "predicted_time_sec", "avg_speed_mps", "bearing_deg",
  "distance_m_target", "athlete",
  "datetime", "name", "value", "ip_value",
  "wind_direction_deg", "current_direction_deg",

  "wind_speed_kmh", "current_speed_kmh",
  "wind_angle_deg", "current_angle_deg",
  "date", "rank",
  "avg_speed_kmh", "join_time", "start_time", "predicted_time_fmt",
  # grib-processing.R / current-field.R
  "u", "v", "speed_kmh", "local_time", "hour", "metric",
  "current_direction", "wind_speed", "wind_direction"
))
