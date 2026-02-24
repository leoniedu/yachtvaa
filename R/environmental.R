# ------------------------------------------------------------------
# Environmental data: interpolation and matching to GPS segments
# ------------------------------------------------------------------

#' Interpolate buoy data to a regular time grid
#'
#' Creates a regular time grid at `interval_sec` spacing and linearly
#' interpolates all numeric columns using [zoo::na.approx()].
#'
#' @param buoy_data A data frame from `rsimcosta::simcosta_fetch()` with a
#'   `datetime` column (POSIXct) and numeric measurement columns.
#' @param interval_sec Interpolation interval in seconds. Default 600 (10 min).
#' @return A tibble with regularly spaced `datetime` and interpolated columns.
#' @export
interpolate_buoy <- function(buoy_data, interval_sec = 600L) {
  if (nrow(buoy_data) == 0L) {
    return(tibble::as_tibble(buoy_data))
  }

  time_range <- range(buoy_data$datetime, na.rm = TRUE)
  regular_grid <- tibble::tibble(
    datetime = seq.POSIXt(
      from = time_range[1],
      to = time_range[2],
      by = interval_sec
    )
  )

  merged <- dplyr::full_join(regular_grid, buoy_data, by = "datetime") |>
    dplyr::arrange(.data$datetime)

  # Drop non-numeric, non-datetime columns before interpolation
  numeric_cols <- names(merged)[purrr::map_lgl(merged, is.numeric)]

  merged |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(numeric_cols),
        ~ zoo::na.approx(.x, na.rm = FALSE)
      )
    ) |>
    # Keep only the regular grid times
    dplyr::semi_join(regular_grid, by = "datetime")
}

#' Match buoy observations to GPS segments by nearest timestamp
#'
#' Uses a rolling join (data.table) to attach the nearest buoy observation
#' to each segment based on the segment's start time.
#'
#' @param segments A data.table or data.frame with a `start_time` column
#'   (POSIXct), typically from [fastest_straight_distance()].
#' @param buoy_data A data frame of (interpolated) buoy data with a
#'   `datetime` column (POSIXct).
#' @return A data.table combining segment data with matched buoy columns.
#' @export
match_buoy_to_segments <- function(segments, buoy_data) {
  seg_dt <- data.table::as.data.table(segments)
  buoy_dt <- data.table::as.data.table(buoy_data)

  # Create numeric join keys
  seg_dt[, join_time := as.numeric(start_time)]
  buoy_dt[, join_time := as.numeric(datetime)]

  data.table::setkey(buoy_dt, join_time)

  result <- buoy_dt[seg_dt, roll = "nearest", on = "join_time"]

  # Clean up join columns
  result[, join_time := NULL]

  result[]
}
