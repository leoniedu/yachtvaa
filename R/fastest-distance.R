# ------------------------------------------------------------------
# Fastest straight-line distance analysis for GPS tracks
# ------------------------------------------------------------------
# Finds, for each athlete, the fastest predicted time to cover
# a given straight-line distance, using:
#
# 1) First point >= distance from each start
# 2) Average speed between start and that point
# 3) Predicted time for exactly `distance`
#
# Tracks are automatically split when consecutive timestamps
# differ by more than `max_gap` seconds (default = 20).
#
# Ported from rtreinus with improvements:
#   - Proper sf:: namespace (no library(sf))
#   - data.table column access without get()
#   - Empty data.table on no results (not string)
#   - bearing_deg column from start/end coords
#   - Multiple distances support via distance_m vector
# ------------------------------------------------------------------

#' Fastest predicted time for one continuous track segment
#'
#' Inner algorithm operating on pre-extracted coordinate/time vectors.
#' Returns NULL if no pair of points spans the required distance.
#'
#' @param x,y Numeric vectors of projected coordinates (meters).
#' @param time_numeric Numeric vector of timestamps (seconds since epoch).
#' @param distance_m Single target distance in meters.
#' @param min_time Minimum elapsed time filter (seconds), or NULL.
#' @param max_speed Maximum speed filter (m/s), or NULL.
#' @return A list with `t_pred_sec`, `start_index`, `end_index`, or NULL.
#' @noRd
.fastest_distance_vectorized <- function(
    x,
    y,
    time_numeric,
    distance_m,
    min_time = NULL,
    max_speed = NULL) {
  n <- length(x)
  if (n < 2L) {
    return(NULL)
  }

  dist_sq_threshold <- distance_m^2
  best_t_pred <- Inf
  best_i <- NA_integer_
  best_j <- NA_integer_

  for (i in seq_len(n - 1L)) {
    j_indices <- (i + 1L):n

    dx <- x[j_indices] - x[i]
    dy <- y[j_indices] - y[i]
    d_sq <- dx^2 + dy^2

    match_idx <- which(d_sq >= dist_sq_threshold)[1L]

    if (!is.na(match_idx)) {
      real_j <- j_indices[match_idx]
      d <- sqrt(d_sq[match_idx])
      dt_ij <- time_numeric[real_j] - time_numeric[i]

      if (!is.null(min_time) && dt_ij < min_time) next
      if (!is.null(max_speed) && (d / dt_ij) > max_speed) next

      t_pred <- (distance_m * dt_ij) / d

      if (t_pred < best_t_pred) {
        best_t_pred <- t_pred
        best_i <- i
        best_j <- real_j
      }
    }
  }

  if (is.infinite(best_t_pred)) {
    return(NULL)
  }

  list(
    t_pred_sec = best_t_pred,
    start_index = best_i,
    end_index = best_j
  )
}

#' Fastest straight-line distance per athlete
#'
#' For each athlete in the input sf points, finds the segment where the
#' predicted time to cover `distance_m` meters in a straight line is
#' minimized. Supports multiple target distances in a single call.
#'
#' Tracks are split into continuous segments when consecutive timestamps
#' differ by more than `max_gap` seconds.
#'
#' @param sf_points An sf object with POINT geometry in a projected CRS
#'   (units in meters).
#' @param athlete_col Character name of the column identifying athletes.
#' @param time_col Character name of the POSIXct timestamp column.
#' @param distance_m Numeric vector of target distances in meters.
#'   Defaults to 500.
#' @param max_gap Maximum time gap (seconds) before splitting a track into
#'   separate segments. Default 20.
#' @param min_time Minimum elapsed time (seconds) for a valid segment, or
#'   NULL for no filter.
#' @param max_speed_kmh Maximum plausible speed in km/h. Default Inf.
#' @return A data.table with one row per athlete per distance, containing:
#'   `distance_m_target`, `predicted_time_sec`, `avg_speed_mps`,
#'   `avg_speed_kmh`, `bearing_deg`, `start_time`, `end_time`,
#'   `straight_distance_m`, `cumulative_distance_m`, `sinuosity`,
#'   `start_x`, `start_y`, `end_x`, `end_y`.
#'   Returns an empty data.table if no valid segments are found.
#' @export
fastest_straight_distance <- function(
    sf_points,
    athlete_col,
    time_col,
    distance_m = 500,
    max_gap = 20,
    min_time = NULL,
    max_speed_kmh = Inf) {
  stopifnot(inherits(sf_points, "sf"))
  stopifnot(lubridate::is.POSIXct(sf_points[[time_col]]))

  crs_info <- sf::st_crs(sf_points)
  if (!is.null(crs_info$units_gdal) && crs_info$units_gdal != "metre") {
    cli::cli_abort("CRS uses non-metre units. Project to a planar CRS first.")
  }

  coords <- sf::st_coordinates(sf_points)
  max_speed <- max_speed_kmh * 1000 / 3600

  dt <- data.table::as.data.table(sf_points)
  dt[, `:=`(
    x = coords[, 1],
    y = coords[, 2],
    time_num = as.numeric(.SD[[time_col]])
  )]
  data.table::setorderv(dt, cols = c(athlete_col, "time_num"))

  # Split tracks by time gaps
  dt[, segment_id := 1L, by = c(athlete_col)]
  if (!is.null(max_gap)) {
    dt[,
      segment_id := cumsum(c(TRUE, diff(time_num) > max_gap)),
      by = c(athlete_col)
    ]
  }

  # Process each target distance
  all_results <- lapply(distance_m, function(dist) {
    seg_res <- dt[,
      {
        out <- .fastest_distance_vectorized(
          x = x,
          y = y,
          time_numeric = time_num,
          distance_m = dist,
          min_time = min_time,
          max_speed = max_speed
        )

        if (is.null(out)) {
          NULL
        } else {
          i <- out$start_index
          j <- out$end_index
          d_end <- sqrt((x[j] - x[i])^2 + (y[j] - y[i])^2)
          dt_ij <- time_num[j] - time_num[i]

          cum_dist <- sum(sqrt(diff(x[i:j])^2 + diff(y[i:j])^2))
          brng <- compute_bearing(x[i], y[i], x[j], y[j])

          time_vals <- .SD[[time_col]]

          .(
            distance_m_target = dist,
            predicted_time_sec = out$t_pred_sec,
            avg_speed_mps = d_end / dt_ij,
            bearing_deg = brng,
            start_time = time_vals[i],
            end_time = time_vals[j],
            straight_distance_m = d_end,
            cumulative_distance_m = cum_dist,
            sinuosity = cum_dist / d_end,
            start_x = x[i],
            start_y = y[i],
            end_x = x[j],
            end_y = y[j]
          )
        }
      },
      by = c(athlete_col, "segment_id")
    ]

    if (nrow(seg_res) == 0L) {
      return(NULL)
    }

    seg_res <- seg_res[!is.na(predicted_time_sec)]

    # Keep best segment per athlete for this distance
    seg_res[, .SD[which.min(predicted_time_sec)], by = c(athlete_col)]
  })

  final <- data.table::rbindlist(all_results, use.names = TRUE)

  if (nrow(final) == 0L) {
    return(.empty_fastest_result(athlete_col))
  }

  final[, avg_speed_kmh := avg_speed_mps * 3.6]
  final[, segment_id := NULL]
  final[]
}

#' Create empty result data.table with correct schema
#' @noRd
.empty_fastest_result <- function(athlete_col) {
  dt <- data.table::data.table(
    athlete = integer(),
    distance_m_target = numeric(),
    predicted_time_sec = numeric(),
    avg_speed_mps = numeric(),
    bearing_deg = numeric(),
    start_time = .POSIXct(numeric(), tz = "UTC"),
    end_time = .POSIXct(numeric(), tz = "UTC"),
    straight_distance_m = numeric(),
    cumulative_distance_m = numeric(),
    sinuosity = numeric(),
    start_x = numeric(),
    start_y = numeric(),
    end_x = numeric(),
    end_y = numeric(),
    avg_speed_kmh = numeric()
  )
  data.table::setnames(dt, "athlete", athlete_col)
  dt
}
