# ------------------------------------------------------------------
# League table: rank athletes by fastest predicted time
# ------------------------------------------------------------------

#' Build a league table from fastest-distance segments
#'
#' Ranks athletes by predicted time for each date and target distance,
#' keeping the top performers.
#'
#' @param segments A data.table from [fastest_straight_distance()] or
#'   [apparent_conditions()]. Must contain `predicted_time_sec` and
#'   `distance_m_target` columns.
#' @param athlete_col Character name of the column identifying athletes.
#' @param date_col Character name of a Date or POSIXct column for grouping
#'   by session date. If NULL, no date grouping is applied.
#' @return A data.table with columns from the input plus `rank` (integer),
#'   ordered by rank within each distance (and date, if provided).
#' @export
build_league_table <- function(
    segments,
    athlete_col = "fullname_athlete",
    date_col = NULL) {
  dt <- data.table::as.data.table(segments)

  group_cols <- "distance_m_target"
  if (!is.null(date_col)) {
    dt[, date := as.Date(.SD[[date_col]])]
    group_cols <- c("date", group_cols)
  }

  dt[
    order(predicted_time_sec),
    rank := seq_len(.N),
    by = group_cols
  ]

  data.table::setorderv(dt, c(group_cols, "rank"))
  dt[]
}

#' Format league table times for display
#'
#' Converts `predicted_time_sec` to MM:SS.s format and optionally
#' subsets to top N athletes per group.
#'
#' @param league A data.table from [build_league_table()].
#' @param top_n Integer maximum rank to display. Default 10.
#' @return A tibble with `predicted_time_fmt` column added and filtered
#'   to top N per group.
#' @export
format_league <- function(league, top_n = 10L) {
  dt <- data.table::as.data.table(league)
  dt <- dt[rank <= top_n]

  dt[, predicted_time_fmt := {
    mins <- floor(predicted_time_sec / 60)
    secs <- predicted_time_sec - mins * 60
    sprintf("%d:%04.1f", mins, secs)
  }]

  tibble::as_tibble(dt)
}
