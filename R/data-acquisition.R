# ------------------------------------------------------------------
# Data acquisition: wrappers for treinusr and simcostar
# ------------------------------------------------------------------

#' Fetch paddler GPS records for a training session
#'
#' Authenticates with Treinus, retrieves exercises for the given date and
#' athletes, then extracts GPS records and attaches athlete metadata.
#'
#' @param session A `treinus_session` object from [treinusr::treinus_auth()].
#' @param date A Date or character date (YYYY-MM-DD) to filter exercises.
#' @param athlete_ids Integer vector of athlete IDs to query. Default 1:70.
#' @param start_time Character HH:MM lower bound for session start. Default "05:00".
#' @param end_time Character HH:MM upper bound for session start. Default "09:00".
#' @return A tibble of GPS records with `id_athlete`, `id_exercise`,
#'   `fullname_athlete`, and all columns from [treinusr::treinus_extract_records()].
#' @export
fetch_paddler_records <- function(
    session,
    date = Sys.Date(),
    athlete_ids = 1:70,
    start_time = "05:00",
    end_time = "09:00") {
  date <- as.Date(date)

  cli::cli_inform("Fetching exercises for {date}...")
  exercises_list <- purrr::map(athlete_ids, function(athlete_id) {
    treinusr::treinus_get_exercises(session, athlete_id = athlete_id)
  })
  exercises <- purrr::list_rbind(exercises_list)

  exercises_filtered <- exercises |>
    dplyr::filter(
      as.Date(.data$start) == .env$date,
      .data$start_time_as_string >= .env$start_time,
      .data$start_time_as_string <= .env$end_time,
      !is.na(.data$speed)
    )

  if (nrow(exercises_filtered) == 0L) {
    cli::cli_warn("No exercises found for {date} between {start_time}-{end_time}.")
    return(tibble::tibble())
  }

  cli::cli_inform("Found {nrow(exercises_filtered)} exercise(s). Fetching analysis...")

  analyses <- purrr::pmap(
    exercises_filtered,
    function(id_exercise, id_athlete, ...) {
      treinusr::treinus_get_exercise_analysis(
        session,
        exercise_id = id_exercise,
        athlete_id = id_athlete
      )
    }
  )

  records <- purrr::map(analyses, function(a) {
    tibble::tibble(
      id_athlete = a$data$Analysis$IdAthlete,
      id_exercise = a$data$Analysis$IdExercise,
      fullname_athlete = a$data$Analysis$User$FullName,
      treinusr::treinus_extract_records(a)
    )
  }) |>
    purrr::list_rbind()

  cli::cli_inform("Extracted {nrow(records)} GPS records from {length(analyses)} athlete(s).")
  records
}

#' Fetch buoy environmental data
#'
#' Thin wrapper around `simcostar::simcosta_fetch()` for the default buoy.
#'
#' @param start Start time (POSIXct, Date, character, or numeric Unix timestamp).
#' @param end End time (same types as start).
#' @param boia_id Integer buoy ID. Default 515.
#' @param endpoint Character vector of endpoints. Default both "standard" and
#'   "currents".
#' @return A data frame of wide-format buoy measurements with a `datetime` column.
#' @export
fetch_buoy_data <- function(
    start,
    end,
    boia_id = BUOY_ID_DEFAULT,
    endpoint = c("standard", "currents")) {
  cli::cli_inform("Fetching buoy {boia_id} data...")
  simcostar::simcosta_fetch(
    boia_id = boia_id,
    start = start,
    end = end,
    endpoint = endpoint,
    wide = TRUE
  )
}

#' Fetch both paddler and buoy data for a session
#'
#' Orchestrates [fetch_paddler_records()] and [fetch_buoy_data()] with
#' automatic time window padding for buoy data.
#'
#' @inheritParams fetch_paddler_records
#' @param boia_id Integer buoy ID. Default 515.
#' @param buoy_pad_sec Seconds to pad before/after session for buoy fetch.
#'   Default 3600 (1 hour).
#' @return A list with elements:
#'   - `records`: GPS records tibble
#'   - `buoy`: buoy data frame
#' @export
fetch_session_data <- function(
    session,
    date = Sys.Date(),
    athlete_ids = 1:70,
    start_time = "05:00",
    end_time = "09:00",
    boia_id = BUOY_ID_DEFAULT,
    buoy_pad_sec = BUOY_TIME_PAD_SEC) {
  records <- fetch_paddler_records(
    session = session,
    date = date,
    athlete_ids = athlete_ids,
    start_time = start_time,
    end_time = end_time
  )

  # Build buoy time window from session date + time bounds + padding
  date <- as.Date(date)
  buoy_start <- as.POSIXct(
    paste(date, start_time),
    tz = "UTC"
  ) - buoy_pad_sec
  buoy_end <- as.POSIXct(
    paste(date, end_time),
    tz = "UTC"
  ) + buoy_pad_sec

  buoy <- fetch_buoy_data(
    start = buoy_start,
    end = buoy_end,
    boia_id = boia_id
  )

  list(records = records, buoy = buoy)
}
