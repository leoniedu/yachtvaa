# ------------------------------------------------------------------
# Data acquisition: wrappers for rtreinus and simcostar
# ------------------------------------------------------------------

#' Fetch paddler GPS records for a training session
#'
#' Retrieves exercises for the given date and athletes, then extracts GPS
#' records and attaches athlete metadata.
#'
#' @param session A `treinus_session` object from [rtreinus::treinus_auth()].
#'   Optional; when `NULL` (default), cached data is used.
#' @param date A Date or character date (YYYY-MM-DD) to filter exercises.
#' @param athlete_ids Integer vector of athlete IDs to query. Default 1:70.
#' @param start_time Character HH:MM lower bound for session start. Default "05:00".
#' @param end_time Character HH:MM upper bound for session start. Default "09:00".
#' @param use_db If `TRUE` (default), use rtreinus's local SQLite DB for
#'   exercise lists.
#' @param overwrite_db If `TRUE`, overwrite existing DB entries with fresh API
#'   data. Default `FALSE`.
#' @param cache_analysis If `TRUE` (default), use rtreinus's RDS cache for
#'   exercise analysis (GPS records).
#' @return A tibble of GPS records with `id_athlete`, `id_exercise`,
#'   `fullname_athlete`, and all columns from [rtreinus::treinus_extract_records()].
#' @export
fetch_paddler_records <- function(
  session = NULL,
  date = Sys.Date(),
  athlete_ids = 1:70,
  start_time = "05:00",
  end_time = "09:00",
  use_db = TRUE,
  overwrite_db = FALSE,
  cache_analysis = TRUE
) {
  date <- as.Date(date)

  cli::cli_inform("Fetching exercises for {date}...")
  exercises_list <- purrr::map(athlete_ids, function(athlete_id) {
    rtreinus::treinus_get_exercises(
      athlete_id = athlete_id,
      session = session,
      use_db = use_db,
      overwrite_db = overwrite_db
    )
  })
  exercises <- purrr::list_rbind(exercises_list)

  exercises_filtered <- exercises |>
    dplyr::filter(
      as.Date(.data$start) == .env$date,
      .data$start_time_as_string >= .env$start_time,
      .data$start_time_as_string <= .env$end_time
    )

  if (nrow(exercises_filtered) == 0L) {
    cli::cli_warn(
      "No exercises found for {date} between {start_time}-{end_time}."
    )
    return(tibble::tibble())
  }

  cli::cli_inform(
    "Found {nrow(exercises_filtered)} exercise(s). Fetching analysis..."
  )

  analyses <- purrr::pmap(
    exercises_filtered,
    function(id_exercise, id_athlete, ...) {
      rtreinus::treinus_get_exercise_analysis(
        exercise_id = id_exercise,
        athlete_id = id_athlete,
        session = session,
        cache = cache_analysis
      )
    }
  )

  records <- purrr::map(analyses, function(a) {
    tibble::tibble(
      id_athlete = a$data$Analysis$IdAthlete,
      id_exercise = a$data$Analysis$IdExercise,
      fullname_athlete = a$data$Analysis$User$FullName,
      rtreinus::treinus_extract_records(a)
    )
  }) |>
    purrr::list_rbind()

  records$fullname_athlete <- .fix_unicode_escapes(records$fullname_athlete)

  cli::cli_inform(
    "Extracted {nrow(records)} GPS records from {length(analyses)} athlete(s)."
  )
  records
}

#' Fix literal Unicode escape sequences in character vectors
#'
#' Replaces `<U+XXXX>` patterns (produced by some JSON parsers under
#' non-UTF-8 locales) with the corresponding UTF-8 characters.
#' @param x Character vector.
#' @return Character vector with escapes resolved.
#' @noRd
.fix_unicode_escapes <- function(x) {
  x <- enc2utf8(x)
  m <- gregexpr("<U\\+([0-9A-Fa-f]{4,6})>", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(escapes) {
    if (length(escapes) == 0L) {
      return(escapes)
    }
    hex <- sub("<U\\+([0-9A-Fa-f]+)>", "\\1", escapes)
    vapply(strtoi(hex, 16L), intToUtf8, character(1L))
  })
  x
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
  endpoint = c("standard", "currents")
) {
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
#' automatic time window padding for buoy data. Downstream packages handle
#' their own caching (rtreinus for exercises/analysis, simcostar for buoy).
#'
#' @param session A `treinus_session` object from [rtreinus::treinus_auth()].
#' @param date A Date or character date (YYYY-MM-DD) to filter exercises.
#' @param athlete_ids Integer vector of athlete IDs to query. Default 1:70.
#' @param start_time Character HH:MM lower bound for session start.
#' @param end_time Character HH:MM upper bound for session start.
#' @param boia_id Integer buoy ID. Default 515.
#' @param buoy_pad_sec Seconds to pad before/after session for buoy fetch.
#'   Default 3600 (1 hour).
#' @inheritParams fetch_paddler_records
#' @return A list with elements:
#'   - `records`: GPS records tibble
#'   - `buoy`: buoy data frame
#' @export
fetch_session_data <- function(
  session = NULL,
  date = Sys.Date(),
  athlete_ids = 1:70,
  start_time = "05:00",
  end_time = "09:00",
  boia_id = BUOY_ID_DEFAULT,
  buoy_pad_sec = BUOY_TIME_PAD_SEC,
  use_db = TRUE,
  overwrite_db = FALSE,
  cache_analysis = TRUE
) {
  records <- fetch_paddler_records(
    session = session,
    date = date,
    athlete_ids = athlete_ids,
    start_time = start_time,
    end_time = end_time,
    use_db = use_db,
    overwrite_db = overwrite_db,
    cache_analysis = cache_analysis
  )

  # Build buoy time window from session date + time bounds + padding
  date <- as.Date(date)
  buoy_start <- as.POSIXct(
    paste(date, start_time),
    tz = "UTC"
  ) -
    buoy_pad_sec
  buoy_end <- as.POSIXct(
    paste(date, end_time),
    tz = "UTC"
  ) +
    buoy_pad_sec

  buoy <- fetch_buoy_data(
    start = buoy_start,
    end = buoy_end,
    boia_id = boia_id
  )

  list(records = records, buoy = buoy)
}
