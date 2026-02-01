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
#' @param use_db If `TRUE` (default), use treinusr's local SQLite DB for
#'   exercise lists.
#' @param overwrite_db If `TRUE`, overwrite existing DB entries with fresh API
#'   data. Default `FALSE`.
#' @param use_memoise If `TRUE` (default), use treinusr's in-session memoise
#'   cache for exercise lists.
#' @param cache_analysis If `TRUE` (default), use treinusr's RDS cache for
#'   exercise analysis (GPS records).
#' @return A tibble of GPS records with `id_athlete`, `id_exercise`,
#'   `fullname_athlete`, and all columns from [treinusr::treinus_extract_records()].
#' @export
fetch_paddler_records <- function(
  session,
  date = Sys.Date(),
  athlete_ids = 1:70,
  start_time = "05:00",
  end_time = "09:00",
  use_db = TRUE,
  overwrite_db = FALSE,
  use_memoise = TRUE,
  cache_analysis = TRUE
) {
  date <- as.Date(date)

  cli::cli_inform("Fetching exercises for {date}...")
  exercises_list <- purrr::map(athlete_ids, function(athlete_id) {
    treinusr::treinus_get_exercises(
      session,
      athlete_id = athlete_id,
      use_db = use_db,
      overwrite_db = overwrite_db,
      use_memoise = use_memoise
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
      treinusr::treinus_get_exercise_analysis(
        session,
        exercise_id = id_exercise,
        athlete_id = id_athlete,
        cache = cache_analysis
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
#' automatic time window padding for buoy data.
#'
#' When `cache = TRUE` the result is loaded from an RDS file on disk and
#' no API calls (or authentication) are made.
#' When `cache = FALSE` (the default) data is fetched from the APIs and
#' the result is saved to disk so that subsequent calls with
#' `cache = TRUE` can skip authentication entirely.
#'
#' @param session A `treinus_session` object from [treinusr::treinus_auth()].
#'   Not required when `cache = TRUE`.
#' @param date A Date or character date (YYYY-MM-DD) to filter exercises.
#' @param athlete_ids Integer vector of athlete IDs to query. Default 1:70.
#' @param start_time Character HH:MM lower bound for session start.
#' @param end_time Character HH:MM upper bound for session start.
#' @param boia_id Integer buoy ID. Default 515.
#' @param buoy_pad_sec Seconds to pad before/after session for buoy fetch.
#'   Default 3600 (1 hour).
#' @param cache If `TRUE`, load previously cached results from disk instead
#'   of calling the APIs. Default `FALSE`.
#' @inheritParams fetch_paddler_records
#' @return A list with elements:
#'   - `records`: GPS records tibble
#'   - `buoy`: buoy data frame
#' @seealso [clear_session_cache()]
#' @export
fetch_session_data <- function(
  session,
  date = Sys.Date(),
  athlete_ids = 1:70,
  start_time = "05:00",
  end_time = "09:00",
  boia_id = BUOY_ID_DEFAULT,
  buoy_pad_sec = BUOY_TIME_PAD_SEC,
  cache = FALSE,
  use_db = TRUE,
  overwrite_db = FALSE,
  use_memoise = TRUE,
  cache_analysis = TRUE
) {
  cache_path <- .session_cache_path(
    date,
    athlete_ids,
    start_time,
    end_time,
    boia_id
  )

  if (cache) {
    if (!file.exists(cache_path)) {
      cli::cli_abort(c(
        "No cached data found for this session.",
        "i" = "Run with {.arg cache = FALSE} first to fetch and cache the data.",
        "i" = "Expected path: {.path {cache_path}}"
      ))
    }
    cli::cli_inform("Loading cached session data from {.path {cache_path}}")
    return(readRDS(cache_path))
  }

  records <- fetch_paddler_records(
    session = session,
    date = date,
    athlete_ids = athlete_ids,
    start_time = start_time,
    end_time = end_time,
    use_db = use_db,
    overwrite_db = overwrite_db,
    use_memoise = use_memoise,
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

  result <- list(records = records, buoy = buoy)

  dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
  saveRDS(result, cache_path)
  cli::cli_inform("Session data cached to {.path {cache_path}}")

  result
}

#' Build the cache file path for a session
#'
#' @noRd
.session_cache_path <- function(
  date,
  athlete_ids,
  start_time,
  end_time,
  boia_id
) {
  date <- as.Date(date)
  id_range <- paste(range(athlete_ids), collapse = "-")
  # Sanitise colons from time strings for safe filenames
  st <- gsub(":", "", start_time)
  et <- gsub(":", "", end_time)
  filename <- sprintf(
    "session_%s_%s_%s_%s_%s.rds",
    date,
    id_range,
    st,
    et,
    boia_id
  )
  file.path(tools::R_user_dir("yachtvaa", "cache"), filename)
}

#' Clear the on-disk session cache
#'
#' Removes cached RDS files so the next [fetch_session_data()] call
#' re-fetches from the APIs.
#'
#' @param date Optional date to clear a specific session cache.
#'   If `NULL` (default), all cached sessions are removed.
#' @return Invisible character vector of removed file paths.
#' @export
clear_session_cache <- function(date = NULL) {
  cache_dir <- tools::R_user_dir("yachtvaa", "cache")
  if (!dir.exists(cache_dir)) {
    cli::cli_inform("No cache directory found.")
    return(invisible(character()))
  }
  if (is.null(date)) {
    files <- list.files(
      cache_dir,
      pattern = "^session_.*\\.rds$",
      full.names = TRUE
    )
  } else {
    files <- list.files(
      cache_dir,
      pattern = paste0("^session_", as.Date(date), "_"),
      full.names = TRUE
    )
  }
  if (length(files) == 0L) {
    cli::cli_inform("No cached sessions found.")
    return(invisible(character()))
  }
  file.remove(files)
  cli::cli_inform("Removed {length(files)} cached session file{?s}.")
  invisible(files)
}
