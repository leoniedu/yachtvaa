# Module: Data loader
# Handles Phase 1 data fetching (Treinus, buoy, GRIB)
# Uses rtreinus, simcostar, rsiscorar directly (no yachtvaa wrappers)

# Constants
SEMICIRCLE_TO_DEG <- 180 / 2^31
CRS_UTM24S <- 31984L
BBOX_SSA <- c(
  xmin = -38.61380,
  ymin = -13.00741,
  xmax = -38.46308,
  ymax = -12.81308
)

# ---------------------------------------------------------------------------
# S3-backed persistent cache (survives Connect Cloud container restarts)
# Uses AWS S3 via memoise::cache_s3() + aws.s3.
# Credentials from env vars: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY,
#   AWS_DEFAULT_REGION (e.g. "us-east-1")
# Falls back to in-memory cache if aws.s3 is unavailable or credentials absent.
# ---------------------------------------------------------------------------
.make_s3_cache <- function(bucket = "yachtvaa-cache") {
  if (!requireNamespace("aws.s3", quietly = TRUE)) return(NULL)
  if (nchar(Sys.getenv("AWS_ACCESS_KEY_ID")) == 0L) return(NULL)
  tryCatch(
    memoise::cache_s3(bucket),
    error = function(e) {
      message("S3 cache unavailable, using in-memory: ", conditionMessage(e))
      NULL
    }
  )
}

.s3_cache <- .make_s3_cache()

# ---------------------------------------------------------------------------
# Global memoised auth + exercise list (shared across all sessions)
#   .get_treinus_session  : re-authenticates at most once per hour (in-memory)
#   .fetch_all_exercises  : keyed by date — auto-refetches on a new day;
#                           S3-backed so survives restarts; button calls forget()
#   .get_exercise_analysis: GPS data keyed by (exercise_id, athlete_id);
#                           S3-backed so never re-downloaded after first fetch
# ---------------------------------------------------------------------------
.get_treinus_session <- memoise::memoise(
  rtreinus::treinus_auth,
  cache = cachem::cache_mem(max_age = 3600)   # always in-memory; tokens are ephemeral
)

.fetch_all_exercises <- memoise::memoise(
  function(today) {   # today acts as the cache key: new day = new fetch
    session <- .get_treinus_session()
    purrr::map(1:70, \(aid)
      rtreinus::treinus_get_exercises(
        athlete_id = aid,
        session    = session,
        use_db     = TRUE
      )
    ) |> purrr::list_rbind()
  },
  cache = if (!is.null(.s3_cache)) .s3_cache else cachem::cache_mem()
)

# GPS analysis: keyed by (exercise_id, athlete_id) — content never changes.
# session is fetched internally so it is not part of the cache key.
.get_exercise_analysis <- memoise::memoise(
  function(exercise_id, athlete_id) {
    session <- tryCatch(.get_treinus_session(), error = function(e) NULL)
    rtreinus::treinus_get_exercise_analysis(
      exercise_id = exercise_id,
      athlete_id  = athlete_id,
      session     = session,
      cache       = TRUE   # rtreinus also caches locally for same-session speed
    )
  },
  cache = if (!is.null(.s3_cache)) .s3_cache else cachem::cache_mem()
)

# Returns the exercise list for today.
# force = TRUE (button click) drops the cache so it re-fetches immediately.
.get_exercises <- function(force = FALSE) {
  if (force) memoise::forget(.fetch_all_exercises)
  .fetch_all_exercises(Sys.Date())
}

mod_data_loader_server <- function(id, input, rv) {

  observeEvent(input$load_btn, {
    withProgress(message = "Carregando dados...", value = 0, {

      # Convert input$date to proper Date
      the_date <- as.Date(input$date, origin = "1970-01-01")
      class(the_date) <- "Date"

      # ---------------------------------------------------------------
      # 1) Exercise list — auto-logins, cached 30 min, shared across sessions
      # ---------------------------------------------------------------
      incProgress(0.1, detail = "Buscando lista de treinos...")
      exercises <- tryCatch(
        .get_exercises(),
        error = function(e) {
          # Auth / API failed; fall back to local DB
          showNotification(
            paste("API indispon\u00edvel, usando banco local:", conditionMessage(e)),
            type = "warning", duration = 5
          )
          tryCatch(
            rtreinus::treinus_get_exercises_db(),
            error = function(e2) {
              if (grepl("no such table", conditionMessage(e2), ignore.case = TRUE)) {
                showNotification(
                  "Banco local vazio. Clique em 'Atualizar lista de treinos' para buscar da API.",
                  type = "error", duration = 15
                )
              } else {
                showNotification(
                  paste("Erro ao buscar treinos:", conditionMessage(e2)),
                  type = "error", duration = 10
                )
              }
              NULL
            }
          )
        }
      )
      req(exercises)

      # ---------------------------------------------------------------
      # 2) Filter by date
      # ---------------------------------------------------------------
      exercises_filtered <- dplyr::filter(
        exercises,
        as.Date(.data$start) == .env$the_date
      )

      if (nrow(exercises_filtered) == 0L) {
        showNotification(
          sprintf("Nenhum treino encontrado em %s", the_date),
          type = "warning", duration = 5
        )
        return()
      }
      showNotification(
        sprintf(
          "%d treino(s) de %d atleta(s)",
          nrow(exercises_filtered),
          length(unique(exercises_filtered$id_athlete))
        ),
        type = "message", duration = 3
      )

      # ---------------------------------------------------------------
      # 3) Load GPS — S3-backed memoise; falls back to rtreinus disk cache
      # ---------------------------------------------------------------
      incProgress(0.2, detail = "Buscando GPS...")

      analyses <- purrr::pmap(
        exercises_filtered,
        function(id_exercise, id_athlete, ...) {
          tryCatch(
            .get_exercise_analysis(id_exercise, id_athlete),
            error = function(e) NULL
          )
        }
      ) |>
        purrr::compact()

      if (length(analyses) == 0L) {
        showNotification(
          "GPS n\u00e3o encontrado. Clique em 'Atualizar lista de treinos' para buscar da API.",
          type = "error", duration = 10
        )
        return()
      }
      if (length(analyses) < nrow(exercises_filtered)) {
        showNotification(
          sprintf(
            "%d de %d treinos com GPS dispon\u00edvel.",
            length(analyses), nrow(exercises_filtered)
          ),
          type = "warning", duration = 5
        )
      }

      records <- purrr::map(analyses, function(a) {
        tibble::tibble(
          id_athlete = as.integer(a$data$Analysis$IdAthlete),
          id_exercise = as.integer(a$data$Analysis$IdExercise),
          fullname_athlete = a$data$Analysis$User$FullName,
          rtreinus::treinus_extract_records(a)
        )
      }) |>
        purrr::list_rbind()

      records <- records |>
        dplyr::mutate(fullname_athlete = capitalizar(fullname_athlete))

      # ---------------------------------------------------------------
      # 4) Convert to sf (semicircles -> degrees -> projected)
      # ---------------------------------------------------------------
      incProgress(0.1, detail = "Processando GPS...")
      records <- dplyr::filter(records, !is.na(.data$position_long)) |>
        dplyr::mutate(
          lat = .data$position_lat * SEMICIRCLE_TO_DEG,
          lon = .data$position_long * SEMICIRCLE_TO_DEG,
          timestamp = as.POSIXct(.data$timestamp, tz = "UTC")
        )

      records_sf <- records |>
        sf::st_as_sf(coords = c("lon", "lat"), remove = FALSE, crs = 4326L) |>
        sf::st_transform(crs = CRS_UTM24S) |>
        dplyr::mutate(
          timestamp = dplyr::if_else(
            id_athlete == 50,
            timestamp + (60 * 60 * 3),
            timestamp
          )
        )

      study_area <- sf::st_bbox(
        c(
          xmin = BBOX_SSA[["xmin"]],
          ymin = BBOX_SSA[["ymin"]],
          xmax = BBOX_SSA[["xmax"]],
          ymax = BBOX_SSA[["ymax"]]
        ),
        crs = 4326L
      ) |>
        sf::st_as_sfc() |>
        sf::st_transform(sf::st_crs(records_sf))

      rv$study_area <- study_area

      # Spatial filter
      athletes_in_bbox <- records_sf[
        lengths(sf::st_intersects(records_sf, study_area)) > 0,
        "id_athlete"
      ]
      records_sf <- records_sf[
        records_sf$id_athlete %in% athletes_in_bbox$id_athlete,
      ]

      rv$the_date <- the_date
      rv$records_sf <- records_sf
      rv$records_sf_bbox <- records_sf[
        lengths(sf::st_intersects(records_sf, study_area)) > 0,
      ]

      # Track summary -> identify valid paddlers
      track_summary <- records_sf |>
        sf::st_drop_geometry() |>
        dplyr::summarise(
          n_fixes = dplyr::n(),
          first_fix = min(timestamp),
          last_fix = max(timestamp),
          duration_min = as.numeric(difftime(
            max(timestamp),
            min(timestamp),
            units = "mins"
          )),
          .by = c(id_athlete, fullname_athlete)
        )

      track_distance <- records_sf |>
        dplyr::arrange(id_athlete, timestamp) |>
        dplyr::mutate(
          coords = sf::st_coordinates(geometry),
          x = coords[, 1],
          y = coords[, 2],
          .keep = "unused"
        ) |>
        sf::st_drop_geometry() |>
        dplyr::summarise(
          track_distance_m = sum(sqrt(diff(x)^2 + diff(y)^2)),
          .by = id_athlete
        )

      track_summary <- track_summary |>
        dplyr::left_join(track_distance, by = "id_athlete")

      rv$paddlers <- track_summary |>
        dplyr::filter(n_fixes >= 100, track_distance_m >= 200)

      # ---------------------------------------------------------------
      # 5) Buoy data (only hours when athletes were in the water)
      # ---------------------------------------------------------------
      athlete_ts_range <- range(records_sf$timestamp, na.rm = TRUE)
      buoy_start <- as.POSIXct(
        format(athlete_ts_range[1], "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )
      buoy_end <- as.POSIXct(
        format(athlete_ts_range[2] + 3599, "%Y-%m-%d %H:00:00"),
        tz = "UTC"
      )

      incProgress(0.1, detail = "Buscando boia...")
      buoy <- tryCatch(
        rsimcosta::simcosta_fetch(
          boia_id = 515L,
          start = buoy_start,
          end = buoy_end,
          endpoint = c("standard", "currents"),
          wide = TRUE
        ),
        error = function(e) {
          showNotification(
            paste("Aviso: boia parcial:", conditionMessage(e)),
            type = "warning",
            duration = 5
          )
          NULL
        }
      )

      # Buoy interpolation (regular 10-min grid + linear interpolation)
      if (!is.null(buoy) && nrow(buoy) > 0L) {
        time_range <- range(buoy$datetime, na.rm = TRUE)
        regular_grid <- tibble::tibble(
          datetime = seq.POSIXt(
            from = time_range[1],
            to = time_range[2],
            by = 600L
          )
        )
        merged <- dplyr::full_join(regular_grid, buoy, by = "datetime") |>
          dplyr::arrange(.data$datetime)
        numeric_cols <- names(merged)[purrr::map_lgl(merged, is.numeric)]
        rv$buoy_ip <- merged |>
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(numeric_cols),
              ~ zoo::na.approx(.x, na.rm = FALSE)
            )
          ) |>
          dplyr::semi_join(regular_grid, by = "datetime")
      } else {
        rv$buoy_ip <- tibble::tibble(datetime = as.POSIXct(character()))
      }

      rv$has_wind <- "wind_speed" %in%
        names(rv$buoy_ip) &&
        any(!is.na(rv$buoy_ip$wind_speed))
      rv$has_buoy_current <- "current_speed_kmh" %in%
        names(rv$buoy_ip) &&
        any(!is.na(rv$buoy_ip$current_speed_kmh))

      # ---------------------------------------------------------------
      # 6) GRIB ocean currents (SISCORAR)
      # ---------------------------------------------------------------
      incProgress(0.3, detail = "Buscando correntes SISCORAR...")
      tryCatch(
        {
          grib <- rsiscorar::get_grib(
            date = as.character(the_date),
            area = "baiatos",
            resolution = 0.001,
            fallback = FALSE
          )

          layer_names <- names(grib)
          is_u <- grepl("u-component|UOGRD", layer_names, ignore.case = TRUE)
          is_v <- grepl("v-component|VOGRD", layer_names, ignore.case = TRUE)

          u_layers <- grib[[which(is_u)]]
          v_layers <- grib[[which(is_v)]]
          n_hours <- terra::nlyr(u_layers)

          rv$grib_data <- list(
            u_layers = u_layers,
            v_layers = v_layers,
            grib_times = as.POSIXct(
              paste(
                as.character(the_date),
                sprintf("%02d:00:00", seq(0, n_hours - 1))
              ),
              tz = "UTC"
            ),
            n_hours = n_hours
          )

          # Crop to study extent
          study_bbox_4326 <- sf::st_bbox(sf::st_transform(study_area, 4326))
          records_bbox_4326 <- sf::st_bbox(sf::st_transform(records_sf, 4326))
          combined_bbox <- c(
            xmin = min(study_bbox_4326[["xmin"]], records_bbox_4326[["xmin"]]),
            xmax = max(study_bbox_4326[["xmax"]], records_bbox_4326[["xmax"]]),
            ymin = min(study_bbox_4326[["ymin"]], records_bbox_4326[["ymin"]]),
            ymax = max(study_bbox_4326[["ymax"]], records_bbox_4326[["ymax"]])
          )
          study_ext <- terra::ext(
            combined_bbox[["xmin"]],
            combined_bbox[["xmax"]],
            combined_bbox[["ymin"]],
            combined_bbox[["ymax"]]
          )

          rv$grib_cropped <- list(
            u_study = terra::crop(u_layers, study_ext),
            v_study = terra::crop(v_layers, study_ext)
          )
        },
        error = function(e) {
          showNotification(
            paste("Erro GRIB:", conditionMessage(e)),
            type = "warning",
            duration = 10
          )
        }
      )

      incProgress(0.15, detail = "Pronto!")
      showNotification(
        sprintf("%d atletas encontrados na \u00e1rea", nrow(rv$paddlers)),
        type = "message",
        duration = 5
      )
    })
  })

  # ---------------------------
  # Force-refresh the exercise list (bypasses 30-min cache)
  # ---------------------------
  observeEvent(input$refresh_exercises_btn, {
    withProgress(message = "Atualizando lista de treinos...", value = 0.2, {
      exercises <- tryCatch(
        .get_exercises(force = TRUE),
        error = function(e) {
          showNotification(
            paste("Erro ao atualizar:", conditionMessage(e)),
            type = "error", duration = 10
          )
          NULL
        }
      )
      req(exercises)
      incProgress(0.8, detail = "Pronto!")
      showNotification(
        sprintf("Lista atualizada: %d treinos no total", nrow(exercises)),
        type = "message", duration = 5
      )
    })
  })
}
