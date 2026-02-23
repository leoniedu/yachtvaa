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

# Simple in-memory cache (avoid disk cache issues)
.exercise_cache <- new.env(hash = TRUE, parent = emptyenv())

.get_exercises_cached <- function(athlete_id, session = NULL, use_db = FALSE) {
  cache_key <- paste0("athlete_", athlete_id)

  if (exists(cache_key, envir = .exercise_cache, inherits = FALSE)) {
    return(get(cache_key, envir = .exercise_cache))
  }

  result <- rtreinus::treinus_get_exercises(
    athlete_id = athlete_id,
    session = session,
    use_db = use_db
  )

  assign(cache_key, result, envir = .exercise_cache)
  result
}

mod_data_loader_server <- function(id, input, rv) {
  observeEvent(input$load_btn, {
    withProgress(message = "Carregando dados...", value = 0, {
      # Clear in-memory cache if force refresh is enabled
      if (isTRUE(input$force_refresh)) {
        rm(list = ls(envir = .exercise_cache), envir = .exercise_cache)
      }

      # Auth: always authenticate; force_refresh only clears the in-memory cache
      incProgress(0.05, detail = "Autenticando...")
      session_treinus <- tryCatch(
        rtreinus::treinus_auth(),
        error = function(e) {
          showNotification(
            paste("Erro de autentica\u00e7\u00e3o:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      # Convert input$date to proper Date
      the_date <- as.Date(input$date, origin = "1970-01-01")
      class(the_date) <- "Date"

      # ---------------------------------------------------------------
      # 1) Get exercise lists for all athletes (lightweight metadata)
      # ---------------------------------------------------------------
      incProgress(0.1, detail = "Buscando lista de treinos...")
      exercises <- tryCatch(
        {
          purrr::map(1:70, function(aid) {
            .get_exercises_cached(
              athlete_id = aid,
              session = session_treinus,
              use_db = FALSE
            )
          }) |>
            purrr::list_rbind()
        },
        error = function(e) {
          showNotification(
            paste("Erro ao buscar treinos:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
      req(exercises)

      # ---------------------------------------------------------------
      # 2) Filter by date only -> load all exercises for the day
      # ---------------------------------------------------------------
      exercises_filtered <- dplyr::filter(
        exercises,
        as.Date(.data$start) == .env$the_date
      )

      if (nrow(exercises_filtered) == 0L) {
        showNotification(
          sprintf("Nenhum treino encontrado em %s", the_date),
          type = "warning",
          duration = 5
        )
        return()
      }
      showNotification(
        sprintf(
          "%d treino(s) de %d atleta(s)",
          nrow(exercises_filtered),
          length(unique(exercises_filtered$id_athlete))
        ),
        type = "message",
        duration = 3
      )

      # ---------------------------------------------------------------
      # 3) Load GPS records only for filtered exercises
      # ---------------------------------------------------------------
      incProgress(0.2, detail = "Buscando GPS...")
      analyses <- tryCatch(
        {
          purrr::pmap(
            exercises_filtered,
            function(id_exercise, id_athlete, ...) {
              rtreinus::treinus_get_exercise_analysis(
                exercise_id = id_exercise,
                athlete_id = id_athlete,
                session = session_treinus,
                cache = TRUE
              )
            }
          )
        },
        error = function(e) {
          showNotification(
            paste("Erro ao buscar GPS:", conditionMessage(e)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )
      req(analyses)

      records <- purrr::map(analyses, function(a) {
        tibble::tibble(
          id_athlete = a$data$Analysis$IdAthlete,
          id_exercise = a$data$Analysis$IdExercise,
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
      # 5) Buoy data (full day, simcostar caches to SQLite)
      # ---------------------------------------------------------------
      incProgress(0.1, detail = "Buscando boia (dia completo)...")
      buoy <- tryCatch(
        simcostar::simcosta_fetch(
          boia_id = 515L,
          start = as.POSIXct(paste(the_date, "00:00:00"), tz = "UTC"),
          end = as.POSIXct(paste(the_date + 1, "00:00:00"), tz = "UTC"),
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
            resolution = 0.001
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
}
