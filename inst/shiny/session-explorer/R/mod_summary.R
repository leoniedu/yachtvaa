# Module: Summary (Atletas tab)
# Value boxes + paddler table

mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    gap = "0.35rem",
    # Row 1: session overview
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      gap = "0.35rem",
      bslib::value_box(
        title = "Atletas na \u00e1gua",
        value = shiny::textOutput(ns("n_paddlers")),
        theme = "primary"
      ),
      bslib::value_box(
        title = "Dura\u00e7\u00e3o m\u00e9dia",
        value = shiny::textOutput(ns("avg_duration")),
        theme = "info"
      ),
      bslib::value_box(
        title = "Corrente m\u00e9dia (SISCORAR)",
        value = shiny::textOutput(ns("avg_current")),
        theme = "warning"
      )
    ),
    # Row 2: top performers
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      gap = "0.35rem",
      bslib::value_box(
        title = shiny::uiOutput(ns("winner_title")),
        value = shiny::uiOutput(ns("winner")),
        theme = "success"
      ),
      bslib::value_box(
        title = "Maior dist\u00e2ncia",
        value = shiny::uiOutput(ns("longest_paddle")),
        theme = "secondary"
      ),
      bslib::value_box(
        title = "Maior vel. m\u00e9dia",
        value = shiny::uiOutput(ns("top_speed")),
        theme = "danger"
      )
    ),
    bslib::card(
      bslib::card_header("Atletas identificados na \u00e1gua"),
      reactable::reactableOutput(ns("paddler_table"))
    )
  )
}

mod_summary_server <- function(id, rv, selected_athletes) {
  shiny::moduleServer(id, function(input, output, session) {

    output$n_paddlers <- shiny::renderText({
      req(rv$paddlers, selected_athletes())
      length(selected_athletes())
    })

    output$avg_duration <- shiny::renderText({
      req(rv$paddlers, selected_athletes())
      sel_ids <- as.integer(selected_athletes())
      avg <- mean(
        rv$paddlers[rv$paddlers$id_athlete %in% sel_ids, ]$duration_min,
        na.rm = TRUE
      )
      if (is.na(avg) || !is.finite(avg)) return("--")
      sprintf("%d:%02d", floor(avg / 60), round(avg %% 60))
    })

    output$avg_current <- shiny::renderText({
      req(rv$grib_hourly)
      records_time_range <- range(rv$records_sf$timestamp)
      gh <- rv$grib_hourly |>
        dplyr::filter(
          datetime >= records_time_range[1],
          datetime <= records_time_range[2]
        )
      if (nrow(gh) == 0) return("--")
      sprintf("%.1f km/h", mean(gh$current_speed_kmh, na.rm = TRUE))
    })

    output$winner_title <- shiny::renderUI({
      req(rv$league_fmt)
      dist <- rv$league_fmt$distance_m_target[1]
      shiny::HTML(paste0("Mais r\u00e1pido (", as.integer(dist), " metros)"))
    })

    output$winner <- shiny::renderUI({
      req(rv$league_fmt)
      lf <- rv$league_fmt
      secs <- lf$predicted_time_sec[1]
      shiny::tagList(
        shiny::tags$span(
          sprintf("%d:%02d", floor(secs / 60), round(secs %% 60)),
          style = "font-size: 0.75em; line-height: 1;"
        ),
        shiny::tags$p(
          lf$fullname_athlete[1],
          style = "font-size: 0.8rem; margin: 0.2em 0 0 0; opacity: 0.85; line-height: 1.2;"
        )
      )
    })

    output$longest_paddle <- shiny::renderUI({
      req(rv$paddlers, selected_athletes())
      sel_ids <- as.integer(selected_athletes())
      p <- rv$paddlers |>
        dplyr::filter(id_athlete %in% sel_ids) |>
        dplyr::slice_max(track_distance_m, n = 1, with_ties = FALSE)
      if (nrow(p) == 0) return("--")
      shiny::tagList(
        shiny::tags$span(
          sprintf("%.1f km", p$track_distance_m[1] / 1000),
          style = "font-size: 0.75em; line-height: 1;"
        ),
        shiny::tags$p(
          p$fullname_athlete[1],
          style = "font-size: 0.8rem; margin: 0.2em 0 0 0; opacity: 0.85; line-height: 1.2;"
        )
      )
    })

    output$top_speed <- shiny::renderUI({
      req(rv$paddlers, selected_athletes())
      sel_ids <- as.integer(selected_athletes())
      p <- rv$paddlers |>
        dplyr::filter(id_athlete %in% sel_ids) |>
        dplyr::mutate(speed_kmh = (track_distance_m / duration_min) * 60 / 1000) |>
        dplyr::slice_max(speed_kmh, n = 1, with_ties = FALSE)
      if (nrow(p) == 0) return("--")
      shiny::tagList(
        shiny::tags$span(
          sprintf("%.1f km/h", p$speed_kmh[1]),
          style = "font-size: 0.75em; line-height: 1;"
        ),
        shiny::tags$p(
          p$fullname_athlete[1],
          style = "font-size: 0.8rem; margin: 0.2em 0 0 0; opacity: 0.85; line-height: 1.2;"
        )
      )
    })

    output$paddler_table <- reactable::renderReactable({
      req(rv$paddlers, selected_athletes())

      sel_ids <- as.integer(selected_athletes())
      tbl <- rv$paddlers |>
        dplyr::filter(id_athlete %in% sel_ids) |>
        dplyr::mutate(
          track_distance_km = round(track_distance_m / 1000, 1),
          duration_fmt = sprintf(
            "%d:%02d",
            floor(duration_min / 60), round(duration_min %% 60)
          ),
          speed_kmh = round((track_distance_m / duration_min) * 60 / 1000, 1),
          first_fix_fmt = format(
            lubridate::with_tz(first_fix, "America/Bahia"), "%H:%M"
          )
        ) |>
        dplyr::select(
          id_athlete, fullname_athlete, first_fix_fmt,
          duration_fmt, track_distance_km, speed_kmh
        ) |>
        dplyr::arrange(fullname_athlete)

      reactable::reactable(
        tbl,
        columns = list(
          id_athlete        = reactable::colDef(name = "ID"),
          fullname_athlete  = reactable::colDef(name = "Atleta", minWidth = 180),
          first_fix_fmt     = reactable::colDef(name = "In\u00edcio"),
          duration_fmt      = reactable::colDef(name = "Dura\u00e7\u00e3o"),
          track_distance_km = reactable::colDef(name = "Dist\u00e2ncia (km)"),
          speed_kmh         = reactable::colDef(name = "Vel. (km/h)")
        ),
        defaultPageSize = 50,
        compact = TRUE,
        striped = TRUE,
        highlight = TRUE
      )
    })
  })
}
