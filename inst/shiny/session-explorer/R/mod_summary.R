# Module: Summary (Atletas tab)
# Value boxes + paddler table

mod_summary_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::layout_columns(
      col_widths = c(3, 3, 3, 3),
      bslib::value_box(
        title = "Atletas na \u00e1gua",
        value = shiny::textOutput(ns("n_paddlers")),
        theme = "primary",
        showcase = shiny::icon("users")
      ),
      bslib::value_box(
        title = "Dura\u00e7\u00e3o m\u00e9dia",
        value = shiny::textOutput(ns("avg_duration")),
        theme = "info",
        showcase = shiny::icon("clock")
      ),
      bslib::value_box(
        title = "Mais r\u00e1pido",
        value = shiny::textOutput(ns("winner")),
        theme = "success",
        showcase = shiny::icon("trophy")
      ),
      bslib::value_box(
        title = "Corrente m\u00e9dia (SISCORAR)",
        value = shiny::textOutput(ns("avg_current")),
        theme = "warning",
        showcase = shiny::icon("water")
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

    output$winner <- shiny::renderText({
      req(rv$league_fmt)
      lf <- rv$league_fmt
      secs <- lf$predicted_time_sec[1]
      sprintf(
        "%s (%d:%02d)",
        lf$fullname_athlete[1],
        floor(secs / 60), round(secs %% 60)
      )
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

    output$paddler_table <- reactable::renderReactable({
      req(rv$paddlers, selected_athletes())

      sel_ids <- as.integer(selected_athletes())
      tbl <- rv$paddlers |>
        dplyr::filter(id_athlete %in% sel_ids) |>
        dplyr::arrange(dplyr::desc(track_distance_m)) |>
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
        dplyr::arrange(dplyr::desc(speed_kmh))

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
