# Module: Hourly Current Map
# Faceted ggplot: current arrows + fastest segments per hour

mod_hourly_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      "Trechos mais r\u00e1pidos por hora (com campo de corrente SISCORAR)"
    ),
    bslib::card_body(
      fillable = TRUE,
      shiny::plotOutput(ns("hourly_plot"), height = "800px")
    )
  )
}

mod_hourly_map_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    output$hourly_plot <- shiny::renderPlot({
      req(
        rv$grib_cropped, rv$grib_data, rv$records_sf,
        rv$fastx, rv$conditions
      )
      rlang::check_installed(c("ggplot2", "ggrepel"))

      grib_times <- rv$grib_data$grib_times
      n_hours    <- rv$grib_data$n_hours

      # Only show hours that have fastest segments (athletes in the water)
      seg_hours_utc <- as.integer(format(rv$fastx$start_time, "%H", tz = "UTC"))
      active_hour_idx <- sort(unique(pmin(pmax(seg_hours_utc, 0L), n_hours - 1L) + 1L))
      if (length(active_hour_idx) == 0) return(NULL)
      training_idx <- active_hour_idx

      # Build current arrows for each hour
      all_arrows <- purrr::map(training_idx, function(h_idx) {
        current_field_arrows(
          rv$grib_cropped$u_study,
          rv$grib_cropped$v_study,
          h_idx,
          rv$records_sf
        ) |>
          dplyr::mutate(
            hour = format(
              lubridate::with_tz(grib_times[h_idx], "America/Bahia"),
              "%Hh"
            )
          )
      }) |>
        purrr::list_rbind()

      # Assign segments to hours
      segments_hourly <- rv$fastx |>
        tibble::as_tibble() |>
        dplyr::mutate(
          hour_idx = pmin(pmax(as.integer(format(start_time, "%H", tz = "UTC")), 0L), n_hours - 1L) + 1L,
          hour     = format(
            lubridate::with_tz(grib_times[hour_idx], "America/Bahia"),
            "%Hh"
          )
        ) |>
        dplyr::filter(hour %in% unique(all_arrows$hour))

      # Plot
      p <- ggplot2::ggplot() +
        # Current arrows
        ggplot2::geom_segment(
          data = all_arrows,
          ggplot2::aes(
            x = x, y = y, xend = xend, yend = yend,
            colour = speed_kmh
          ),
          arrow = ggplot2::arrow(
            length = ggplot2::unit(0.1, "cm"), type = "closed"
          ),
          linewidth = 0.4, alpha = 0.7
        ) +
        ggplot2::scale_colour_viridis_c(
          option = "mako", name = "Corrente (km/h)"
        )

      # Fastest segments (use fill for second scale)
      if (nrow(segments_hourly) > 0) {
        p <- p +
          ggnewscale::new_scale_colour() +
          ggplot2::geom_segment(
            data = segments_hourly,
            ggplot2::aes(
              x = start_x, y = start_y,
              xend = end_x, yend = end_y,
              colour = avg_speed_kmh
            ),
            arrow = ggplot2::arrow(
              length = ggplot2::unit(0.25, "cm"), type = "closed"
            ),
            linewidth = 1.2
          ) +
          ggplot2::scale_colour_viridis_c(
            option = "plasma", name = "Vel. atleta (km/h)"
          ) +
          ggrepel::geom_text_repel(
            data = segments_hourly,
            ggplot2::aes(
              x = (start_x + end_x) / 2,
              y = (start_y + end_y) / 2,
              label = fullname_athlete
            ),
            size = 2.5, max.overlaps = 15,
            bg.color = "white", bg.r = 0.15
          )
      }

      p +
        ggplot2::facet_wrap(~hour, ncol = 3) +
        ggplot2::coord_sf(crs = sf::st_crs(rv$records_sf)) +
        ggplot2::labs(
          title = "Campo de correntes e trechos mais r\u00e1pidos por hora"
        ) +
        ggplot2::theme_void(base_size = 11) +
        ggplot2::theme(
          legend.position  = "bottom",
          plot.title       = ggplot2::element_text(face = "bold"),
          strip.text       = ggplot2::element_text(size = 12, face = "bold"),
          plot.margin      = ggplot2::margin(5, 5, 5, 5)
        )
    })
  })
}
