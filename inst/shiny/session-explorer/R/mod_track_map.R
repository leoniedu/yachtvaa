# Module: Track Map
# Interactive leaflet map with athlete GPS tracks and animated playback

mod_track_map_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = 12,
    bslib::card(
      bslib::card_header("Percursos dos atletas"),
      bslib::card_body(
        fillable = TRUE,
        shiny::fluidRow(
          shiny::column(
            8,
            shiny::sliderInput(
              ns("time_slider"), "Tempo de anima\u00e7\u00e3o",
              min = 0, max = 100, value = 100,
              step = 1, post = "%",
              animate = shiny::animationOptions(interval = 200, loop = TRUE),
              width = "100%"
            )
          ),
          shiny::column(
            4,
            shiny::checkboxInput(
              ns("show_currents"), "Mostrar correntes SISCORAR",
              value = TRUE
            )
          )
        ),
        leaflet::leafletOutput(ns("map"), height = "600px")
      )
    )
  )
}

mod_track_map_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    # Build track lines per athlete (WGS84)
    tracks_data <- shiny::reactive({
      req(rv$records_sf, rv$paddlers)

      recs <- rv$records_sf |>
        dplyr::filter(id_athlete %in% rv$paddlers$id_athlete) |>
        dplyr::arrange(id_athlete, timestamp) |>
        sf::st_transform(4326)

      # Per-athlete lines + time range
      athletes <- unique(recs$id_athlete)
      palette <- grDevices::rainbow(length(athletes), s = 0.7, v = 0.85)
      colour_map <- stats::setNames(palette, athletes)

      tracks <- lapply(athletes, function(aid) {
        pts <- recs[recs$id_athlete == aid, ]
        if (nrow(pts) < 2) return(NULL)

        coords <- sf::st_coordinates(pts)
        line <- sf::st_linestring(coords)
        line_sf <- sf::st_sf(
          id_athlete       = aid,
          fullname_athlete = pts$fullname_athlete[1],
          n_pts            = nrow(pts),
          geometry         = sf::st_sfc(line, crs = 4326)
        )
        line_sf$colour <- colour_map[[as.character(aid)]]
        line_sf$timestamps <- list(pts$timestamp)
        line_sf
      })

      tracks <- tracks[!vapply(tracks, is.null, logical(1))]
      do.call(rbind, tracks)
    })

    # Time range for animation
    time_range <- shiny::reactive({
      req(rv$records_sf, rv$paddlers)
      recs <- rv$records_sf |>
        dplyr::filter(id_athlete %in% rv$paddlers$id_athlete)
      range(recs$timestamp)
    })

    # Precompute current field grid with U/V matrices for all training hours
    # Stores raw U/V per hour so we can interpolate between adjacent hours
    current_grid <- shiny::reactive({
      req(rv$grib_cropped, rv$grib_data, rv$records_sf)

      grib_times <- rv$grib_data$grib_times
      records_time_range <- range(rv$records_sf$timestamp)

      training_idx <- which(
        grib_times >= lubridate::floor_date(records_time_range[1], "hour") &
          grib_times <= lubridate::ceiling_date(records_time_range[2], "hour")
      )
      if (length(training_idx) == 0) return(NULL)

      recs_4326 <- sf::st_transform(rv$records_sf, 4326)
      bbox <- sf::st_bbox(recs_4326)
      ext <- terra::ext(
        bbox[["xmin"]], bbox[["xmax"]],
        bbox[["ymin"]], bbox[["ymax"]]
      )

      agg_factor <- 8L

      # Process first hour to establish grid coordinates
      u0 <- terra::aggregate(
        terra::crop(rv$grib_cropped$u_study[[training_idx[1]]], ext),
        fact = agg_factor, fun = "mean", na.rm = TRUE
      )
      crds <- terra::crds(u0, na.rm = FALSE)
      n_cells <- nrow(crds)

      # Extract U/V for all training hours into matrices
      u_mat <- matrix(NA_real_, nrow = n_cells, ncol = length(training_idx))
      v_mat <- matrix(NA_real_, nrow = n_cells, ncol = length(training_idx))

      for (j in seq_along(training_idx)) {
        h_idx <- training_idx[j]
        u_agg <- terra::aggregate(
          terra::crop(rv$grib_cropped$u_study[[h_idx]], ext),
          fact = agg_factor, fun = "mean", na.rm = TRUE
        )
        v_agg <- terra::aggregate(
          terra::crop(rv$grib_cropped$v_study[[h_idx]], ext),
          fact = agg_factor, fun = "mean", na.rm = TRUE
        )
        u_mat[, j] <- terra::values(u_agg)[, 1]
        v_mat[, j] <- terra::values(v_agg)[, 1]
      }

      # Keep only ocean cells (non-NA in at least one hour)
      ocean <- apply(u_mat, 1, function(x) !all(is.na(x)))

      list(
        lng        = crds[ocean, 1],
        lat        = crds[ocean, 2],
        u_mat      = u_mat[ocean, , drop = FALSE],
        v_mat      = v_mat[ocean, , drop = FALSE],
        grib_times = grib_times[training_idx]
      )
    })

    # Base map
    output$map <- leaflet::renderLeaflet({
      req(tracks_data())
      td <- tracks_data()
      bbox <- sf::st_bbox(td)

      leaflet::leaflet() |>
        leaflet::addProviderTiles(
          leaflet::providers$CartoDB.Positron,
          group = "CartoDB"
        ) |>
        leaflet::addProviderTiles(
          leaflet::providers$Esri.WorldImagery,
          group = "Sat\u00e9lite"
        ) |>
        leaflet::fitBounds(
          bbox[["xmin"]], bbox[["ymin"]],
          bbox[["xmax"]], bbox[["ymax"]]
        ) |>
        leaflet::addLayersControl(
          baseGroups = c("CartoDB", "Sat\u00e9lite"),
          options = leaflet::layersControlOptions(collapsed = FALSE)
        )
    })

    # Consistent speed domain for color palette (across all hours)
    speed_domain <- shiny::reactive({
      cg <- current_grid()
      if (is.null(cg)) return(c(0, 1))
      all_speed <- sqrt(cg$u_mat^2 + cg$v_mat^2) * 3.6
      c(0, max(all_speed, na.rm = TRUE))
    })

    # Manage current legend (only updates on checkbox or data change)
    shiny::observe({
      cg <- current_grid()
      show <- isTRUE(input$show_currents)
      proxy <- leaflet::leafletProxy("map")

      if (show && !is.null(cg)) {
        dom <- speed_domain()
        pal <- leaflet::colorNumeric("viridis", domain = dom)
        proxy |>
          leaflet::addLegend(
            position = "bottomright",
            pal = pal,
            values = seq(dom[1], dom[2], length.out = 5),
            title = "Corrente (km/h)",
            layerId = "current_legend"
          )
      } else {
        proxy |>
          leaflet::removeControl("current_legend")
      }
    })

    # Update tracks + currents on slider
    shiny::observe({
      req(tracks_data(), input$time_slider)
      td <- tracks_data()
      tr <- time_range()
      pct <- input$time_slider / 100

      # Cutoff time
      cutoff <- tr[1] + as.numeric(difftime(tr[2], tr[1], units = "secs")) * pct

      proxy <- leaflet::leafletProxy("map") |>
        leaflet::clearGroup("tracks") |>
        leaflet::clearGroup("labels") |>
        leaflet::clearGroup("currents")

      # --- Current field arrows (temporally interpolated) ---
      cg <- current_grid()
      if (isTRUE(input$show_currents) && !is.null(cg)) {
        gt <- cg$grib_times
        n_hours <- length(gt)
        cutoff_num <- as.numeric(cutoff)
        gt_num <- as.numeric(gt)

        # Find bounding hours for linear interpolation
        before_idx <- which(gt_num <= cutoff_num)
        after_idx  <- which(gt_num >= cutoff_num)

        if (length(before_idx) == 0) {
          idx1 <- 1L; idx2 <- 1L
        } else if (length(after_idx) == 0) {
          idx1 <- n_hours; idx2 <- n_hours
        } else {
          idx1 <- max(before_idx)
          idx2 <- min(after_idx)
        }

        if (idx1 == idx2) {
          weight <- 0
        } else {
          weight <- (cutoff_num - gt_num[idx1]) / (gt_num[idx2] - gt_num[idx1])
        }

        # Interpolate U/V between adjacent hours
        u_now <- cg$u_mat[, idx1] * (1 - weight) + cg$u_mat[, idx2] * weight
        v_now <- cg$v_mat[, idx1] * (1 - weight) + cg$v_mat[, idx2] * weight

        valid <- !is.na(u_now) & !is.na(v_now)
        if (any(valid)) {
          lng_v <- cg$lng[valid]
          lat_v <- cg$lat[valid]
          u_v   <- u_now[valid]
          v_v   <- v_now[valid]

          speed_kmh <- sqrt(u_v^2 + v_v^2) * 3.6
          dir_len   <- sqrt(u_v^2 + v_v^2)
          u_norm    <- u_v / pmax(dir_len, 1e-10)
          v_norm    <- v_v / pmax(dir_len, 1e-10)

          # Fixed-size arrow geometry in WGS84 degrees (~400m shaft)
          shaft_deg <- 0.004
          barb_deg  <- shaft_deg * 0.35
          barb_angle <- 5 * pi / 6  # 150 degrees (backward-pointing V)

          cos_lat <- cos(lat_v * pi / 180)
          tip_lng <- lng_v + u_norm * shaft_deg / cos_lat
          tip_lat <- lat_v + v_norm * shaft_deg

          angle <- atan2(u_norm, v_norm)
          left_lng  <- tip_lng + sin(angle + barb_angle) * barb_deg / cos_lat
          left_lat  <- tip_lat + cos(angle + barb_angle) * barb_deg
          right_lng <- tip_lng + sin(angle - barb_angle) * barb_deg / cos_lat
          right_lat <- tip_lat + cos(angle - barb_angle) * barb_deg

          n_arrows <- length(lng_v)

          # Shafts: start -> tip
          shaft_lines <- lapply(seq_len(n_arrows), function(i) {
            sf::st_linestring(matrix(
              c(lng_v[i], lat_v[i], tip_lng[i], tip_lat[i]),
              ncol = 2, byrow = TRUE
            ))
          })
          shaft_sf <- sf::st_sf(
            speed_kmh = speed_kmh,
            geometry  = sf::st_sfc(shaft_lines, crs = 4326)
          )

          # Arrowheads: left_barb -> tip -> right_barb
          head_lines <- lapply(seq_len(n_arrows), function(i) {
            sf::st_linestring(matrix(
              c(left_lng[i], left_lat[i],
                tip_lng[i], tip_lat[i],
                right_lng[i], right_lat[i]),
              ncol = 2, byrow = TRUE
            ))
          })
          head_sf <- sf::st_sf(
            speed_kmh = speed_kmh,
            geometry  = sf::st_sfc(head_lines, crs = 4326)
          )

          pal <- leaflet::colorNumeric("viridis", domain = speed_domain())

          proxy <- proxy |>
            leaflet::addPolylines(
              data = shaft_sf,
              color = ~pal(speed_kmh),
              weight = 1.5, opacity = 0.75,
              group = "currents",
              label = ~paste0(round(speed_kmh, 1), " km/h")
            ) |>
            leaflet::addPolylines(
              data = head_sf,
              color = ~pal(speed_kmh),
              weight = 2.5, opacity = 0.85,
              group = "currents"
            )
        }
      }

      # --- Athlete tracks ---
      for (i in seq_len(nrow(td))) {
        ts <- td$timestamps[[i]]
        keep <- ts <= cutoff
        if (sum(keep) < 2) next

        pts <- rv$records_sf |>
          dplyr::filter(id_athlete == td$id_athlete[i]) |>
          dplyr::arrange(timestamp) |>
          sf::st_transform(4326)
        pts <- pts[seq_len(sum(keep)), ]
        coords <- sf::st_coordinates(pts)

        # Trail (shadow): full track up to cutoff, lower opacity
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = coords[, 1], lat = coords[, 2],
            color = td$colour[i],
            weight = 2, opacity = 0.35,
            group = "tracks"
          )

        # Current position: last point with higher opacity
        n <- nrow(coords)
        trail_n <- max(1, n - 10)
        proxy <- proxy |>
          leaflet::addPolylines(
            lng = coords[trail_n:n, 1], lat = coords[trail_n:n, 2],
            color = td$colour[i],
            weight = 4, opacity = 0.9,
            group = "tracks"
          )

        # Label at current position
        proxy <- proxy |>
          leaflet::addMarkers(
            lng = coords[n, 1], lat = coords[n, 2],
            label = td$fullname_athlete[i],
            labelOptions = leaflet::labelOptions(
              noHide = TRUE, textOnly = TRUE,
              style = list("font-size" = "11px", "font-weight" = "bold")
            ),
            group = "labels",
            options = leaflet::markerOptions(opacity = 0)
          )
      }
    })
  })
}
