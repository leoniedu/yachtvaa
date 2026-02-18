# ------------------------------------------------------------------
# Current field visualization and conditions table helpers
# ------------------------------------------------------------------

#' Build current field arrow data for visualization
#'
#' Projects GRIB U/V layers for a specific hour to UTM, aggregates to reduce
#' arrow density, and computes arrow endpoints for plotting with
#' [ggplot2::geom_segment()].
#'
#' @param u_layers,v_layers Cropped SpatRaster (WGS84).
#' @param hour_idx Integer layer index (1-based).
#' @param records_sf `sf` object in projected CRS (for extent cropping).
#' @param crs Integer EPSG code for projection. Default `31984` (UTM 24S).
#' @param agg_factor Integer aggregation factor to reduce arrow density.
#'   Default `4`.
#' @param arrow_scale Numeric scaling for arrow length in projected units.
#'   Default `800`.
#' @return A tibble with columns: `x`, `y`, `u`, `v`, `speed_kmh`,
#'   `xend`, `yend`.
#' @export
current_field_arrows <- function(u_layers, v_layers, hour_idx, records_sf,
                                 crs = CRS_UTM24S, agg_factor = 4L,
                                 arrow_scale = 800) {
  records_ext <- terra::ext(
    sf::st_bbox(records_sf)[c("xmin", "xmax", "ymin", "ymax")]
  )

  u_utm <- terra::project(u_layers[[hour_idx]], paste0("EPSG:", crs)) |>
    terra::crop(records_ext)
  v_utm <- terra::project(v_layers[[hour_idx]], paste0("EPSG:", crs)) |>
    terra::crop(records_ext)

  u_agg <- terra::aggregate(u_utm, fact = agg_factor, fun = "mean", na.rm = TRUE)
  v_agg <- terra::aggregate(v_utm, fact = agg_factor, fun = "mean", na.rm = TRUE)

  tibble::tibble(
    x = terra::crds(u_agg, na.rm = FALSE)[, 1],
    y = terra::crds(u_agg, na.rm = FALSE)[, 2],
    u = terra::values(u_agg)[, 1],
    v = terra::values(v_agg)[, 1]
  ) |>
    dplyr::filter(!is.na(u), !is.na(v)) |>
    dplyr::mutate(
      speed_kmh = sqrt(u^2 + v^2) * 3.6,
      xend      = x + u * arrow_scale,
      yend      = y + v * arrow_scale
    )
}

#' Build hourly environmental conditions table
#'
#' Combines SISCORAR GRIB currents, SIMCOSTA buoy currents, and buoy wind
#' into a wide-format table with metric rows and hour columns, suitable for
#' display with [reactable::reactable()] or [gt::gt()].
#'
#' @param grib_hourly Tibble from [grib_hourly_summary()].
#' @param buoy_ip Interpolated buoy data from [interpolate_buoy()].
#' @param records_time_range Length-2 POSIXct vector
#'   (`c(min_timestamp, max_timestamp)` of athlete records).
#' @param tz Timezone string. Default `"America/Bahia"`.
#' @return A tibble in wide format: rows are metrics (wind, SISCORAR,
#'   buoy current), columns are hours.
#' @export
build_conditions_table <- function(grib_hourly, buoy_ip, records_time_range,
                                   tz = "America/Bahia") {
  # Canonical hour range from records: floor(min) to ceiling(max)
  hour_start <- lubridate::floor_date(records_time_range[1], "hour")
  hour_end   <- lubridate::ceiling_date(records_time_range[2], "hour")

  canonical_hours_utc <- seq(hour_start, hour_end, by = "1 hour")
  canonical_hours <- format(
    lubridate::with_tz(canonical_hours_utc, tzone = tz),
    "%Hh"
  )
  # Deduplicate (in case floor == ceiling)
  canonical_hours <- unique(canonical_hours)

  # Filter GRIB to canonical range
  grib_training <- grib_hourly |>
    dplyr::filter(datetime >= hour_start, datetime <= hour_end)

  current_h <- grib_training |>
    dplyr::summarise(
      speed = round(mean(current_speed_kmh, na.rm = TRUE), 1),
      dir   = round(mean(current_direction, na.rm = TRUE), 0),
      .by   = hour
    ) |>
    dplyr::arrange(hour)

  # SISCORAR current metrics
  metrics_long <- dplyr::bind_rows(
    tibble::tibble(
      hour   = current_h$hour,
      metric = "SISCORAR (km/h)",
      value  = as.character(current_h$speed)
    ),
    tibble::tibble(
      hour   = current_h$hour,
      metric = "Dir. SISCORAR",
      value  = dir_arrow(current_h$dir)
    )
  )

  # Buoy data: filter to same canonical range
  buoy_local <- buoy_ip |>
    dplyr::mutate(
      local_time = lubridate::with_tz(datetime, tzone = tz),
      hour       = format(lubridate::floor_date(local_time, "hour"), "%Hh")
    ) |>
    dplyr::filter(datetime >= hour_start, datetime <= hour_end)

  has_buoy_current <- "current_speed_kmh" %in% names(buoy_ip) &&
    any(!is.na(buoy_ip$current_speed_kmh))
  has_wind <- "wind_speed" %in% names(buoy_ip) &&
    any(!is.na(buoy_ip$wind_speed))

  if (has_buoy_current) {
    buoy_current_h <- buoy_local |>
      dplyr::filter(!is.na(current_speed_kmh)) |>
      dplyr::summarise(
        speed = round(mean(current_speed_kmh, na.rm = TRUE), 1),
        dir   = round(mean(current_direction, na.rm = TRUE), 0),
        .by   = hour
      ) |>
      dplyr::arrange(hour)

    metrics_long <- dplyr::bind_rows(
      metrics_long,
      tibble::tibble(
        hour = buoy_current_h$hour, metric = "Boia (km/h)",
        value = as.character(buoy_current_h$speed)
      ),
      tibble::tibble(
        hour = buoy_current_h$hour, metric = "Dir. Boia",
        value = dir_arrow(buoy_current_h$dir)
      )
    )
  }

  if (has_wind) {
    wind_h <- buoy_local |>
      dplyr::filter(!is.na(wind_speed)) |>
      dplyr::summarise(
        speed = round(mean(wind_speed * 3.6, na.rm = TRUE), 0),
        dir   = round(mean(wind_direction, na.rm = TRUE), 0),
        .by   = hour
      ) |>
      dplyr::arrange(hour)

    metrics_long <- dplyr::bind_rows(
      tibble::tibble(
        hour = wind_h$hour, metric = "Vento (km/h)",
        value = as.character(wind_h$speed)
      ),
      tibble::tibble(
        hour = wind_h$hour, metric = "Dir. vento",
        value = dir_arrow(wind_h$dir, from = TRUE)
      ),
      metrics_long
    )
  }

  metric_order <- c(
    "Vento (km/h)", "Dir. vento",
    "SISCORAR (km/h)", "Dir. SISCORAR",
    "Boia (km/h)", "Dir. Boia"
  )

  metrics_long |>
    dplyr::mutate(
      metric = factor(metric, levels = metric_order),
      hour   = factor(hour, levels = canonical_hours)
    ) |>
    dplyr::arrange(metric, hour) |>
    tidyr::pivot_wider(
      names_from = hour, values_from = value, values_fill = "\u2014"
    )
}

#' Convert compass direction to Unicode arrow
#'
#' Maps a compass bearing (0-360 degrees) to one of 8 Unicode arrow characters.
#'
#' @param deg Numeric degrees (0-360).
#' @param from Logical. If `TRUE`, reverse direction (FROM vs TO convention).
#'   Default `FALSE`.
#' @return Character vector of arrow Unicode characters.
#' @export
dir_arrow <- function(deg, from = FALSE) {
  if (from) deg <- (deg + 180) %% 360
  arrows <- c(
    "\u2191", "\u2197", "\u2192", "\u2198",
    "\u2193", "\u2199", "\u2190", "\u2196"
  )
  ifelse(is.na(deg), "\u2014", arrows[(round(deg / 45) %% 8) + 1])
}
