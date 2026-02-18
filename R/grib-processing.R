# ------------------------------------------------------------------
# GRIB ocean current data processing
# ------------------------------------------------------------------

#' Fetch and parse SISCORAR GRIB ocean current data
#'
#' Wraps [rsiscorar::get_grib()] and splits the returned SpatRaster into
#' U (eastward) and V (northward) current velocity layers with hourly
#' timestamps.
#'
#' @param date Date or `"YYYY-MM-DD"` string.
#' @param area Character area name passed to `rsiscorar::get_grib()`.
#'   Default `"baiatos"`.
#' @param resolution Numeric grid resolution. Default `0.001`.
#' @return A list with elements:
#'   \describe{
#'     \item{u_layers}{SpatRaster with U-component layers (one per hour)}
#'     \item{v_layers}{SpatRaster with V-component layers (one per hour)}
#'     \item{grib_times}{POSIXct vector of hourly timestamps (UTC)}
#'     \item{n_hours}{Integer number of hours}
#'   }
#' @export
fetch_grib_data <- function(date, area = "baiatos", resolution = 0.001) {
  rlang::check_installed("rsiscorar", reason = "to fetch GRIB current data")
  rlang::check_installed("terra", reason = "to process GRIB raster layers")

  grib <- rsiscorar::get_grib(
    date = date, area = area, resolution = resolution
  )

  layer_names <- names(grib)
  is_u <- grepl("u-component|UOGRD", layer_names, ignore.case = TRUE)
  is_v <- grepl("v-component|VOGRD", layer_names, ignore.case = TRUE)

  if (!any(is_u) || !any(is_v)) {
    cli::cli_abort("Expected U and V layers in GRIB, found neither.")
  }
  if (sum(is_u) != sum(is_v)) {
    cli::cli_abort(
      "Expected equal number of U ({sum(is_u)}) and V ({sum(is_v)}) layers."
    )
  }

  u_layers <- grib[[which(is_u)]]
  v_layers <- grib[[which(is_v)]]
  n_hours  <- terra::nlyr(u_layers)

  grib_times <- as.POSIXct(
    paste(as.character(date), sprintf("%02d:00:00", seq(0, n_hours - 1))),
    tz = "UTC"
  )

  list(
    u_layers   = u_layers,
    v_layers   = v_layers,
    grib_times = grib_times,
    n_hours    = n_hours
  )
}

#' Crop GRIB layers to study extent
#'
#' Computes the combined bounding box of a study area polygon and GPS records,
#' then crops U and V layers to that extent.
#'
#' @param u_layers,v_layers SpatRaster U and V components (WGS84).
#' @param study_area `sfc` polygon defining the study area (any CRS).
#' @param records_sf `sf` POINT object of GPS records (any CRS).
#' @return A list with elements `u_study` and `v_study` (cropped SpatRasters).
#' @export
crop_grib_to_extent <- function(u_layers, v_layers, study_area, records_sf) {
  study_bbox_4326   <- sf::st_bbox(sf::st_transform(study_area, 4326))
  records_bbox_4326 <- sf::st_bbox(sf::st_transform(records_sf, 4326))

  combined_bbox <- c(
    xmin = min(study_bbox_4326[["xmin"]], records_bbox_4326[["xmin"]]),
    xmax = max(study_bbox_4326[["xmax"]], records_bbox_4326[["xmax"]]),
    ymin = min(study_bbox_4326[["ymin"]], records_bbox_4326[["ymin"]]),
    ymax = max(study_bbox_4326[["ymax"]], records_bbox_4326[["ymax"]])
  )

  study_ext <- terra::ext(
    combined_bbox[["xmin"]], combined_bbox[["xmax"]],
    combined_bbox[["ymin"]], combined_bbox[["ymax"]]
  )

  list(
    u_study = terra::crop(u_layers, study_ext),
    v_study = terra::crop(v_layers, study_ext)
  )
}

#' Compute hourly GRIB current at a specific location
#'
#' Extracts U and V current components at a specific point for each hour
#' and computes speed and direction. Defaults to the SIMCOSTA buoy 515
#' position so that SISCORAR and buoy values are comparable at the same
#' location.
#'
#' @param u_layers,v_layers Cropped SpatRaster layers (WGS84).
#' @param grib_times POSIXct vector of layer timestamps.
#' @param lon,lat Numeric extraction point in WGS84.
#'   Default: SIMCOSTA buoy 515 (~12\u00b059'S, 38\u00b032'W).
#' @param tz Character timezone for `local_time` column.
#'   Default `"America/Bahia"`.
#' @return A tibble with columns: `datetime`, `current_speed_kmh`,
#'   `current_direction`, `local_time`, `hour`.
#' @export
grib_hourly_summary <- function(u_layers, v_layers, grib_times,
                                lon = BUOY_515_LON, lat = BUOY_515_LAT,
                                tz = "America/Bahia") {
  pt <- terra::vect(
    matrix(c(lon, lat), ncol = 2),
    crs = "EPSG:4326"
  )

  # Extract U,V at the point for all hours; column 1 = ID, rest = layers
  u_vals <- as.numeric(terra::extract(u_layers, pt)[1, -1])
  v_vals <- as.numeric(terra::extract(v_layers, pt)[1, -1])

  # If point falls on NA (near coast), try focal fill
  na_mask <- is.na(u_vals) | is.na(v_vals)
  if (any(na_mask)) {
    for (i in which(na_mask)) {
      u_filled <- .fill_focal(u_layers[[i]])
      v_filled <- .fill_focal(v_layers[[i]])
      u_vals[i] <- terra::extract(u_filled, pt)[1, 2]
      v_vals[i] <- terra::extract(v_filled, pt)[1, 2]
    }
  }

  tibble::tibble(
    datetime          = grib_times,
    current_speed_kmh = sqrt(u_vals^2 + v_vals^2) * 3.6,
    current_direction = (atan2(u_vals, v_vals) * 180 / pi) %% 360
  ) |>
    dplyr::mutate(
      local_time = lubridate::with_tz(datetime, tzone = tz),
      hour       = format(lubridate::floor_date(local_time, "hour"), "%Hh")
    )
}

#' Match GRIB current to fastest segments
#'
#' For each segment from [fastest_straight_distance()], extracts U and V
#' current values at the segment midpoint location and nearest GRIB hour.
#' Near-shore NA values are filled via focal interpolation.
#'
#' @param fastx `data.table` from [fastest_straight_distance()] with columns
#'   `start_x`, `start_y`, `end_x`, `end_y`, `start_time`.
#' @param u_study,v_study Cropped SpatRaster layers (WGS84).
#' @param grib_times POSIXct vector of GRIB timestamps (UTC).
#' @param records_crs CRS of the records (for midpoint projection).
#'   Accepts anything [sf::st_crs()] understands.
#' @return The input `fastx` with added columns `current_speed_kmh` and
#'   `current_direction_deg`.
#' @export
match_grib_to_segments <- function(fastx, u_study, v_study, grib_times,
                                   records_crs) {
  if (nrow(fastx) == 0L) {
    fastx$current_speed_kmh     <- numeric(0)
    fastx$current_direction_deg <- numeric(0)
    return(fastx)
  }

  n_hours <- terra::nlyr(u_study)

  # Segment midpoints -> WGS84

  mid_x <- (fastx$start_x + fastx$end_x) / 2
  mid_y <- (fastx$start_y + fastx$end_y) / 2

  mid_pts <- sf::st_as_sf(
    tibble::tibble(x = mid_x, y = mid_y),
    coords = c("x", "y"),
    crs = records_crs
  ) |>
    sf::st_transform(4326)

  mid_vect <- terra::vect(mid_pts)

  # Nearest GRIB hour for each segment

  seg_hour_utc <- as.numeric(format(fastx$start_time, "%H", tz = "UTC"))
  seg_hour_idx <- pmin(pmax(seg_hour_utc, 0L), n_hours - 1L) + 1L

  # Extract U and V at all midpoints for all hours
  u_all <- as.matrix(terra::extract(u_study, mid_vect))
  v_all <- as.matrix(terra::extract(v_study, mid_vect))

  # Column 1 is ID; columns 2:(n_hours+1) are hours 0:(n_hours-1)
  row_idx <- seq_len(nrow(fastx))
  col_idx <- seg_hour_idx + 1L

  u_vals <- u_all[cbind(row_idx, col_idx)]
  v_vals <- v_all[cbind(row_idx, col_idx)]

  # Fill near-shore NAs via focal interpolation
  na_mask <- is.na(u_vals) | is.na(v_vals)
  if (any(na_mask)) {
    na_hours <- unique(seg_hour_idx[na_mask])
    for (h in na_hours) {
      h_mask <- na_mask & (seg_hour_idx == h)
      u_filled <- .fill_focal(u_study[[h]])
      v_filled <- .fill_focal(v_study[[h]])
      u_vals[h_mask] <- terra::extract(u_filled, mid_vect[h_mask])[, 2]
      v_vals[h_mask] <- terra::extract(v_filled, mid_vect[h_mask])[, 2]
    }
  }

  # Convert U,V (m/s) to speed (km/h) and direction (TO convention)
  fastx$current_speed_kmh     <- sqrt(u_vals^2 + v_vals^2) * 3.6
  fastx$current_direction_deg <- (atan2(u_vals, v_vals) * 180 / pi) %% 360

  fastx
}

#' Expand ocean values into coastal NA cells via focal smoothing
#' @noRd
.fill_focal <- function(r, max_iter = 10L) {
  for (i in seq_len(max_iter)) {
    if (!any(is.na(terra::values(r)))) break
    r <- terra::focal(r, w = 11, fun = "mean", na.policy = "only", na.rm = TRUE)
  }
  r
}
