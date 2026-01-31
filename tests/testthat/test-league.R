test_that("build_league_table ranks athletes correctly", {
  segments <- data.table::data.table(
    athlete = c("Alice", "Bob", "Carol"),
    distance_m_target = rep(500, 3),
    predicted_time_sec = c(120, 100, 110)
  )

  result <- build_league_table(segments, athlete_col = "athlete")

  expect_true("rank" %in% names(result))
  # Bob is fastest (100s), then Carol (110s), then Alice (120s)
  expect_equal(result$athlete, c("Bob", "Carol", "Alice"))
  expect_equal(result$rank, 1:3)
})

test_that("build_league_table ranks per distance", {
  segments <- data.table::data.table(
    athlete = rep(c("Alice", "Bob"), 2),
    distance_m_target = c(100, 100, 500, 500),
    predicted_time_sec = c(10, 12, 60, 55)
  )

  result <- build_league_table(segments, athlete_col = "athlete")

  # At 100m: Alice rank 1, Bob rank 2
  r100 <- result[distance_m_target == 100]
  expect_equal(r100$athlete, c("Alice", "Bob"))
  expect_equal(r100$rank, 1:2)

  # At 500m: Bob rank 1, Alice rank 2
  r500 <- result[distance_m_target == 500]
  expect_equal(r500$athlete, c("Bob", "Alice"))
  expect_equal(r500$rank, 1:2)
})

test_that("build_league_table supports date grouping", {
  segments <- data.table::data.table(
    athlete = c("Alice", "Bob", "Alice", "Bob"),
    distance_m_target = rep(500, 4),
    predicted_time_sec = c(120, 100, 95, 110),
    session_date = as.POSIXct(c(
      "2026-01-28 10:00:00",
      "2026-01-28 10:00:00",
      "2026-01-29 10:00:00",
      "2026-01-29 10:00:00"
    ), tz = "UTC")
  )

  result <- build_league_table(
    segments,
    athlete_col = "athlete",
    date_col = "session_date"
  )

  expect_true("date" %in% names(result))
  # Day 1: Bob rank 1, Alice rank 2
  # Day 2: Alice rank 1, Bob rank 2
  r_day1 <- result[date == as.Date("2026-01-28")]
  expect_equal(r_day1$athlete, c("Bob", "Alice"))

  r_day2 <- result[date == as.Date("2026-01-29")]
  expect_equal(r_day2$athlete, c("Alice", "Bob"))
})

test_that("format_league formats time and filters top_n", {
  segments <- data.table::data.table(
    athlete = c("Alice", "Bob", "Carol"),
    distance_m_target = rep(500, 3),
    predicted_time_sec = c(120.5, 100.3, 110.7)
  )

  league <- build_league_table(segments, athlete_col = "athlete")
  result <- format_league(league, top_n = 2)

  expect_equal(nrow(result), 2L)
  expect_true("predicted_time_fmt" %in% names(result))
  # Bob (100.3s) = 1:40.3
  expect_equal(result$predicted_time_fmt[1], "1:40.3")
})

test_that("tide_prediction returns NULL with message", {
  expect_message(
    result <- tide_prediction(),
    "not yet implemented"
  )
  expect_null(result)
})
