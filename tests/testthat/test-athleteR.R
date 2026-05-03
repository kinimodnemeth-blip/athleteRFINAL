library(testthat)
library(athleteR)

# ============================================================
#  Tests: S3 Classes
# ============================================================

test_that("new_athlete creates a valid athlete object", {
  a <- new_athlete("Jordan Smith", "basketball", "guard", 26,
                   c(points = 22.4, assists = 7.1, rebounds = 4.3), 4)
  expect_s3_class(a, "athlete")
  expect_equal(a$name, "Jordan Smith")
  expect_equal(a$sport, "basketball")
  expect_equal(a$stats["points"], c(points = 22.4))
  expect_equal(a$seasons, 4L)
})

test_that("new_athlete validates inputs", {
  expect_error(new_athlete(123, "basketball", "guard", 26, c(pts = 10)),
               "`name` must be a single character string.")
  expect_error(new_athlete("A", "basketball", "guard", -5, c(pts = 10)),
               "`age` must be a non-negative number.")
  expect_error(new_athlete("A", "basketball", "guard", 25, c(10, 20)),
               "`stats` must be a named numeric vector.")
})

test_that("new_team creates a valid team object", {
  a1 <- new_athlete("Alice", "basketball", "center", 28,
                    c(points = 18, rebounds = 10), 5)
  a2 <- new_athlete("Bob", "basketball", "guard", 24,
                    c(points = 14, assists = 8), 2)
  t1 <- new_team("Dream Team", "basketball", list(a1, a2), "2025-26")
  expect_s3_class(t1, "team")
  expect_equal(t1$roster_size, 2L)
  expect_equal(t1$season, "2025-26")
})

test_that("new_team rejects non-athlete list elements", {
  expect_error(new_team("Bad Team", "basketball", list("not an athlete"), "2025"),
               "All elements of `athletes` must be objects of class 'athlete'.")
})

# ============================================================
#  Tests: Core Functions
# ============================================================

test_that("performance_score returns correct equal-weight sum", {
  a <- new_athlete("X", "soccer", "forward", 23,
                   c(goals = 10, assists = 5, dribbles = 30), 2)
  expect_equal(performance_score(a), 45)
})

test_that("performance_score applies custom weights", {
  a <- new_athlete("X", "soccer", "forward", 23,
                   c(goals = 10, assists = 5), 2)
  score <- performance_score(a, weights = c(goals = 2, assists = 1))
  expect_equal(score, 25)  # 10*2 + 5*1
})

test_that("compare_athletes returns data frame with correct dims", {
  a1 <- new_athlete("Alice", "basketball", "center", 28,
                    c(points = 18, rebounds = 10, assists = 3), 5)
  a2 <- new_athlete("Bob", "basketball", "guard", 24,
                    c(points = 14, rebounds = 5, assists = 8), 2)
  result <- compare_athletes(a1, a2)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true("perf_score" %in% names(result))
})

test_that("compare_athletes errors with fewer than 2 athletes", {
  a1 <- new_athlete("Alice", "basketball", "center", 28, c(pts = 18), 5)
  expect_error(compare_athletes(a1), "Provide at least two athlete objects.")
})

test_that("team_summary returns correct structure", {
  a1 <- new_athlete("Alice", "basketball", "center", 28, c(points = 18, rebounds = 10), 5)
  a2 <- new_athlete("Bob", "basketball", "guard", 24, c(points = 14, rebounds = 5), 2)
  t1 <- new_team("Team A", "basketball", list(a1, a2), "2025")
  result <- team_summary(t1)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2)
  expect_true(all(c("stat", "mean", "sd", "min", "max") %in% names(result)))
  expect_equal(result$mean[result$stat == "points"], 16)
})

test_that("top_performers returns correct number of rows", {
  data(sample_athletes)
  top5 <- top_performers(sample_athletes, metric = "points", n = 5)
  expect_equal(nrow(top5), 5)
  expect_true(all(diff(top5$points) <= 0))  # descending order
})

test_that("normalize_stats rescales to 0-100", {
  data(sample_athletes)
  norm <- normalize_stats(sample_athletes, columns = c("points", "assists"))
  expect_equal(min(norm$points), 0)
  expect_equal(max(norm$points), 100)
})

test_that("season_growth returns growth_pct column", {
  data(sample_seasons)
  result <- season_growth(sample_seasons, "name", "season", "points")
  expect_true("growth_pct" %in% names(result))
  # First season per athlete should be NA
  first_rows <- !duplicated(result$name)
  expect_true(all(is.na(result$growth_pct[first_rows])))
})

test_that("consistency_index returns ascending CV", {
  data(sample_seasons)
  result <- consistency_index(sample_seasons, "name", "points")
  expect_s3_class(result, "data.frame")
  expect_true(all(c("athlete", "cv") %in% names(result)))
  cvs <- result$cv[!is.na(result$cv)]
  expect_equal(cvs, sort(cvs))
})
