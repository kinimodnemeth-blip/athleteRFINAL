# ============================================================
#  Core Analytics Functions
# ============================================================

#' Compute a Composite Performance Score
#'
#' @description
#' Calculates a weighted composite performance score for an \code{athlete}
#' object. Each statistic in the athlete's \code{stats} vector is multiplied
#' by a corresponding weight, then summed. If no weights are provided, all
#' statistics are weighted equally.
#'
#' @param athlete An object of class \code{athlete}.
#' @param weights Optional named numeric vector of weights. Names must match
#'   those in \code{athlete$stats}. Unmatched stats receive a weight of
#'   \code{1}. Defaults to \code{NULL} (equal weighting).
#'
#' @return A single numeric value representing the composite performance score.
#'
#' @examples
#' a <- new_athlete("Jordan Smith", "basketball", "guard", 26,
#'                  c(points = 22.4, assists = 7.1, rebounds = 4.3), 4)
#' performance_score(a)
#' performance_score(a, weights = c(points = 2, assists = 1.5, rebounds = 1))
#'
#' @export
performance_score <- function(athlete, weights = NULL) {
  if (!inherits(athlete, "athlete"))
    stop("`athlete` must be an object of class 'athlete'.")

  s <- athlete$stats
  if (is.null(weights)) {
    return(sum(s))
  }
  w <- rep(1, length(s))
  names(w) <- names(s)
  matched <- intersect(names(weights), names(s))
  w[matched] <- weights[matched]
  sum(s * w)
}

#' Compare Two or More Athletes
#'
#' @description
#' Produces a side-by-side data frame comparing the statistics of two or more
#' \code{athlete} objects. Only statistics present in \emph{all} athletes are
#' included. An optional composite performance score column is appended.
#'
#' @param ...      Two or more objects of class \code{athlete}.
#' @param weights  Optional named numeric vector passed to
#'   \code{\link{performance_score}}.
#' @param score    Logical. If \code{TRUE} (default), a \code{perf_score}
#'   column is added.
#'
#' @return A data frame with one row per athlete and columns for name, sport,
#'   position, age, each shared statistic, and (optionally) \code{perf_score}.
#'
#' @examples
#' a1 <- new_athlete("Alice", "basketball", "center", 28,
#'                   c(points = 18, rebounds = 10, assists = 3), 5)
#' a2 <- new_athlete("Bob", "basketball", "guard", 24,
#'                   c(points = 14, rebounds = 5, assists = 8), 2)
#' compare_athletes(a1, a2)
#'
#' @export
compare_athletes <- function(..., weights = NULL, score = TRUE) {
  athletes <- list(...)
  if (length(athletes) < 2)
    stop("Provide at least two athlete objects.")
  bad <- !vapply(athletes, inherits, logical(1), "athlete")
  if (any(bad)) stop("All arguments must be objects of class 'athlete'.")

  # Find common stats
  stat_names <- Reduce(intersect, lapply(athletes, function(a) names(a$stats)))
  if (length(stat_names) == 0)
    stop("Athletes share no common statistics to compare.")

  rows <- lapply(athletes, function(a) {
    base <- data.frame(
      name     = a$name,
      sport    = a$sport,
      position = a$position,
      age      = a$age,
      stringsAsFactors = FALSE
    )
    stat_df <- as.data.frame(t(a$stats[stat_names]))
    cbind(base, stat_df)
  })

  result <- do.call(rbind, rows)

  if (score) {
    result$perf_score <- vapply(athletes, performance_score,
                                numeric(1), weights = weights)
  }
  rownames(result) <- NULL
  result
}

#' Summarize Team-Level Statistics
#'
#' @description
#' Aggregates per-athlete statistics across all members of a \code{team}
#' object, returning the team mean, standard deviation, minimum, and maximum
#' for every shared statistic.
#'
#' @param team An object of class \code{team}.
#'
#' @return A data frame with one row per statistic and columns
#'   \code{stat}, \code{mean}, \code{sd}, \code{min}, and \code{max}.
#'
#' @examples
#' a1 <- new_athlete("Alice", "basketball", "center", 28,
#'                   c(points = 18, rebounds = 10), 5)
#' a2 <- new_athlete("Bob", "basketball", "guard", 24,
#'                   c(points = 14, rebounds = 5), 2)
#' t1 <- new_team("Dream Team", "basketball", list(a1, a2), "2025-26")
#' team_summary(t1)
#'
#' @export
team_summary <- function(team) {
  if (!inherits(team, "team"))
    stop("`team` must be an object of class 'team'.")

  stat_names <- Reduce(intersect, lapply(team$athletes, function(a) names(a$stats)))
  if (length(stat_names) == 0)
    stop("No common statistics found across team members.")

  mat <- do.call(rbind, lapply(team$athletes, function(a) a$stats[stat_names]))

  data.frame(
    stat = stat_names,
    mean = apply(mat, 2, mean),
    sd   = apply(mat, 2, stats::sd),
    min  = apply(mat, 2, min),
    max  = apply(mat, 2, max),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

#' Rank and Return Top Performers
#'
#' @description
#' Given a data frame of athletes (such as \code{\link{sample_athletes}} or
#' the output of \code{\link{compare_athletes}}), returns the top \code{n}
#' rows ranked by a chosen numeric column.
#'
#' @param data    A data frame containing athlete statistics.
#' @param metric  Character. The column name to rank by. Must be numeric.
#' @param n       Integer. Number of top athletes to return. Default is \code{5}.
#' @param desc    Logical. If \code{TRUE} (default), ranks in descending order.
#'
#' @return A data frame of the top \code{n} athletes sorted by \code{metric}.
#'
#' @examples
#' top_performers(sample_athletes, metric = "points", n = 5)
#'
#' @export
top_performers <- function(data, metric, n = 5, desc = TRUE) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!metric %in% names(data))
    stop(sprintf("Column '%s' not found in data.", metric))
  if (!is.numeric(data[[metric]]))
    stop(sprintf("Column '%s' must be numeric.", metric))

  n <- min(n, nrow(data))
  idx <- order(data[[metric]], decreasing = desc)[seq_len(n)]
  data[idx, , drop = FALSE]
}

#' Normalize Athlete Statistics to a 0-100 Scale
#'
#' @description
#' Applies min-max normalization to one or more numeric columns of a data
#' frame, rescaling values to the range \eqn{[0, 100]}. Useful for creating
#' comparable composite indices across statistics measured on different scales.
#'
#' @param data    A data frame.
#' @param columns Character vector of column names to normalize. If \code{NULL}
#'   (default), all numeric columns are normalized.
#'
#' @return A data frame identical to \code{data} with the selected columns
#'   replaced by their normalized values.
#'
#' @examples
#' norm_df <- normalize_stats(sample_athletes, columns = c("points", "assists"))
#' range(norm_df$points)  # Should be c(0, 100)
#'
#' @export
normalize_stats <- function(data, columns = NULL) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  if (is.null(columns)) {
    columns <- names(data)[vapply(data, is.numeric, logical(1))]
  }
  bad <- setdiff(columns, names(data))
  if (length(bad) > 0)
    stop("Columns not found in data: ", paste(bad, collapse = ", "))

  minmax <- function(x) {
    rng <- range(x, na.rm = TRUE)
    if (rng[1] == rng[2]) return(rep(50, length(x)))
    (x - rng[1]) / (rng[2] - rng[1]) * 100
  }

  for (col in columns) {
    if (!is.numeric(data[[col]]))
      warning(sprintf("Column '%s' is not numeric; skipping.", col))
    else
      data[[col]] <- minmax(data[[col]])
  }
  data
}

#' Calculate Season-over-Season Growth Rate
#'
#' @description
#' Computes the percentage growth (or decline) of a numeric statistic between
#' consecutive seasons in a longitudinal data frame. The data frame must contain
#' columns identifying the athlete, the season, and the statistic of interest.
#'
#' @param data        A data frame in long format (one row per athlete-season).
#' @param id_col      Character. Column name identifying the athlete (e.g.,
#'   \code{"name"}).
#' @param season_col  Character. Column name for the season (numeric or
#'   character that sorts chronologically).
#' @param stat_col    Character. Column name of the statistic to analyze.
#'
#' @return A data frame with the original columns plus a \code{growth_pct}
#'   column. The first season for each athlete will have \code{NA} growth.
#'
#' @examples
#' growth <- season_growth(sample_seasons, "name", "season", "points")
#' head(growth)
#'
#' @export
season_growth <- function(data, id_col, season_col, stat_col) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  for (col in c(id_col, season_col, stat_col)) {
    if (!col %in% names(data))
      stop(sprintf("Column '%s' not found in data.", col))
  }

  data <- data[order(data[[id_col]], data[[season_col]]), ]
  data$growth_pct <- NA_real_

  ids <- unique(data[[id_col]])
  for (id in ids) {
    idx  <- which(data[[id_col]] == id)
    vals <- data[[stat_col]][idx]
    grw  <- c(NA, diff(vals) / abs(vals[-length(vals)]) * 100)
    data$growth_pct[idx] <- grw
  }
  rownames(data) <- NULL
  data
}

#' Compute Athlete Consistency Index
#'
#' @description
#' Measures how consistently an athlete performs over seasons by computing
#' the coefficient of variation (CV) for a chosen statistic. A lower CV
#' indicates more consistent performance.
#'
#' CV is defined as: \eqn{CV = (sd / mean) \times 100}
#'
#' @param data       A data frame in long format (one row per athlete-season).
#' @param id_col     Character. Column name identifying the athlete.
#' @param stat_col   Character. Column name of the statistic.
#'
#' @return A data frame with columns \code{athlete}, \code{mean_stat},
#'   \code{sd_stat}, and \code{cv} (coefficient of variation, in percent).
#'   Athletes are sorted from most to least consistent (ascending CV).
#'
#' @examples
#' consistency_index(sample_seasons, id_col = "name", stat_col = "points")
#'
#' @export
consistency_index <- function(data, id_col, stat_col) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  for (col in c(id_col, stat_col)) {
    if (!col %in% names(data))
      stop(sprintf("Column '%s' not found in data.", col))
  }

  ids <- unique(data[[id_col]])
  result <- do.call(rbind, lapply(ids, function(id) {
    vals <- data[[stat_col]][data[[id_col]] == id]
    m    <- mean(vals, na.rm = TRUE)
    s    <- stats::sd(vals, na.rm = TRUE)
    cv   <- if (m == 0) NA_real_ else (s / m) * 100
    data.frame(athlete = id, mean_stat = m, sd_stat = s, cv = cv,
               stringsAsFactors = FALSE)
  }))

  result[order(result$cv, na.last = TRUE), ]
}
