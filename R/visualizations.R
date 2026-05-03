# ============================================================
#  Visualization Functions
# ============================================================

#' Plot Athlete Performance Over Time
#'
#' @description
#' Produces a base-R line plot of a chosen statistic across seasons for one
#' or more athletes. Data must be supplied as a longitudinal (long-format)
#' data frame with one row per athlete-season observation.
#'
#' @param data       A data frame in long format.
#' @param id_col     Character. Column name identifying each athlete.
#' @param season_col Character. Column name for the season (x-axis).
#' @param stat_col   Character. Column name of the statistic to plot (y-axis).
#' @param athletes   Optional character vector of athlete names to include.
#'   If \code{NULL} (default), all athletes are plotted.
#' @param title      Character. Plot title. Defaults to a generated string.
#' @param ...        Additional graphical parameters passed to \code{plot()}.
#'
#' @return Invisibly returns \code{NULL}. Called for its side-effect (plot).
#'
#' @examples
#' plot_performance(sample_seasons, "name", "season", "points")
#'
#' @export
plot_performance <- function(data, id_col, season_col, stat_col,
                             athletes = NULL, title = NULL, ...) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  for (col in c(id_col, season_col, stat_col)) {
    if (!col %in% names(data))
      stop(sprintf("Column '%s' not found in data.", col))
  }

  if (!is.null(athletes)) {
    data <- data[data[[id_col]] %in% athletes, ]
    if (nrow(data) == 0) stop("No matching athletes found.")
  }

  ids    <- unique(data[[id_col]])
  n_ids  <- length(ids)
  cols   <- grDevices::rainbow(n_ids)
  seasons <- sort(unique(data[[season_col]]))

  y_range <- range(data[[stat_col]], na.rm = TRUE)
  if (is.null(title)) title <- paste("Performance Over Time:", stat_col)

  graphics::plot(
    x    = range(as.numeric(seasons)),
    y    = y_range,
    type = "n",
    xlab = season_col,
    ylab = stat_col,
    main = title,
    xaxt = "n",
    ...
  )
  graphics::axis(1, at = as.numeric(seasons), labels = seasons)

  for (i in seq_along(ids)) {
    sub <- data[data[[id_col]] == ids[i], ]
    sub <- sub[order(sub[[season_col]]), ]
    graphics::lines(as.numeric(sub[[season_col]]), sub[[stat_col]],
                    col = cols[i], lwd = 2, type = "b", pch = 16)
  }

  graphics::legend("topleft", legend = ids, col = cols,
                   lwd = 2, pch = 16, bty = "n", cex = 0.85)
  invisible(NULL)
}

#' Bar Chart of Top Performers
#'
#' @description
#' Draws a horizontal bar chart of the top \code{n} athletes for a given
#' numeric metric, using base-R graphics.
#'
#' @param data    A data frame containing athlete data.
#' @param metric  Character. Column name to rank and display.
#' @param name_col Character. Column name holding athlete names. Default
#'   is \code{"name"}.
#' @param n       Integer. Number of athletes to display. Default \code{10}.
#' @param title   Character. Plot title. Defaults to a generated string.
#' @param color   Character. Bar fill color. Default \code{"steelblue"}.
#'
#' @return Invisibly returns the subset data frame used for plotting.
#'
#' @examples
#' plot_top_performers(sample_athletes, metric = "points", n = 8)
#'
#' @export
plot_top_performers <- function(data, metric, name_col = "name",
                                n = 10, title = NULL, color = "steelblue") {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")
  if (!metric %in% names(data))
    stop(sprintf("Column '%s' not found in data.", metric))
  if (!name_col %in% names(data))
    stop(sprintf("Column '%s' not found in data.", name_col))

  top <- top_performers(data, metric, n = n, desc = TRUE)
  top <- top[order(top[[metric]]), ]   # ascending for barplot (lowest at bottom)

  if (is.null(title)) title <- paste("Top", n, "Athletes by", metric)

  old_mar <- graphics::par(mar = c(4, 9, 3, 2))
  on.exit(graphics::par(old_mar))

  graphics::barplot(
    height  = top[[metric]],
    names.arg = top[[name_col]],
    horiz   = TRUE,
    las     = 1,
    col     = color,
    xlab    = metric,
    main    = title,
    border  = NA
  )
  invisible(top)
}

#' Radar / Spider Chart for Athlete Comparison
#'
#' @description
#' Produces a simple radar (spider) chart comparing two athletes across
#' shared statistics using base-R graphics. Values are normalized to
#' \eqn{[0, 1]} relative to the maximum observed across both athletes.
#'
#' @param athlete1 An object of class \code{athlete}.
#' @param athlete2 An object of class \code{athlete}.
#' @param title    Character. Plot title. Defaults to a comparison label.
#'
#' @return Invisibly returns a matrix of normalized values (rows = athletes,
#'   cols = shared statistics).
#'
#' @examples
#' a1 <- new_athlete("Alice", "soccer", "forward", 25,
#'                   c(goals = 20, assists = 8, dribbles = 60), 4)
#' a2 <- new_athlete("Bob", "soccer", "midfielder", 27,
#'                   c(goals = 10, assists = 18, dribbles = 45), 6)
#' plot_radar(a1, a2)
#'
#' @export
plot_radar <- function(athlete1, athlete2, title = NULL) {
  if (!inherits(athlete1, "athlete") || !inherits(athlete2, "athlete"))
    stop("Both arguments must be objects of class 'athlete'.")

  shared <- intersect(names(athlete1$stats), names(athlete2$stats))
  if (length(shared) < 3)
    stop("At least 3 shared statistics are required for a radar chart.")

  mat <- rbind(athlete1$stats[shared], athlete2$stats[shared])
  rownames(mat) <- c(athlete1$name, athlete2$name)

  # Normalize to [0, 1]
  col_max <- apply(mat, 2, max)
  col_max[col_max == 0] <- 1
  norm_mat <- sweep(mat, 2, col_max, "/")

  k      <- length(shared)
  angles <- seq(0, 2 * pi, length.out = k + 1)[-(k + 1)]
  xs     <- cos(angles)
  ys     <- sin(angles)

  if (is.null(title))
    title <- paste("Comparison:", athlete1$name, "vs", athlete2$name)

  graphics::plot(0, 0, type = "n", xlim = c(-1.4, 1.4), ylim = c(-1.4, 1.4),
                 asp = 1, axes = FALSE, xlab = "", ylab = "", main = title)

  # Draw grid rings
  for (r in c(0.25, 0.5, 0.75, 1.0)) {
    graphics::polygon(xs * r, ys * r, border = "grey80", lty = 2)
  }
  # Draw spokes
  graphics::segments(0, 0, xs, ys, col = "grey70")
  # Labels
  graphics::text(xs * 1.25, ys * 1.25, labels = shared, cex = 0.8)

  colors <- c("steelblue", "tomato")
  for (i in 1:2) {
    vals <- norm_mat[i, ]
    px   <- c(vals * xs, vals[1] * xs[1])
    py   <- c(vals * ys, vals[1] * ys[1])
    graphics::polygon(vals * xs, vals * ys,
                      border = colors[i], col = grDevices::adjustcolor(colors[i], 0.2),
                      lwd = 2)
  }

  graphics::legend("bottomright", legend = rownames(norm_mat),
                   fill = grDevices::adjustcolor(colors, 0.4),
                   border = colors, bty = "n", cex = 0.85)

  invisible(norm_mat)
}
