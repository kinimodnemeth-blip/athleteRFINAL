# ============================================================
#  Built-in Datasets
# ============================================================

#' Sample Athletes Data Frame
#'
#' @description
#' A simulated cross-sectional data frame of 50 fictional athletes spanning
#' five sports. Each row represents one athlete's average statistics for their
#' most recent completed season. This dataset is intended for demonstration
#' and testing of \pkg{athleteR} functions.
#'
#' @format A data frame with 50 rows and 9 variables:
#' \describe{
#'   \item{name}{Character. Athlete's full name.}
#'   \item{sport}{Character. Sport: one of \code{"basketball"}, \code{"soccer"},
#'     \code{"baseball"}, \code{"tennis"}, or \code{"track"}.}
#'   \item{position}{Character. Playing or event position.}
#'   \item{age}{Integer. Athlete age in years (range 18–38).}
#'   \item{seasons}{Integer. Career seasons completed (range 1–15).}
#'   \item{points}{Numeric. Average points or goals scored per game/match.}
#'   \item{assists}{Numeric. Average assists per game/match.}
#'   \item{rebounds}{Numeric. Average rebounds or defensive actions per game.}
#'   \item{win_rate}{Numeric. Season win rate (proportion, 0–1).}
#' }
#'
#' @source Simulated data generated for package demonstration purposes.
#'
#' @examples
#' data(sample_athletes)
#' head(sample_athletes)
#' summary(sample_athletes)
#'
"sample_athletes"

#' Sample Seasons Longitudinal Data Frame
#'
#' @description
#' A simulated longitudinal data frame recording the season-by-season
#' statistics of 10 fictional athletes over up to 8 seasons. Used to
#' demonstrate time-series and growth functions such as
#' \code{\link{season_growth}}, \code{\link{consistency_index}}, and
#' \code{\link{plot_performance}}.
#'
#' @format A data frame with up to 80 rows and 6 variables:
#' \describe{
#'   \item{name}{Character. Athlete's full name.}
#'   \item{sport}{Character. Sport.}
#'   \item{season}{Integer. Season year (e.g., 2018–2025).}
#'   \item{points}{Numeric. Average points per game that season.}
#'   \item{assists}{Numeric. Average assists per game that season.}
#'   \item{win_rate}{Numeric. Win rate that season (proportion, 0–1).}
#' }
#'
#' @source Simulated data generated for package demonstration purposes.
#'
#' @examples
#' data(sample_seasons)
#' head(sample_seasons)
#' plot_performance(sample_seasons, "name", "season", "points")
#'
"sample_seasons"
