#' athleteR: Sports and Athletics Data Analysis Tools
#'
#' @description
#' The \pkg{athleteR} package provides a comprehensive set of tools for
#' analyzing sports and athletics performance data. It introduces S3 classes
#' for representing athletes and teams, along with functions to compute
#' performance metrics, compare players, summarize team statistics, and
#' produce publication-ready visualizations.
#'
#' @section Main S3 Classes:
#' \describe{
#'   \item{\code{athlete}}{Represents an individual athlete with performance history.}
#'   \item{\code{team}}{Represents a team composed of multiple athlete objects.}
#' }
#'
#' @section Key Functions:
#' \describe{
#'   \item{\code{\link{new_athlete}}}{Construct a new athlete S3 object.}
#'   \item{\code{\link{new_team}}}{Construct a new team S3 object.}
#'   \item{\code{\link{performance_score}}}{Compute a composite performance score.}
#'   \item{\code{\link{compare_athletes}}}{Compare two or more athletes side-by-side.}
#'   \item{\code{\link{team_summary}}}{Summarize team-level statistics.}
#'   \item{\code{\link{plot_performance}}}{Visualize athlete performance over time.}
#'   \item{\code{\link{top_performers}}}{Rank and return top athletes by metric.}
#'   \item{\code{\link{normalize_stats}}}{Normalize raw statistics to a 0-100 scale.}
#' }
#'
#' @section Built-in Datasets:
#' \describe{
#'   \item{\code{\link{sample_athletes}}}{A data frame of 50 simulated athletes.}
#'   \item{\code{\link{sample_seasons}}}{A longitudinal data frame of season-level stats.}
#' }
#'
#' @docType package
#' @name athleteR-package
#' @aliases athleteR
"_PACKAGE"
