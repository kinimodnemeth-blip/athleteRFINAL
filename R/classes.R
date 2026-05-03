# ============================================================
#  S3 Classes: athlete and team
# ============================================================

#' Create a New Athlete Object
#'
#' @description
#' Constructs an S3 object of class \code{athlete} representing an individual
#' athlete with associated metadata and performance statistics.
#'
#' @param name      Character. Full name of the athlete.
#' @param sport     Character. Sport the athlete competes in (e.g., \code{"basketball"}).
#' @param position  Character. Playing position (e.g., \code{"guard"}).
#' @param age       Numeric. Athlete's age in years.
#' @param stats     Named numeric vector. Performance statistics (e.g., points, assists).
#' @param seasons   Integer. Number of professional seasons played. Default is \code{1}.
#'
#' @return An object of class \code{athlete} (a named list) with components:
#' \describe{
#'   \item{name}{Athlete name.}
#'   \item{sport}{Sport name.}
#'   \item{position}{Playing position.}
#'   \item{age}{Age.}
#'   \item{stats}{Named numeric vector of statistics.}
#'   \item{seasons}{Seasons played.}
#' }
#'
#' @examples
#' a <- new_athlete(
#'   name     = "Jordan Smith",
#'   sport    = "basketball",
#'   position = "guard",
#'   age      = 26,
#'   stats    = c(points = 22.4, assists = 7.1, rebounds = 4.3),
#'   seasons  = 4
#' )
#' print(a)
#'
#' @export
new_athlete <- function(name, sport, position, age, stats, seasons = 1) {
  if (!is.character(name) || length(name) != 1)
    stop("`name` must be a single character string.")
  if (!is.numeric(stats) || is.null(names(stats)))
    stop("`stats` must be a named numeric vector.")
  if (!is.numeric(age) || age < 0)
    stop("`age` must be a non-negative number.")

  structure(
    list(
      name     = name,
      sport    = sport,
      position = position,
      age      = age,
      stats    = stats,
      seasons  = as.integer(seasons)
    ),
    class = "athlete"
  )
}

#' Print Method for Athlete Objects
#'
#' @description Displays a human-readable summary of an \code{athlete} object.
#'
#' @param x   An object of class \code{athlete}.
#' @param ... Further arguments passed to or from other methods (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' a <- new_athlete("Jane Doe", "soccer", "forward", 24,
#'                  c(goals = 18, assists = 9), seasons = 3)
#' print(a)
#'
#' @export
print.athlete <- function(x, ...) {
  cat("Athlete:", x$name, "\n")
  cat("Sport  :", x$sport, "| Position:", x$position, "\n")
  cat("Age    :", x$age, "| Seasons:", x$seasons, "\n")
  cat("Stats  :\n")
  for (nm in names(x$stats)) {
    cat(sprintf("  %-15s %s\n", nm, x$stats[[nm]]))
  }
  invisible(x)
}

#' Summary Method for Athlete Objects
#'
#' @description Returns a brief statistical summary of an \code{athlete} object.
#'
#' @param object An object of class \code{athlete}.
#' @param ...    Further arguments (ignored).
#'
#' @return Invisibly returns a list with \code{name}, \code{sport}, and
#'   \code{stat_summary} (output of \code{summary(stats)}).
#'
#' @examples
#' a <- new_athlete("Jane Doe", "soccer", "forward", 24,
#'                  c(goals = 18, assists = 9, saves = 0), seasons = 3)
#' summary(a)
#'
#' @export
summary.athlete <- function(object, ...) {
  cat("=== Athlete Summary ===\n")
  cat("Name  :", object$name, "\n")
  cat("Sport :", object$sport, "\n")
  print(summary(object$stats))
  invisible(list(name = object$name, sport = object$sport,
                 stat_summary = summary(object$stats)))
}

# -------------------------------------------------------

#' Create a New Team Object
#'
#' @description
#' Constructs an S3 object of class \code{team} from a list of
#' \code{athlete} objects.
#'
#' @param name     Character. Team name.
#' @param sport    Character. Sport the team competes in.
#' @param athletes List of objects of class \code{athlete}.
#' @param season   Character or integer. Season identifier (e.g., \code{"2025-26"}).
#'
#' @return An object of class \code{team} containing:
#' \describe{
#'   \item{name}{Team name.}
#'   \item{sport}{Sport name.}
#'   \item{athletes}{List of \code{athlete} objects.}
#'   \item{season}{Season identifier.}
#'   \item{roster_size}{Number of athletes on the roster.}
#' }
#'
#' @examples
#' a1 <- new_athlete("Alice", "basketball", "center", 28,
#'                   c(points = 18, rebounds = 10), 5)
#' a2 <- new_athlete("Bob", "basketball", "guard", 24,
#'                   c(points = 14, assists = 8), 2)
#' t1 <- new_team("Dream Team", "basketball", list(a1, a2), "2025-26")
#' print(t1)
#'
#' @export
new_team <- function(name, sport, athletes, season) {
  if (!is.list(athletes))
    stop("`athletes` must be a list of athlete objects.")
  bad <- !vapply(athletes, inherits, logical(1), "athlete")
  if (any(bad))
    stop("All elements of `athletes` must be objects of class 'athlete'.")

  structure(
    list(
      name        = name,
      sport       = sport,
      athletes    = athletes,
      season      = as.character(season),
      roster_size = length(athletes)
    ),
    class = "team"
  )
}

#' Print Method for Team Objects
#'
#' @description Displays a formatted roster summary of a \code{team} object.
#'
#' @param x   An object of class \code{team}.
#' @param ... Further arguments (ignored).
#'
#' @return Invisibly returns \code{x}.
#'
#' @examples
#' a1 <- new_athlete("Alice", "basketball", "center", 28,
#'                   c(points = 18, rebounds = 10), 5)
#' t1 <- new_team("Dream Team", "basketball", list(a1), "2025-26")
#' print(t1)
#'
#' @export
print.team <- function(x, ...) {
  cat("Team  :", x$name, "\n")
  cat("Sport :", x$sport, "| Season:", x$season, "\n")
  cat("Roster:", x$roster_size, "athletes\n")
  cat("Members:\n")
  for (a in x$athletes) {
    cat(sprintf("  - %-20s (%s, age %d)\n", a$name, a$position, a$age))
  }
  invisible(x)
}

#' Summary Method for Team Objects
#'
#' @description Aggregates and prints team-level statistics.
#'
#' @param object An object of class \code{team}.
#' @param ...    Further arguments (ignored).
#'
#' @return Invisibly returns a data frame of aggregated statistics.
#'
#' @examples
#' a1 <- new_athlete("Alice", "basketball", "center", 28,
#'                   c(points = 18, rebounds = 10), 5)
#' a2 <- new_athlete("Bob", "basketball", "guard", 24,
#'                   c(points = 14, rebounds = 5), 2)
#' t1 <- new_team("Dream Team", "basketball", list(a1, a2), "2025-26")
#' summary(t1)
#'
#' @export
summary.team <- function(object, ...) {
  cat("=== Team Summary:", object$name, "(", object$season, ") ===\n")
  result <- team_summary(object)
  print(result)
  invisible(result)
}
