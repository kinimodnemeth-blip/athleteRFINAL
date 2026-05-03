## data-raw/generate_datasets.R
## Run this script once to regenerate the package datasets.
## Then call: usethis::use_data(sample_athletes, sample_seasons, overwrite = TRUE)

set.seed(42)

sports    <- c("basketball", "soccer", "baseball", "tennis", "track")
positions <- list(
  basketball = c("guard", "forward", "center"),
  soccer     = c("forward", "midfielder", "defender", "goalkeeper"),
  baseball   = c("pitcher", "outfielder", "infielder", "catcher"),
  tennis     = c("singles", "doubles"),
  track      = c("sprinter", "distance", "field")
)

first_names <- c("Jordan","Alex","Morgan","Taylor","Casey","Riley","Jamie",
                 "Avery","Quinn","Blake","Skyler","Cameron","Dakota","Drew",
                 "Emerson","Finley","Hayden","Hunter","Kendall","Landen",
                 "Mackenzie","Nolan","Parker","Payton","Phoenix","Reese",
                 "River","Rowan","Sage","Sawyer","Spencer","Sterling",
                 "Sydney","Tatum","Teagan","Tristan","Tyler","Vaughn",
                 "Whitney","Zion","Aaron","Brianna","Carlos","Diana",
                 "Ethan","Fiona","Gabriel","Hannah","Isaac","Julia")
last_names  <- c("Smith","Johnson","Williams","Brown","Jones","Garcia",
                 "Miller","Davis","Rodriguez","Martinez","Hernandez","Lopez",
                 "Gonzalez","Wilson","Anderson","Thomas","Taylor","Moore",
                 "Jackson","Martin","Lee","Perez","Thompson","White",
                 "Harris","Sanchez","Clark","Ramirez","Lewis","Robinson",
                 "Walker","Young","Allen","King","Wright","Scott","Torres",
                 "Nguyen","Hill","Flores","Green","Adams","Nelson","Baker",
                 "Hall","Rivera","Campbell","Mitchell","Carter","Roberts")

n <- 50
sport_vec    <- sample(sports, n, replace = TRUE)
position_vec <- mapply(function(sp) sample(positions[[sp]], 1), sport_vec)

sample_athletes <- data.frame(
  name     = paste(first_names, last_names),
  sport    = sport_vec,
  position = position_vec,
  age      = sample(18:38, n, replace = TRUE),
  seasons  = sample(1:15, n, replace = TRUE),
  points   = round(stats::runif(n, 5, 35), 1),
  assists  = round(stats::runif(n, 0, 12), 1),
  rebounds = round(stats::runif(n, 0, 14), 1),
  win_rate = round(stats::runif(n, 0.2, 0.85), 3),
  stringsAsFactors = FALSE
)

## Longitudinal dataset: 10 athletes over 8 seasons
athlete_names <- sample_athletes$name[1:10]
athlete_sports <- sample_athletes$sport[1:10]
seasons_seq   <- 2018:2025

sample_seasons <- do.call(rbind, lapply(seq_along(athlete_names), function(i) {
  base_pts <- stats::runif(1, 8, 30)
  base_ast <- stats::runif(1, 1, 10)
  base_win <- stats::runif(1, 0.3, 0.75)
  ns <- length(seasons_seq)
  data.frame(
    name     = athlete_names[i],
    sport    = athlete_sports[i],
    season   = seasons_seq,
    points   = round(pmax(0, base_pts + cumsum(stats::rnorm(ns, 0.3, 1.2))), 1),
    assists  = round(pmax(0, base_ast + cumsum(stats::rnorm(ns, 0.1, 0.8))), 1),
    win_rate = round(pmin(1, pmax(0, base_win + cumsum(stats::rnorm(ns, 0, 0.05)))), 3),
    stringsAsFactors = FALSE
  )
}))

# Save to data/
save(sample_athletes, file = "data/sample_athletes.rda")
save(sample_seasons,  file = "data/sample_seasons.rda")

message("Datasets saved to data/")
