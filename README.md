# athleteR <img src="man/figures/logo.png" align="right" height="139" alt="" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/studentauthor/athleteR/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/studentauthor/athleteR/actions)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

## Overview

**athleteR** is an R package providing a comprehensive toolkit for analyzing
sports and athletics performance data. It introduces S3 classes for representing
athletes and teams, a suite of analytical functions, and publication-ready
base-R visualizations.

### Key Features

- **S3 Classes** — `athlete` and `team` objects with `print()` and `summary()` methods  
- **8+ Unique Functions** — performance scoring, athlete comparison, team summaries, rankings, normalization, growth rates, and consistency indices  
- **3 Visualization Types** — line charts, bar charts, and radar (spider) plots  
- **2 Built-in Datasets** — `sample_athletes` (cross-sectional, n=50) and `sample_seasons` (longitudinal, 10 athletes × 8 seasons)  
- **MIT Licensed** — free to use, modify, and distribute  

---

## Installation

Install directly from GitHub using `devtools`:

```r
# install.packages("devtools")
devtools::install_github("studentauthor/athleteR")
```

---

## Quick Start

```r
library(athleteR)

# --- Create athlete objects ---
a1 <- new_athlete(
  name     = "Jordan Smith",
  sport    = "basketball",
  position = "guard",
  age      = 26,
  stats    = c(points = 22.4, assists = 7.1, rebounds = 4.3),
  seasons  = 4
)

a2 <- new_athlete(
  name     = "Alex Rivera",
  sport    = "basketball",
  position = "forward",
  age      = 29,
  stats    = c(points = 18.8, assists = 4.2, rebounds = 9.5),
  seasons  = 7
)

# --- Compute performance scores ---
performance_score(a1)
performance_score(a1, weights = c(points = 3, assists = 1.5, rebounds = 1))

# --- Compare athletes ---
compare_athletes(a1, a2)

# --- Build a team ---
t1 <- new_team("Thunder Hawks", "basketball", list(a1, a2), "2025-26")
team_summary(t1)

# --- Work with built-in data ---
data(sample_athletes)
top_performers(sample_athletes, metric = "points", n = 5)
normalize_stats(sample_athletes, columns = c("points", "assists"))

data(sample_seasons)
season_growth(sample_seasons, "name", "season", "points")
consistency_index(sample_seasons, "name", "points")

# --- Visualize ---
plot_performance(sample_seasons, "name", "season", "points")
plot_top_performers(sample_athletes, metric = "points", n = 8)
plot_radar(a1, a2)
```

---

## Functions

| Function | Description |
|---|---|
| `new_athlete()` | Construct an `athlete` S3 object |
| `new_team()` | Construct a `team` S3 object |
| `performance_score()` | Weighted composite performance score |
| `compare_athletes()` | Side-by-side data frame comparison |
| `team_summary()` | Aggregate team-level statistics |
| `top_performers()` | Rank athletes by a numeric metric |
| `normalize_stats()` | Min-max normalize to a 0–100 scale |
| `season_growth()` | Season-over-season percentage growth |
| `consistency_index()` | Coefficient of variation across seasons |
| `plot_performance()` | Line chart of stats over time |
| `plot_top_performers()` | Horizontal bar chart of top athletes |
| `plot_radar()` | Radar/spider chart for two-athlete comparison |

---

## Datasets

| Dataset | Rows | Description |
|---|---|---|
| `sample_athletes` | 50 | Cross-sectional: one row per athlete, most recent season |
| `sample_seasons` | 80 | Longitudinal: 10 athletes × 8 seasons (2018–2025) |

---

## Documentation

Full vignette available after installation:

```r
vignette("getting-started", package = "athleteR")
```

---

## License

MIT © 2026 Student Author. See [LICENSE](LICENSE) for details.
