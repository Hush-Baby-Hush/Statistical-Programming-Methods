library(tidyverse)

nfl = nflreadr::load_schedules(seasons = 1999:2021)

impossible = tibble(
  los_score = c(0, 1, 1, 1, 1, 1, 1),
  win_score = c(1, 1, 2, 3, 4, 5, 7)
)

for (i in 0:100) {
  temp = tibble(
    win_score = i,
    los_score = (i + 1):100
  )
  impossible = bind_rows(impossible, temp)
}
