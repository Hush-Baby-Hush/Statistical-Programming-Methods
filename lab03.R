#exercise 1
set.seed(42)
x = rnorm(100)
y = sample(letters, size = 1000, replace = TRUE)
z = sample(c(TRUE, FALSE, NA), size = 100, replace = TRUE)

sum(x[seq(0,100,2)])

(sum(y=="a")+sum(y=="e")+sum(y=="i")+sum(y=="o")+sum(y=="u"))/1000

sum(is.na(z))

#exercise 2
absurd_list = list(
  q = "Look elsewhere.", 
  y = list(
    z = list(x = 1),
    x = list(x = 2),
    zzz = list(
      zzz = "Not here.",
      zz = list(qq = "gg"),
      a = list(
        b = list(
          q = list(
            answer = list(
              answer = "Hello World!")
          )))
    )
  ))

absurd_list[[2]][[3]][[3]][[1]][[1]][[1]]

#exercise 3
head(airquality)

answer = airquality[complete.cases(airquality[, c("Ozone","Solar.R")]), ]

all(answer == na.omit(airquality))

#exercise 4
library(nflreadr)
library(tidyverse)
rosters_2021 = as.data.frame(load_rosters(seasons = 2021))
rosters_2021 = rosters_2021[complete.cases(rosters_2021[, "college"]), ]
nfl_illini_2021 = select(rosters_2021[which(rosters_2021$college=="Illinois"),],team,position,jersey_number,full_name,height,weight)
nfl_illini_2021$height[which(nfl_illini_2021$full_name == "Nate Hobbs")]="6-0"
nfl_illini_2021

#exercise 5
nfl_2021 = as.data.frame(load_schedules(seasons = 2021))

library(dplyr)
df = filter(nfl_2021, home_team =="CHI" | away_team =="CHI")
select(df, week, away_team , home_team)








