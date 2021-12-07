## day 5 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day06/day06input.txt"
#input_loc <- "./day06/day06example.txt"

dat <- data.table::fread(input_loc, colClasses = "character", sep2 = "  -> ")
df <- tibble::as_tibble(dat)

df <- df |> pivot_longer(cols = everything()) |> select(value) |> rename(state = value) |> mutate(state = as.integer(state))

sim_period <- 80

for (i in 1:sim_period) {
  new_fish <- df |> filter(state == 0)
  new_fish_cnt <- dim(new_fish)[1]
  if (new_fish_cnt > 0) {
    for (j in 1:new_fish_cnt) {
      df <- df |> add_row(state = 9)  
    }
  }
  df <- df |> mutate(state = state - 1) |> mutate(state = if_else(state == -1, 6, state))
}
dim(df)[1]