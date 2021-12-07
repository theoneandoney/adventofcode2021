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

df <- df |> pivot_longer(cols = everything()) |> select(value) |> rename(state = value) |> mutate(state = as.double(state))

sim_period <- 256


df <- table(df$state) |> as.data.frame()
df <- t(df) |> as.data.frame() |> as_tibble()

df <- df |> mutate(V1 = as.double(V1)) |> mutate(V2 = as.double(V2)) |> 
  mutate(V3 = as.double(V3)) |> mutate(V4 = as.double(V4)) |> mutate(V5 = as.double(V5))
df <- df[2,]

df <- df |> mutate(V0 = 0) |> mutate(V6 = 0) |> mutate(V7 = 0) |> mutate(V8 = 0) |> #mutate(V5 = 0) |> 
  select(V0, V1, V2, V3, V4, V5, V6, V7, V8)

for (i in 1:sim_period) {
  df <- df |> add_row()
  for (j in 1:9) {
    if (j == 9) {
      df[i+1,j] <- df[i, 1]
    } else if (j == 7) {
      df[i+1,j] <- df[i,j+1] + df[i,1]
    } else {
      df[i+1,j] <- df[i,j+1]
    }
  }
}

solution <- sum(df[dim(df)[1],])
solution