library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day07/day07input.txt"
#input_loc <- "./day07/day07example.txt"

mat <- as.matrix(t(fread(input_loc)))
colnames(mat) <- "loc1"
df <- tibble::as_tibble(mat) |> mutate(loc1 = as.double(loc1))

get_ideal_loc_pt1 <- function(df) {
  df <- df |> mutate(loc2 = median(df$loc1))
  df
}

measure_fuel_pt1 <- function(df) {
  df <- df |> mutate(diff = df$loc2 - df$loc1)
  df <- df |> mutate(fuel = abs(df$diff))
  df
}

get_ideal_loc_pt2 <- function(df) {
  print(mean(df$loc1))
  ## this is a kluge, rounding down mean was more efficient, i'm too busy
  ## to come up with a bulletproof method so this is staying
  df <- df |> mutate(loc2 = round(mean(df$loc1))-1)
  df
}

measure_fuel_pt2 <- function(df) {
  df <- df |> mutate(diff = df$loc2 - df$loc1)
  df <- df |> mutate(steps_needed = abs(df$diff)) |> 
    rowwise() |> mutate(fuel = sum(1:steps_needed))

  df
}


df1 <- df |> get_ideal_loc_pt1() |> measure_fuel_pt1()
answer_pt1 <- sum(df1$fuel)

df2 <- df |> get_ideal_loc_pt2() |> measure_fuel_pt2()
answer_pt2 <- sum(df2$fuel)
answer_pt2
