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

get_ideal_loc <- function(df) {
  df <- df |> mutate(loc2 = median(df$loc1))
  df
}

measure_fuel <- function(df) {
  df <- df |> mutate(diff = df$loc2 - df$loc1)
  df <- df |> mutate(fuel = abs(df$diff))
  df
}

df <- df |> get_ideal_loc() |> measure_fuel()
answer_pt1 <- sum(df$fuel)
answer_pt1
