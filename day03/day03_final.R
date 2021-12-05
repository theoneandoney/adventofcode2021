#day03_final.R

library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")

input_loc <- "./day03/day03input.txt"

dat <- data.table::fread(input_loc, colClasses = "character")
df <- tibble::as_tibble(dat)
len <- str_length(df[1, 1])
df <- df |>
  separate(V1, c("bin","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12"), "") |> # nolint
  select(c1:c12) |>
  mutate(across(.cols = 1:12, .fns = as.integer))

get_value <- function(x, bc) {
#  n <- length(x)
  n <- nrow(x)
  if (bc == "mcv") {
    if (sum(x) > n / 2) {
      value <- 1
    } else if (sum(x) < n / 2) {
      value <- 0
    } else {
      value <- 1
    }
  } else if (bc == "lcv") {
    if (sum(x) > n / 2) {
      value <- 0
    } else if (sum(x) < n / 2) {
      value <- 1
    } else {
      value <- 0
    }
  } else {
    (print("error"))
  }
  value
}

filter_df <- function(df, bc) {
#  l <- str_length(df[1, 1])
  l <- ncol(df)
  y <- NA
  if (bc == "mcv" | bc == "lcv") {
    for (i in 1:l) {
      if (dim(df)[1] == 1) {
        # this is solution
        y <- df
        y
        break
      } else if (dim(df)[1] > 1) {
        if (i == l) {
          val <- get_value(df[,i], bc)
          df <- df |> filter(select(df, i) == val)
          y <- df
          y
          break
        } else {
        val <- get_value(df[,i], bc)
        df <- df |> filter(select(df, i) == val)
        }
      } else {
        print("error")
      }
    }
  }
  y
}

o2_gen_rating <- filter_df(df, "mcv") |> unite("o2", sep = "") |> select(o2) |> strtoi(base=2)
co2_rating <- filter_df(df, "lcv") |> unite("co2", sep = "") |> select(co2) |> strtoi(base=2)
life_support_rating <- o2_gen_rating * co2_rating
life_support_rating