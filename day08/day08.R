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

#input_loc <- "./day08/day08input.txt"
input_loc <- "./day08/day08example.txt"

dat <- data.table::fread(input_loc, colClasses = "character", sep2 = " | ", header = FALSE)
df <- tibble::as_tibble(dat) |> rename (signal = V1, out = V2)

#df1 <- df |> select(out) |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)

sep_signal <- function(df) {
  # df <- df |> select(signal) |> separate(signal, into = c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "sig9", "sig10"),
  #                                        sep =" ", extra = 'drop', remove = FALSE)
  df <- df |> separate(signal, into = c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "sig9", "sig10"),
                       sep =" ", extra = 'drop', remove = FALSE)
  df
}

sep_output <- function(df) {
  df <- df |> select(out) |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df <- df |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df
}

cnt_output <- function(df) {
  df <- df |> mutate(cnt1 = str_length(out1)) |>
              mutate(cnt2 = str_length(out2)) |>
              mutate(cnt3 = str_length(out3)) |>
              mutate(cnt4 = str_length(out4))
}

solution1 <- function(df) {
  df <- df |> select(cnt1:cnt4)
  ones <- sum(df$cnt1 == 2) + sum(df$cnt2 == 2) + sum(df$cnt3 == 2) + sum(df$cnt4 == 2)
  fours <- sum(df$cnt1 == 4) + sum(df$cnt2 == 4) + sum(df$cnt3 == 4) + sum(df$cnt4 == 4)
  sevens <- sum(df$cnt1 == 3) + sum(df$cnt2 == 3) + sum(df$cnt3 == 3) + sum(df$cnt4 == 3)
  eights <- sum(df$cnt1 == 7) + sum(df$cnt2 == 7) + sum(df$cnt3 == 7) + sum(df$cnt4 == 7)
  ones + fours + sevens + eights
}


#stringi::stri_count_fixed(seq,"CG")

df1 <- df |> sep_output() |> cnt_output()

solution1 <- df1 |> solution1()

df2 <- df |> sep_signal() |> sep_output()
