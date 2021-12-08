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
#input_loc <- "./day08/day08example.txt"
input_loc <- "./day08/day08simpleexample.txt"

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
#  df <- df |> select(out) |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df <- df |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df
}

cnt_output <- function(df) {
  df <- df |> mutate(cnto1 = str_length(out1)) |>
              mutate(cnto2 = str_length(out2)) |>
              mutate(cnto3 = str_length(out3)) |>
              mutate(cnto4 = str_length(out4))
}

cnt_signal <- function(df) {
  df <- df |> mutate(cnts1 = str_length(sig1)) |>
              mutate(cnts2 = str_length(sig2)) |>
              mutate(cnts3 = str_length(sig3)) |>
              mutate(cnts4 = str_length(sig4)) |>
              mutate(cnts5 = str_length(sig5)) |>
              mutate(cnts6 = str_length(sig6)) |>
              mutate(cnts7 = str_length(sig7)) |>
              mutate(cnts8 = str_length(sig8)) |>
              mutate(cnts9 = str_length(sig9)) |>
              mutate(cnts10 = str_length(sig10))
}

solution1 <- function(df) {
  df <- df |> select(cnto1:cnto4)
  ones <- sum(df$cnto1 == 2) + sum(df$cnto2 == 2) + sum(df$cnto3 == 2) + sum(df$cnto4 == 2)
  fours <- sum(df$cnto1 == 4) + sum(df$cnto2 == 4) + sum(df$cnto3 == 4) + sum(df$cnto4 == 4)
  sevens <- sum(df$cnto1 == 3) + sum(df$cnto2 == 3) + sum(df$cnto3 == 3) + sum(df$cnto4 == 3)
  eights <- sum(df$cnto1 == 7) + sum(df$cnto2 == 7) + sum(df$cnto3 == 7) + sum(df$cnto4 == 7)
  ones + fours + sevens + eights
}


#stringi::stri_count_fixed(seq,"CG")

df1 <- df |> sep_output() |> cnt_output()

solution1 <- df1 |> solution1()

df2 <- df |> sep_signal() |> sep_output() |> cnt_output() |> cnt_signal()

test <- df |> sep_signal() |> sep_output()

decode_entry <- function (entry) {
  entry <- entry |> select(2:11, 13:16) |> add_row() 
#  entry[2,1] <- entry[1,1]
  
  for (i in 1:14) {
    entry[2,i] <- case_when(
      str_length(entry[1,i]) == 2 ~ "One",
      str_length(entry[1,i]) == 4 ~ "Four",
      str_length(entry[1,i]) == 3 ~ "Seven",
      str_length(entry[1,i]) == 7 ~ "Eight"
    )
  }
  entry
}

test <- test |> decode_entry()
test