## day 11 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day11/day11input.txt"
#input_loc <- "./day11/day11example.txt"

dat <- data.table::fread(input_loc, header = FALSE, na.strings = c("", NA))
dat <- str_split_fixed(dat$V1, "", n = Inf)

df <- tibble::as_tibble(dat)
df <- df |> mutate(across(.cols = everything(), .fns = as.integer))


take_step <- function(df) {
  ## first add NA border to simplify the process, will remove later
  dat_temp <- matrix(NA, nrow=1, ncol=10)
  dat_temp <- tibble::as_tibble(dat_temp) |> mutate(across(.cols = everything(), .fns = as.integer))
  df <- bind_rows(dat_temp, df)
  df <- bind_rows(df, dat_temp)
  df <- df |> add_column(first = NA, .before = "V1") |> add_column(last = NA)
  
  ## increment all values
  df <- as_tibble(df + 1)
  keep_counting <- TRUE
  
  while (keep_counting == TRUE) {
    for (row in 2:11) {
      for (col in 2:11) {
        if (is.na(df[row,col])) {
          df[row,col] <- df[row,col]
        } else if (df[row,col] < 10) {
          df[row,col] <- df[row,col]
        } else {
          df[row-1,col-1] <- df[row-1,col-1] + 1
          df[row-1,col] <-  df[row-1,col] + 1
          df[row-1,col+1] <-  df[row-1,col+1] + 1
          df[row,col-1] <- df[row,col-1] + 1
          df[row,col+1] <- df[row,col+1] + 1
          df[row+1,col-1] <- df[row+1,col-1] + 1
          df[row+1,col] <- df[row+1,col] + 1
          df[row+1,col+1] <- df[row+1,col+1] + 1
          df[row,col] <- NA
          flashes[row,col] <- TRUE
          cnt_flashes <- cnt_flashes + 1
        }
      }
    }
    keep_counting <- any(df > 9)
    if (is.na(keep_counting)) {
      keep_counting <- FALSE
    }
  }
  df <- df |> select(V1:V10) |> filter(row_number() > 1) |> filter(row_number() < 11)
  df
}
   


#flashes <- df
#flashes[] <- FALSE

count_flashes <- function(df) {
  cnt <- sum(is.na(df))
  cnt
}

reset_zeros <- function(df) {
  df <- df |> mutate(across(everything(), .fns = replace_na, 0))
  df
}

solve_pt1 <- function(df, n_steps) {
  cnt_flashes <- 0
  for (i in 1:n_steps){
    x <- 0
    df <- df |> take_step() 
    x <- df |> count_flashes()
    cnt_flashes <- cnt_flashes + x
    df <- df |> reset_zeros()
  }
  cnt_flashes
}

#solution1 <- solve_pt1(df, 100)
#solution1


solve_pt2 <- function(df) {
  n_step <- 0
  keep_going <- TRUE
  while (keep_going == TRUE){
#  for (i in 1:Inf){
    n_step <- n_step + 1
    n_flashes <- 0
    df <- df |> take_step() 
    n_flashes <- df |> count_flashes()
    df <- df |> reset_zeros()
    if (n_flashes == 100) {
      keep_going <- FALSE
      break
    } else {
      keep_going <- TRUE
    }
  }
  n_step
}

solution2 <- solve_pt2(df)
solution2