## day 9 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day09/day09input.txt"
#input_loc <- "./day09/day09example.txt"


dat <- data.table::fread(input_loc, header = FALSE)
dat <- str_split_fixed(dat$V1, "", n = Inf)
df <- tibble::as_tibble(dat) |> mutate(across(.cols = everything(), .fns = as.double))


find_low_pts <- function(df) {
  dat_temp <- matrix("9", nrow=1, ncol=dim(dat)[2])
  dat_temp <- tibble::as_tibble(dat_temp) |> mutate(across(.cols = everything(), .fns = as.double))
  df <- bind_rows(dat_temp, df)
  df <- bind_rows(df, dat_temp)
  df <- df |> add_column(first = 9, .before = "V1") |> add_column(last = 9)
  low_pts <- matrix(0, ncol = dim(df)[2], nrow = dim(df)[1])
  low_pts <- as.tibble(low_pts) |> mutate(across(.cols = everything(), .fns = as.double))
  colnames(low_pts) <- colnames(df)
  
  for (i in 2:(dim(df)[1]-1)) {
    for (j in 2:(dim(df)[2]-1)) {
      low_pts[i,j] <- (df[i,j] < df[i+1,j]) * (df[i,j] < df[i-1,j]) * (df[i,j] < df[i,j+1]) * (df[i,j] < df[i,j-1])
    }
  }
  y <- dim(low_pts)[1] - 1
  x <- dim(low_pts)[2] - 1
  low_pts <- low_pts[2:y,2:x]
}

calculate_risk <- function(df, low_pts) {
  risk <- (df + 1) * low_pts
  sum(risk)
}


lp <- df|> find_low_pts()
answer1 <- calculate_risk(df, lp)
answer1
