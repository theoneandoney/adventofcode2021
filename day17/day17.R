## day 17 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day17/day17input.txt"
#input_loc <- "./day17/day17example.txt"

dat <- fread(input_loc, nrows = 1, header = FALSE)

tgt_x <- str_split_fixed(dat$V1, ": ", n = Inf)[1,2]
tgt_x <- str_split_fixed(tgt_x, "x=", n = Inf)[1,2]
tgt_x <- str_split_fixed(tgt_x, "\\.\\.", n = Inf) |> as.integer()


tgt_y <- str_split_fixed(dat$V2, "=", n = Inf)[1,2]
tgt_y <- str_split_fixed(tgt_y, "\\.\\.", n = Inf) |> as.integer()

a <- tgt_x[1]
b <- tgt_x[2]
d <- tgt_y[1]
c <- tgt_y[2]

test_shot <- function(v0, a, b, c, d) {
  v0 <- as.numeric(v0)
  i <- 0:(2*(abs(d) + 1) + 1)
  v_x <- v0[1] - i
  v_x[v_x < 0] <- 0
  v_y <- v0[2] - i
  x <- c(0, cumsum(v_x))
  y <- c(0, cumsum(v_y))
  any(a <= x & x <= b & d <= y & y <= c)
}

v_x_range <- ceiling((sqrt(8 * a + 1) - 1)/2):b
v_y_range <- d:(-(d+1))
dat <- expand.grid(v_x_range, v_y_range)
dat$hit <- apply(dat, 1, test_shot, a, b, c, d)
sum(dat$hit)

