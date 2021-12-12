## day 12 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

#input_loc <- "./day12/day12input.txt"
input_loc <- "./day12/day12example1.txt"
#input_loc <- "./day12/day12example2.txt"
#input_loc <- "./day12/day12example3.txt"

dat <- data.table::fread(input_loc, header = FALSE, na.strings = c("", NA))
dat <- str_split_fixed(dat$V1, "-", n = Inf)

df <- tibble::as_tibble(dat, .name_repair = "minimal")
colnames(df) <- c("V1", "V2")
df2 <- df |> select(V2, V1) |> rename(V1 = V2, V2 = V1)
df <- union(df, df2) |> filter(V1 != "end") |> filter(V2 != "start")

n_starts <- dim(df |> filter(V1 == "start"))[1]
starts <- df |> filter(V1 == "start")
paths <- df |> filter(V1 != "start")





tmp <- as_tibble(matrix(nrow=100, ncol=100))
tmp[1,1:2] <- starts[1,1:2]
cnt <- dim(paths)

row <- 1

col <- 2

tmp_add <- paths |> filter(V1 == as.character(tmp[row,col]))
tmp[row:(row+dim(tmp_add)[1]-1), col] <- tmp[row,col]
tmp[row:(row+dim(tmp_add)[1]-1), (col+1):(col+2)] <- tmp_add[1:dim(tmp_add)[1], 1:2]

col <- 4 #loop across all rows before moving to col 6
tmp_add  <- paths |> filter(V1 == as.character(tmp[row,col]))
#tmp[row:(row+dim(tmp_add)[1]-1), col-1] <- tmp[row,col-1]
tmp[row:(row+dim(tmp_add)[1]-1), (col+1):(col+2)] <- tmp_add[1:dim(tmp_add)[1], 1:2]

row <- row + dim(tmp_add)[1]#2
tmp_add  <- paths |> filter(V1 == as.character(tmp[row,col]))
tmp[row:(row+dim(tmp_add)[1]-1), (col+1):(col+2)] <- tmp_add[1:dim(tmp_add)[1], 1:2]

row <- row + dim(tmp_add)[1]#2
tmp_add  <- paths |> filter(V1 == as.character(tmp[row,col]))


row <- 3
col <- 4
tmp_add  <- paths |> filter(V1 == as.character(tmp[row,col]))
tmp[row:(row+dim(tmp_add)[1]-1), (col+1):(col+2)] <- tmp_add[1:dim(tmp_add)[1], 1:2]



