## day 14 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day14/day14input.txt"
#input_loc <- "./day14/day14example.txt"

dat <- data.table::fread(input_loc, header = FALSE, nrows=1)
#df <- as_tibble(str_split_fixed(dat$V1, "", n = Inf))
df <- as_tibble(dat)
colnames(df) <- c("V1")
dat2 <- data.table::fread(input_loc, header = FALSE, skip = 2)
rules <- as_tibble(dat2) |> select(1,3) |> rename(V2 = V3)



insert_pairs <- function(string, rules) {
  len <- str_length(string)
  string <- as.character(string)
  output <- "" #str_sub(string, 1,1)
  temp <- ""
  insert <- ""
  for (i in 1:(len-1)) {
    temp <- str_sub(string, i, i+1)
    insert <- rules |> filter(V1 == temp) |> select(V2) |> as.character()
    output <- str_glue(output, str_sub(string, i, i), insert)
  }
  output <- str_glue(output, str_sub(string, len, len))
  output
}

pair_insertion_process <- function(df, rules, n) {
  output <- df
  for (i in 1:n) {
#    insert_pairs(df[i,1], rules)
    output <- output |> add_row(V1 = insert_pairs(output[i,1], rules))
#    output[i+1,1] <- insert_pairs(df[i,1], rules)
  }
  output
}

solution_pt1 <- function(df, n) {
  string <- df[n+1,1]
  freq <- as_tibble(sort(table(unlist(strsplit(as.character(string[1,1]), split = NULL))), decreasing = TRUE), .name_repair = "minimal")
  solution <- freq[1, 2] - freq[(dim(freq)[1]), 2]
}

n <- 10
solution <- df |> pair_insertion_process(rules,n) |> solution_pt1(n)
solution