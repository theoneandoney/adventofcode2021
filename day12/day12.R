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

input_loc <- "./day12/day12input.txt"
#input_loc <- "./day12/day12example1.txt"
#input_loc <- "./day12/day12example2.txt"
#input_loc <- "./day12/day12example3.txt"

dat <- data.table::fread(input_loc, header = FALSE, na.strings = c("", NA))
dat <- str_split_fixed(dat$V1, "-", n = Inf)

df <- tibble::as_tibble(dat, .name_repair = "minimal")
colnames(df) <- c("V1", "V2")
df2 <- df |> select(V2, V1) |> rename(V1 = V2, V2 = V1)
df <- union(df, df2) |> filter(V1 != "end") |> filter(V2 != "start")

starts <- df |> filter(V1 == "start")
paths <- df |> filter(V1 != "start")
dfo <- as_tibble(matrix(nrow=100, ncol=25))
counts <- as_tibble(matrix(nrow=100, ncol=25))

row <- 1
col <- 1

counts[row,col] <- dim(starts)[1]
dfo[row,col] <- starts[row, col]
lower <- FALSE

case_is_lower <- function(string) {
  lower <- FALSE
  if (string == "start" | string == "end" | is.na(string)){
    lower <- FALSE
  } else if (str_detect(string,"[[:upper:]]")) {
    lower <- FALSE
  } else {
    lower <- TRUE
  }
  lower
}


while(counts[row,1] != 0) {
  if (counts[row,col] > 0) {
    paths <- df |> filter(V1 == as.character(dfo[row,col]))
    lower_case <- case_is_lower(as.character(paths[as.integer(counts[row,col]),2]))
    
    if (is.na(lower_case)) {
      dfo[row,col+1] <- paths[as.integer(counts[row,col]),2]
      counts[row,col+1] <- dim(df |> filter(V1 == as.character(dfo[row,col+1])))[1]
      row <- row
      col <- col + 1
    } else if (lower_case == TRUE) {
      if (is.na(any(dfo[row,] == as.character(paths[as.integer(counts[row,col]),2]))) | 
          any(dfo[row,] == as.character(paths[as.integer(counts[row,col]),2])) == FALSE) {
        dfo[row,col+1] <- paths[as.integer(counts[row,col]),2]
        counts[row,col+1] <- dim(df |> filter(V1 == as.character(dfo[row,col+1])))[1]
        row <- row
        col <- col + 1
      } else {
        dfo[row,col+1] <- NA
        counts[row,col+1] <- 0
        row <- row
        col <- col + 1
      }
    } else {
      dfo[row,col+1] <- paths[as.integer(counts[row,col]),2]
      counts[row,col+1] <- dim(df |> filter(V1 == as.character(dfo[row,col+1])))[1]
      row <- row
      col <- col + 1
    }
  } else {
    dfo[row+1,1:(col-1)] <- dfo[row,1:(col-1)]
    if (col > 2) {
      counts[row+1,1:(col-2)] <- counts[row,1:(col-2)]  
    }
    
    counts[row+1,col-1] <- counts[row,col-1]-1
    row <- row + 1
    col <- col - 1
  }

  
}

ends <- rowSums(dfo == "end", na.rm = TRUE)# |> as_tibble()
dfo <- dfo |> mutate(ends = ends) |> filter(ends == 1)
  
solution1 <- dim(dfo)[1]
solution1

