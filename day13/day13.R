## day 13 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day13/day13input.txt"
#input_loc <- "./day13/day13example.txt"

dat <- data.table::fread(input_loc, header = FALSE, fill=FALSE)
df <- tibble::as_tibble(dat) |> rename(x = V1, y = V2) |> mutate(x = x+1, y = y+1)
dat2 <- data.table::fread(input_loc, header = FALSE, skip = dim(df)[1])
folds <- as_tibble(dat2) |> select(V3)
folds <- as_tibble(str_split_fixed(folds$V3, "=", n = Inf)) |> mutate(V2 = as.integer(V2)) |> mutate(V2 = V2 + 1)


populate_map <- function(df) {
  map <- as_tibble(matrix(nrow = max(df$y), ncol = max(df$x), 0))
  for (i in 1:dim(df)[1]) {
    map[as.numeric((df[i,2])), as.numeric((df[i,1]))] <- 1
  }
  map
}

fold_map_x <- function(map, x) {
  df1 <- map[,1:(x-1)]
  df2 <- map[,((x+1):dim(map)[2])]
  df2 <- df2[,dim(df2)[2]:1]
  ## what if not same size?
  if (dim(df1)[2] > dim(df2)[2]) {
    dfo <- as_tibble(df1[,(dim(df1)[2] - dim(df2)[2]):dim(df1)[2]] + df2)
    dfo <- bind_cols(df1[,1:(dim(df1)[2] - dim(df2)[2])], dfo)
  } else if (dim(df1)[2] < dim(df2)[2]) {
    dfo <- as_tibble(df1 + df2[,1:dim(df1)[2]])
    dfo <- bind_cols(dfo, df2[,dim(df1)[2]:dim(df2)[2]])
  } else {
    dfo <- as_tibble(df1 + df2)
  }
  dfo
}

fold_map_y <- function(map, y) {
  df1 <- map[1:y-1,]
  df2 <- map[(y+1):dim(map)[1],]
  df2 <- df2[dim(df2)[1]:1,]
  ## what if not same size?
  if (dim(df1)[1] > dim(df2)[1]) {
    dfo <- as_tibble(df1[(dim(df1)[1] - dim(df2)[1] + 1):(dim(df1)[1]),] + df2)
    dfo <- bind_rows(df1[1:(dim(df1)[1]-dim(df2)[1]),], dfo)
  } else if (dim(df1)[1] < dim(df2)[1]) {
    dfo <- as_tibble(df1 + df2[(1:dim(df1)[1]),])
    dfo <- bind_rows(dfo, df2[dim(df1)[1]:dim(df2)[1],])
  } else {
    dfo <- as_tibble(df1 + df2)
  }
  dfo
}

fold_map <- function(map, fold) {
  x <- NA
  y <- NA
  if (fold[1,1] == "x") {
    map[,as.numeric(fold[1,2])] <- NA
    x <- as.numeric(fold[1,2])
    y <- y
  } else if (fold[1,1] == "y") {
    map[as.numeric(fold[1,2]),] <- NA
    y <- as.numeric(fold[1,2])
    x <- x
  } else {
    map <- map
    y <- y
    x <- x
  }
  
  if (is.na(x)) {
    map <- fold_map_y(map,y)
  } else if (is.na(y)) {
    map <- fold_map_x(map,x)
  } else {
    map <- map
  }
  map
}

count_dots <- function(map) {
  cnt <- sum(map > 0)
  cnt
}


map <- df |> populate_map()

#for(i in 1:2) {
for(i in 1:dim(folds)[1]) {  
  map <- fold_map(map, folds[i,])
}

cleanup_output <- function(map) {
  map <- map |> replace(map < 1, NA) |> replace(map > 0, 1) 
}

answer2 <- cleanup_output(map)
answer2