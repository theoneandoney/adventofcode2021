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

input_loc <- "./day05/day05input.txt"
#input_loc <- "./day05/day05example.txt"

dat <- data.table::fread(input_loc, colClasses = "character", sep2 = "  -> ")
df_coords <- tibble::as_tibble(dat)

df_coords <- df_coords |> separate(V2, c("y1","x2"), " -> ") |>
  rename(x1 = V1, y2 = V3) |>
  mutate(across(.fns = as.integer))

max_x <- max(df_coords$x1, df_coords$x2)+1
max_y <- max(df_coords$y1, df_coords$y2)+1

#map <- matrix(nrow = 10, ncol = 10, data = 0)
map <- matrix(nrow = max_y, ncol = max_x, data = 0)
map <- tibble::as_tibble(map)

#coords <- df_coords[1,]+1

## takes a matrix(map), and two sets of coordinates (x1,y1, x2, y2)
## coordinates must be between 1:dim(matrix), so have to convert from 0:9 
## format before using this function
draw_lines <- function (map, coords){
  x1 <- coords$x1
  x2 <- coords$x2
  y1 <- coords$y1
  y2 <- coords$y2
  if (x1 == x2){
    ## draw line along y axis
    for (i in y1:y2) {
      map[i,x1] <- map[i,x1] + 1
    }
  } else if (y1 == y2) {
    ## draw line along x axis
    for (j in x1:x2){
      map[y1,j] <- map[y1,j] + 1
    }
  } else if (abs(x1 - x2) == abs(y1 - y2)){
    ## diagonal
    if (y1 < y2) {up <- 1} else {up <- 0}
    if (x1 < x2) {right <-1} else {right <- 0}

    for (k in 0:(abs(x1 - x2))) {
      if (up == 1 & right == 1) { ## 45 degrees
        map[(y1 + k),(x1 + k)] <- map[(y1 + k),(x1 + k)] + 1
      } else if (up == 1 & right == 0) { ## 135 degrees
        map[(y1 + k),(x1 - k)] <- map[(y1 + k),(x1 - k)] + 1
      } else if (up == 0 & right == 0) { ## 225 degrees
        map[(y1 - k),(x1 - k)] <- map[(y1 - k),(x1 - k)] + 1
      } else {  ## 315 degrees
        map[(y1 - k),(x1 + k)] <- map[(y1 - k),(x1 + k)] + 1
      }
    }
  }
  map
}

update_map <- function(map, df_coords){
  for (k in 1:dim(df_coords)[1]) {
    map <- draw_lines(map, df_coords[k,]+1)
  }
  map
}

final_map <- update_map(map, df_coords)

calculate_overlaps <- function(map, threshold){
  overlaps <- 0
  for (i in 1:dim(map)[1]){
    overlaps <- overlaps + sum(map[,i] >= threshold)
  }
  overlaps
}


answer <- calculate_overlaps(final_map,2)