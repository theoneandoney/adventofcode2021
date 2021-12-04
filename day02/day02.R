## day 2 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")

input_loc <- "./day02/day02input.txt"

dat <- data.table::fread(input_loc)
df <- tibble::as_tibble(dat) |>
  rename(command = V1, units = V2)

position <- tibble(horizontal = 0, vertical = 0)
n <- dim(df)[1]

for (i in 1:n){
  if (df$command[i] == "forward"){
    position$horizontal[1] <- position$horizontal[1] + df$units[i]
  } else if (df$command[i] == "down"){
    position$vertical[1] <- position$vertical[1] + df$units[i]
  } else if (df$command[i] == "up"){
    position$vertical[1] <- position$vertical[1] - df$units[i]
  } 
}

position1 <- position
solution1 <- position[1,1]*position[1,2]
solution1[1,1]


position <- tibble(horizontal = 0, vertical = 0, aim = 0)

for (i in 1:n){
  if (df$command[i] == "forward"){
    position$horizontal[1] <- position$horizontal[1] + df$units[i]
    position$vertical[1] <- position$vertical[1] + (position$aim[1] * df$units[i])
  } else if (df$command[i] == "down"){
    position$aim[1] <- position$aim[1] + df$units[i]
  } else if (df$command[i] == "up"){
    position$aim[1] <- position$aim[1] - df$units[i]
  } 
}

position2 <- position
solution2 <- position[1,1]*position[1,2]
solution2[1,1]
