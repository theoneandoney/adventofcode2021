# and here is day 01 in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("wesanderson")

input_loc <- "./day01/day01input.txt"

dat <- data.table::fread(input_loc)
df <- tibble::as_tibble(dat) |>
  mutate(increase = FALSE)

n <- dim(df)[1]

for(i in 2:n) {
 df$increase[i] <- if_else(slice(df, i) > slice(df, i-1), TRUE, FALSE)
}

df$increase[1] <- NA

## solution to part 1 = 1298
df |> select(increase) |> filter(!is.na(increase)) |> sum()
solution1 <- df

## for part 2, need a rolling 3-sum window
## maybe just create a new tibble and add rows as we go
df <- select(df, V1)
df_sums <- tibble(window = numeric(), window_sum = numeric())

for(i in 3:n) {
#  temp_sum <- slice(df, i) + slice(df, i-1) + slice(df, i-2)
  temp_sum <- slice(df, i) + slice(df, i-1) + slice(df, i-2)
  df_sums <- df_sums |> add_row(window = i-2, window_sum = as.numeric(temp_sum))
}

m <- dim(df_sums)[1]
df_sums <- df_sums |> mutate(sum_increase = FALSE)
for(j in 2:m){
  df_sums$sum_increase[j] <- if_else(df_sums$window_sum[j] > df_sums$window_sum[j-1], TRUE, FALSE)
}
df_sums$sum_increase[1] <- NA
df_sums |> select(sum_increase) |> filter(!is.na(sum_increase)) |> sum()