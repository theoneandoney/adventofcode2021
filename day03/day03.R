## day 3 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")

input_loc <- "./day03/day03input.txt"

dat <- data.table::fread(input_loc, colClasses = 'character')
df <- tibble::as_tibble(dat)
n <- dim(df)[1]

len <- str_length(df[1,1])

df2 <- df |> separate(V1, c("bin","c1","c2","c3","c4","c5","c6","c7","c8","c9","c10","c11","c12"), "")
df <- df2 |> mutate(bin = df$V1)
rm(df2)

df <- df |> mutate(across(.cols = 2:13, .fns = as.integer))

gamma <- tibble("g1" = 0, "g2" = 0, "g3" = 0, "g4" = 0, "g5" = 0, "g6" = 0, "g7" = 0, "g8" = 0, "g9" = 0, "g10" = 0, "g11" = 0, "g12" = 0)
epsilon <- tibble("g1" = 0, "g2" = 0, "g3" = 0, "g4" = 0, "g5" = 0, "g6" = 0, "g7" = 0, "g8" = 0, "g9" = 0, "g10" = 0, "g11" = 0, "g12" = 0)
mcb <- tibble("g1" = 0, "g2" = 0, "g3" = 0, "g4" = 0, "g5" = 0, "g6" = 0, "g7" = 0, "g8" = 0, "g9" = 0, "g10" = 0, "g11" = 0, "g12" = 0)
lcb <- tibble("g1" = 0, "g2" = 0, "g3" = 0, "g4" = 0, "g5" = 0, "g6" = 0, "g7" = 0, "g8" = 0, "g9" = 0, "g10" = 0, "g11" = 0, "g12" = 0)

for (i in 1:len) {
  gamma[1,i] <- if_else(sum(df[,i+1]) > (n/2), 1, 0)
  epsilon[1,i] <- if_else(sum(df[,i+1]) > (n/2), 0, 1)
  mcb[1,i] <- if_else(sum(df[,i+1]) >= (n/2), 1, 0)
  lcb[1,i] <- if_else(sum(df[,i+1]) > (n/2), 0, 1)
}

gamma <- gamma |> mutate(across(.fns = as.character))
epsilon <- epsilon |> mutate(across(.fns = as.character))

g <- gamma |> unite("gamma", g1:g12, sep = "") |> strtoi(base=2)
e <- epsilon |> unite("epsilon", g1:g12, sep = "") |> strtoi(base=2)

solution1 <- g*e

# df2 <- df
# 
# ## solution to part 2
# for (i in 1:len) {
#   if (dim(df2)[1] > 1) {
#     if (sum(df2[,i+1]) >= (n/2)){
#       df2 <- df2 |> filter(select(df2,i+1) == 1)
#     #df2 <- df2 |> filter(select(df2,i+1) == as.numeric(mcb[1,i]))
#     } else {
#       df2 <- df2 |> filter(select(df2,i+1) == 0)
#     }
#   } else {
#     # solution
#     o2_gen_rating <- df2 |> select(1) |> strtoi(base=2)
# #    o2_gen_rating <- df2 |> unite("o2", 1:i+1, sep = "") |> select(o2) |> strtoi(base=2)
#     break
#   }
# }
# 
# df3 <- df
# 
# for (i in 1:len) {
#   if (dim(df3)[1] > 1) {
#     if (sum(df3[,i+1]) <= (n/2)){
#       df3 <- df3 |> filter(select(df3,i+1) == 0)
# #    df3 <- df3 |> filter(select(df3,i+1) == as.numeric(lcb[1,i]))
#     } else{
#       df3 <- df3 |> filter(select(df3,i+1) == 1)
#     }
#   } else {
#     # solution
#     co2_rating <- df3 |> select(1) |> strtoi(base=2)
# #    co2_rating <- df3 |> unite("co2", 1:i+1, sep = "") |> select(co2) |> strtoi(base=2)
#     break
#   }
# }
# 
# life_support_rating <- o2_gen_rating * co2_rating
# life_support_rating
