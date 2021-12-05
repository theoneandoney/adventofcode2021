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

df <- df |> separate(V1, c("bin","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12"), "") |>
  mutate(bin = df$V1) |>
  mutate(across(.cols = 2:13, .fns = as.integer))

gamma <- tibble("b1" = 0, "b2" = 0, "b3" = 0, "b4" = 0, "b5" = 0, "b6" = 0, "b7" = 0, "b8" = 0, "b9" = 0, "b10" = 0, "b11" = 0, "b12" = 0)
epsilon <- tibble("b1" = 0, "b2" = 0, "b3" = 0, "b4" = 0, "b5" = 0, "b6" = 0, "b7" = 0, "b8" = 0, "b9" = 0, "b10" = 0, "b11" = 0, "b12" = 0)
mcb <- tibble("b1" = 0, "b2" = 0, "b3" = 0, "b4" = 0, "b5" = 0, "b6" = 0, "b7" = 0, "b8" = 0, "b9" = 0, "b10" = 0, "b11" = 0, "b12" = 0)
lcb <- tibble("b1" = 0, "b2" = 0, "b3" = 0, "b4" = 0, "b5" = 0, "b6" = 0, "b7" = 0, "b8" = 0, "b9" = 0, "b10" = 0, "b11" = 0, "b12" = 0)

for (i in 1:len) {
  gamma[1,i] <- if_else(sum(df[,i+1]) > (n/2), 1, 0)
  epsilon[1,i] <- if_else(sum(df[,i+1]) > (n/2), 0, 1)
  mcb[1,i] <- if_else(sum(df[,i+1]) >= (n/2), 1, 0)
  lcb[1,i] <- if_else(sum(df[,i+1]) > (n/2), 0, 1)
}

gamma <- gamma |> mutate(across(.fns = as.character))
epsilon <- epsilon |> mutate(across(.fns = as.character))

g <- gamma |> unite("gamma", b1:b12, sep = "") |> strtoi(base=2)
e <- epsilon |> unite("epsilon", b1:b12, sep = "") |> strtoi(base=2)

solution1 <- g*e

o2_gen_rating <- 0
co2_rating <- 0

df2 <- tibble::as_tibble(dat)
df2 <- df2 |> separate(V1, c("bin","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12"), "") |>
  mutate(bin = df2$V1) |>
  mutate(across(.cols = 2:13, .fns = as.double))

# ## solution to part 2
# n <- as.double(dim(df2)[1])
# for (i in 1:len) {
#   print(n)
#   if (n > 1) {
#     if (sum(df2[,i+1]) > (n/2.0)){
#       df2 <- df2 |> filter(select(df2,i+1) == 1)
#       n <- dim(df2)[1]
#     } else if (sum(df2[,i+1]) < (n/2.0)) {
#       df2 <- df2 |> filter(select(df2,i+1) == 0)
#       n <- dim(df2)[1]
#     } else if (all.equal(sum(df2[,i+1]), (n/2.0))) {
#       df2 <- df2 |> filter(select(df2,i+1) == 1)
#       n <- dim(df2)[1]
#     } else {print("error!")
#       print(n)
#       print(i)
#       print(sum(df2[,i+1]))
#       break
#     }
#   } else if (n==1){
#     # solution
#     print(n)
#     o2_gen_rating <- df2 |> select(1) |> strtoi(base=2)
# #    o2_gen_rating <- df2 |> unite("o2", 1:i+1, sep = "") |> select(o2) |> strtoi(base=2)
#     break
#   }
# }
# 
# df3 <- tibble::as_tibble(dat)
# df3 <- df3 |> separate(V1, c("bin","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12"), "") |>
#   mutate(bin = df3$V1) |>
#   mutate(across(.cols = 2:13, .fns = as.integer))
# up <- 0
# down<-0
# equal<-0
# 
# for (j in 1:len) {
#   m <- dim(df3)[1]
#   if (dim(df3)[1] > 1) {
#     if (sum(df3[,j+1]) < (m/2)){
#       df3 <- df3 |> filter(select(df3,j+1) == 1)
# #    df3 <- df3 |> filter(select(df3,i+1) == as.numeric(lcb[1,i]))
#       up <- up+1
#       print(dim(df3)[1])
#       print("up")
#       print(up)
#       print(j)
#       print(m)
#     } else if(sum(df3[,j+1]) > (m/2)){
#       df3 <- df3 |> filter(select(df3,j+1) == 0)
#       down <- down + 1
#       print(dim(df3)[1])
#       print("down")
#       print(down)
#       print(j)
#       print(m)
#     } else if(sum(df3[,j+1]) == (m/2)) {
#       df3 <- df3 |> filter(select(df3,j+1) == 0)
#       equal <- equal + 1
#       print(m)
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
