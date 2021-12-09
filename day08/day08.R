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

input_loc <- "./day08/day08input.txt"
#input_loc <- "./day08/day08example.txt"
#input_loc <- "./day08/day08simpleexample.txt"

dat <- data.table::fread(input_loc, colClasses = "character", sep2 = " | ", header = FALSE)
df <- tibble::as_tibble(dat) |> rename (signal = V1, out = V2)

sep_signal <- function(df) {
  # df <- df |> select(signal) |> separate(signal, into = c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "sig9", "sig10"),
  #                                        sep =" ", extra = 'drop', remove = FALSE)
  df <- df |> separate(signal, into = c("sig1", "sig2", "sig3", "sig4", "sig5", "sig6", "sig7", "sig8", "sig9", "sig10"),
                       sep =" ", extra = 'drop', remove = FALSE)
  df
}

sep_output <- function(df) {
#  df <- df |> select(out) |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df <- df |> separate(out, into = c("out1", "out2", "out3", "out4"), sep = " ", extra = 'drop', remove = FALSE)
  df
}

cnt_output <- function(df) {
  df <- df |> mutate(cnto1 = str_length(out1)) |>
              mutate(cnto2 = str_length(out2)) |>
              mutate(cnto3 = str_length(out3)) |>
              mutate(cnto4 = str_length(out4))
}

cnt_signal <- function(df) {
  df <- df |> mutate(cnts1 = str_length(sig1)) |>
              mutate(cnts2 = str_length(sig2)) |>
              mutate(cnts3 = str_length(sig3)) |>
              mutate(cnts4 = str_length(sig4)) |>
              mutate(cnts5 = str_length(sig5)) |>
              mutate(cnts6 = str_length(sig6)) |>
              mutate(cnts7 = str_length(sig7)) |>
              mutate(cnts8 = str_length(sig8)) |>
              mutate(cnts9 = str_length(sig9)) |>
              mutate(cnts10 = str_length(sig10))
}

solution1 <- function(df) {
  df <- df |> select(cnto1:cnto4)
  ones <- sum(df$cnto1 == 2) + sum(df$cnto2 == 2) + sum(df$cnto3 == 2) + sum(df$cnto4 == 2)
  fours <- sum(df$cnto1 == 4) + sum(df$cnto2 == 4) + sum(df$cnto3 == 4) + sum(df$cnto4 == 4)
  sevens <- sum(df$cnto1 == 3) + sum(df$cnto2 == 3) + sum(df$cnto3 == 3) + sum(df$cnto4 == 3)
  eights <- sum(df$cnto1 == 7) + sum(df$cnto2 == 7) + sum(df$cnto3 == 7) + sum(df$cnto4 == 7)
  ones + fours + sevens + eights
}

#df1 <- df |> sep_output() |> cnt_output()
#solution1 <- df1 |> solution1()


decode_entry <- function (entry) {
  entry <- entry |> select(2:11) |> add_row() |> add_row() |> add_row() |> add_row() |> add_row() |> add_row() |> add_row() 
  d1 <- ""
  d2 <- ""
  d3 <- ""
  d4 <- ""
  d5 <- ""
  d6 <- ""
  d7 <- ""
  one <- vector(mode = "character", length = 2)
  two <- vector(mode = "character", length = 5)
  three <- vector(mode = "character", length = 5)
  four <- vector(mode = "character", length = 4)
  five <- vector(mode = "character", length = 5)
  six <- vector(mode = "character", length = 6)
  seven <- vector(mode = "character", length = 3)
  eight <- vector(mode = "character", length = 7)
  nine <- vector(mode = "character", length = 5)
  zero <- vector(mode = "character", length = 6)
  
  ## find #1, 4, 7, 8
  for (i in 1:10) {
    entry[2,i] <- case_when(
      str_length(entry[1,i]) == 2 ~ "One",
      str_length(entry[1,i]) == 4 ~ "Four",
      str_length(entry[1,i]) == 3 ~ "Seven",
      str_length(entry[1,i]) == 7 ~ "Eight"
    )
    if (str_length(entry[1,i]) == 2) {one <- t(as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal"))
    } else if (str_length(entry[1,i]) == 4) {four <- t(as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal"))
    } else if (str_length(entry[1,i]) == 3) {seven <- t(as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal"))
    } else if (str_length(entry[1,i]) == 7) {eight <- t(as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal"))
    }
  }
  
  ## solve for d1
  entry[3,] <- t(gsub(one[1], "", entry[1,]))
  entry[4,] <- t(gsub(one[2], "", entry[3,]))
  for (j in 1:10) {
    if (str_length(entry[4,j]) == 1) {d1 <- entry[4,j]}
  }
  
  ## Find #3
  for (k in 1:10) {
    if ((str_length(entry[1,k]) == 5) & (grepl(one[1], entry[1,k])) & (grepl(one[2], entry[1,k]))) {
      three <- t((as_tibble(str_split(entry[1,k], ""), .name_repair = "minimal")))
      entry[2,k] <- "Three"
    }
  }
  
  ## find #0, 6, 9
  for (k in 1:10) {
    if ((str_length(entry[1,k]) == 6) & is.na(entry[2,k])) {
      if (grepl(four[1], entry[1,k]) & grepl(four[2], entry[1,k]) & grepl(four[3], entry[1,k]) & grepl(four[4], entry[1,k])){
        nine <- t((as_tibble(str_split(entry[1,k], ""), .name_repair = "minimal")))
        entry[2,k] <- "Nine"
      } else if (grepl(one[1], entry[1,k]) & grepl(one[2], entry[1,k])) {
        zero <- t((as_tibble(str_split(entry[1,k], ""), .name_repair = "minimal")))
        entry[2,k] <- "Zero"
      } else {
        six <- t((as_tibble(str_split(entry[1,k], ""), .name_repair = "minimal")))
        entry[2,k] <- "Six"
      }
    }
  }
  
  ## solve for d5
  entry[3,] <- t(gsub(nine[1], "", entry[1,]))
  entry[4,] <- t(gsub(nine[2], "", entry[3,]))
  entry[5,] <- t(gsub(nine[3], "", entry[4,]))
  entry[6,] <- t(gsub(nine[4], "", entry[5,]))
  entry[7,] <- t(gsub(nine[5], "", entry[6,]))
  entry[8,] <- t(gsub(nine[6], "", entry[7,]))
  for (l in 1:10) {
    if (is.na(entry[2,l])) {
      d5 <- d5
    } else if (entry[2,l] == "Eight") { 
      d5 <- entry[8,l] 
    } else { 
      d5 <- d5
    }
  }
  
  ## solve for 2 and 5
  for (i in 1:10) { 
    if (is.na(entry[2,i]) & (str_length(entry[1,i] == 5))) {
      if (entry[8,i] == "") {
        five <- t((as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal")))
        entry[2,i] <- "Five"
      } else {
        two <- t((as_tibble(str_split(entry[1,i], ""), .name_repair = "minimal")))
        entry[2,i] <- "Two"
      }
    }
  }
 
  ## solve for d7
  entry[3,] <- t(gsub(four[1], "", entry[1,]))
  entry[4,] <- t(gsub(four[2], "", entry[3,]))
  entry[5,] <- t(gsub(four[3], "", entry[4,]))
  entry[6,] <- t(gsub(four[4], "", entry[5,]))
  entry[7,] <- NA
  entry[8,] <- NA
  for (i in 1:10) {
    if (entry[2,i] == "Nine") {
      d7 <- gsub(d1, "", entry[6,i])
    } else {
      d7 <- d7
    }
  }
  
  ## solve for d2
  entry[3,] <- t(gsub(three[1], "", entry[1,]))
  entry[4,] <- t(gsub(three[2], "", entry[3,]))
  entry[5,] <- t(gsub(three[3], "", entry[4,]))
  entry[6,] <- t(gsub(three[4], "", entry[5,]))
  entry[7,] <- t(gsub(three[5], "", entry[6,]))
  entry[8,] <- NA
  for (i in 1:10) {
    if (entry[2,i] == "Five") {
      d2 <- entry[7,i]
    } else {
      d2 <- d2
    }
  }
  
  ## solve for d4
  entry[3,] <- t(gsub(zero[1], "", entry[1,]))
  entry[4,] <- t(gsub(zero[2], "", entry[3,]))
  entry[5,] <- t(gsub(zero[3], "", entry[4,]))
  entry[6,] <- t(gsub(zero[4], "", entry[5,]))
  entry[7,] <- t(gsub(zero[5], "", entry[6,]))
  entry[8,] <- t(gsub(zero[6], "", entry[7,]))
  for (i in 1:10) {
    if (entry[2,i] == "Eight") {
      d4 <- entry[8,i]
    } else {
      d4 <- d4
    }
  }
  
  ## solve for d6
  entry[3,] <- t(gsub(two[1], "", entry[1,]))
  entry[4,] <- t(gsub(two[2], "", entry[3,]))
  entry[5,] <- t(gsub(two[3], "", entry[4,]))
  entry[6,] <- t(gsub(two[4], "", entry[5,]))
  entry[7,] <- t(gsub(two[5], "", entry[6,]))
  entry[8,] <- NA
  for (i in 1:10) {
    if (entry[2,i] == "One") {
      d6 <- entry[7,i]
    } else {
      d6 <- d6
    }
  }
  
  
  ## solve for d3
  for (i in 1:10) {
    if (entry[2,i] == "One") {
      d3 <- gsub(d6, "", entry[1,i])
    } else {
      d3 <- d3
    }
  }
  
  ## output final code
  code <- as.matrix.data.frame(tibble(d1 = d1, d2 = d2, d3 = d3, d4 = d4, d5 = d5, d6 = d6, d7 = d7))
  code
}


solve_output <- function(entry, code) {

  output <- entry |> select(13:16) |> add_row() #|> add_row() |> add_row() |> add_row() |> add_row() |> add_row() |> add_row() 
  code <- as.tibble(code)
  
  for (i in 1:4) {
    if (str_length(output[1,i]) == 2) {
      output[2,i] <- "1"
    } else if (str_length(output[1,i]) == 3) {
      output[2,i] <- "7"
    } else if (str_length(output[1,i]) == 7) {
      output[2,i] <- "8"
    } else if (str_length(output[1,i]) == 4) {
      output[2,i] <- "4"
    } else if (str_length(output[1,i]) == 5) {
      # if (str_length(gsub(code$d5, "", output[1,i])) == 4) {
      #   output[2,i] <- "3"
      #} else 
      if (str_length(gsub(code$d3, "", output[1,i])) == 5) {
        output[2,i] <- "5"
      } else if (str_length(gsub(code$d6, "", output[1,i])) == 5) {
        output[2,i] <- "2"
      } else {
        output[2,i] <- "3"
      }
    } else if (str_length(output[1,i]) == 6) {
      if (str_length(gsub(code$d3, "", output[1,i])) == 6) {
        output[2,i] <- "6"
      } else if (str_length(gsub(code$d4, "", output[1,i])) == 6){
        output[2,i] <- "0"
      } else if (str_length(gsub(code$d5, "", output[1,i])) == 6){
        output[2,i] <- "9"
      } else {
        output[2,i] <- output[2,i]
      }
    } else {
      output[2,i] <= output[2,i]
    }
  }
  output <- output |> filter(row_number() == 2)# |> mutate(across(out1:out4, .fns = as.integer))
  output
}

# df2  <- df |> sep_signal() |> sep_output()
# code <- df2 |> decode_entry()
# out_solutions <- df2 |> sep_output() |> solve_output(code)
# out_solutions


solution2 <- function(df) {
  y <- dim(df)[1]
  df <- df |> sep_signal() |> sep_output()
  display_out <- matrix(nrow = y, ncol = 4) |> as_tibble(.name_repair = "minimal")
  colnames(display_out) <- (c("out1","out2","out3","out4"))
  display_out <- display_out |> mutate(across(.cols = everything() ,.fns = as.character))
  codebook <- matrix(nrow = 0, ncol = 7) |> as_tibble(.name_repair = "minimal")
  colnames(codebook) <- (c("d1","d2","d3","d4","d5","d6","d7"))
  codebook <- codebook |> mutate(across(.cols = everything(), .fns = as.character))

  for (i in 1:y) {
    code <- decode_entry(df[i,])
    #codebook[i,] <- df |> filter(row_number() == i) |> decode_entry()
    codebook[i,] <- code
    display_out[i,] <- df |> filter(row_number() == i) |> solve_output(codebook[i,])
  }
  
  display_out <- display_out |> unite("out", out1:out4, sep = "")
  display_out <- display_out |> mutate(across(.cols = everything(), .fns = as.integer))
  
#  solution <- display_out
  solution <- sum(display_out)
  solution
}


answer2 <- df |> solution2()
answer2


# 
# 
# 
# dft <- df[8,] |>  sep_signal() |> sep_output()
# code <- dft |> decode_entry()
# entry <- dft
