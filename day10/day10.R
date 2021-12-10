## day 10 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day10/day10input.txt"
#input_loc <- "./day10/day10example.txt"

dat <- data.table::fread(input_loc, header = FALSE, na.strings = c("", NA))
dat <- str_split_fixed(dat$V1, "", n = Inf)

df <- tibble::as_tibble(dat)
df <- df |> mutate(across(.cols = everything(), .fns = ~na_if(.,"")))


entry_status <- function (entry) {
  illegal_char <- NA
  status <- NA
  buffer <- ""
  x <- 1
  for (i in 1:dim(entry)[2]) {
    if (is.na(entry[1,i])) {
      status <- "Incomplete"
      break
    } else if (!is.na(status)) {
      break
    } else {
      if (entry[1,i] == "[" | entry[1,i] == "(" | entry[1,i] == "<" | entry[1,i] == "{") {
        buffer[x] <- entry[1,i]
        x <- x + 1
      } else if (entry[1,i] == "]" | entry[1,i] == ")" | entry[1,i] == ">" | entry[1,i] == "}") {
        if ((entry[1,i] == ")" & buffer[x-1] == "(") | (entry[1,i] == "]" & buffer[x-1] == "[") |
            (entry[1,i] == "}" & buffer[x-1] == "{") | (entry[1,i] == ">" & buffer[x-1] == "<")) {
          buffer <- buffer[1:(length(buffer)-1)]
          x <- x - 1
        } else {
          status <- "Corrupt"
          illegal_char <- entry[1,i]
        }
      }
    }
  }
  if (is.na(status)) {
    if (length(buffer) > 1) {
      status <- "Incomplete"
    } else {
      status <- "Complete"
    }
  }
  status <- tibble(status = status, illegal_char = illegal_char)
  status
}


update_status <- function(df) {
  l <- dim(df)[2]
  df <- df |> mutate(status = "", illegal_char = "")
  for (i in 1:dim(df)[1]) {
    df[i,l+1] <- as.character(entry_status(df[i,1:l])[1])
    df[i,l+2] <- as.character(entry_status(df[i,1:l])[[2]])
  }
  df
}


# ): 3 points.
# ]: 57 points.
# }: 1197 points.
# >: 25137 points.
score <- function(c) {
#  if (c == ")") 
  score <- switch(c, 
                  ")"= 3,
                  "]"= 57,
                  "}"= 1197,
                  ">"= 25137)
  score
}

calculate_corrupt_scores <- function(df) {
  df <- df |> filter(status == "Corrupt") |> mutate(score = NA)
  for (i in 1:dim(df)[1]) {
    df[i,dim(df)[2]] <- score(as.character(df[i,dim(df)[2]-1]))
  }
  scores <- sum(df$score)
}

solution1 <- df |> update_status() |> calculate_corrupt_scores()
solution1





