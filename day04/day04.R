## day 3 advent of code in R
library("tidyverse")
library("dplyr")
library("data.table")
library("gmodels")
library("stringr")
library("devtools")
library("R.utils")
library("binaryLogic")
library("reader")

input_loc <- "./day04/day04input.txt"
#input_loc <- "./day04/day04example.txt"

bingo_numbers<- as_tibble(read.table(file = input_loc, header = F, nrows = 1, sep = ","))
                                 

temp <- data.table::fread(input_loc, skip = 2, fill=FALSE)
card_size_x <- dim(temp)[2]
card_size_y <- dim(temp)[1]

dat <- data.table::fread(input_loc, skip = 2, fill=TRUE)
bingo_cards <- as_tibble(dat) |> drop_na()
input_size_x <- dim(bingo_cards)[2]
input_size_y <- dim(bingo_cards)[1]
num_of_cards <- input_size_y / card_size_y

bingo_answers <- data.frame(matrix(ncol = input_size_x, nrow = input_size_y))
#solution <- tibble(a = FALSE) ## think about adding more dimensions,track which card won


update_answers <- function (x, cards, answers) {
  matches <- which(cards[] == x, arr.ind = TRUE)
  for (i in 1:dim(matches)[1]) {
    answers[matches[i,1], matches[i,2]] <- x
    cards[matches[i,1], matches[i,2]] <- NA
  }
  answers
}

update_cards <- function (x, cards, answers) {
  matches <- which(cards[] == x, arr.ind = TRUE)
  for (i in 1:dim(matches)[1]) {
    cards[matches[i,1], matches[i,2]] <- NA
  }
  cards
}



## maybe I should create a function that first looks at an individual
## bingo card, checks horizontal and vertical NAs
## next loop through the entire input, feeding individual bingo cards into 
## the function
check_for_bingo <- function (bingo_card, size_x, size_y) {
  bingo_cnt_y <- 0
  bingo_cnt_x <- 0
  ## first check all rows, count NAs for each row
  for (x in 1:size_x) {
    bingo_cnt_x <- bingo_cnt_x + if_else(sum(is.na(bingo_card[1:size_y,x])) == 0, 1, 0)
  }
  for (y in 1:size_y) {
    bingo_cnt_y <- bingo_cnt_y + if_else(sum(is.na(bingo_card[y,])) == 0, 1, 0)
  }
  bingo_cnt_x + bingo_cnt_y
}

check_for_all_bingos <- function (num_of_cards, bingo_answers, card_size_x, card_size_y) {
  bingo_winners <- tibble(bingo_cnt = integer(), card_number = integer())
  ## go through full deck of cards, count bingos on each card and 
  ## track card # for future reference
  for (i in 1:num_of_cards){
    n <- i - 1
    answer_card <- bingo_answers[(1:card_size_y)+(card_size_y * n),]
    bingo_winners <- bingo_winners |>
      add_row(bingo_cnt = check_for_bingo(answer_card,card_size_x,card_size_y), card_number = i)
    #print(bingo_answers)
  }
  bingo_winners
}

score_bingo <- function (winning_card, card_size_x, winning_number){
  score <- 0
  for (i in 1:card_size_x){
    score <- score + sum(winning_card |> filter(!is.na(winning_card[,i])) |> select(i))
  }
  score <- score * winning_number
  score
}

bingo <- function(bingo_numbers, num_of_cards, bingo_cards, card_size_x, card_size_y, bingo_answers) {
  ## go through each bingo number, check against bingo cards
  winning_number <- NA
  winning_card <- tibble(.rows = card_size_y, .cols = card_size_x, fill=NA)
  solution <- NA
  for (k in 1:dim(bingo_numbers)[2]){
    previous_number <- current_number
    current_number <- as.integer(bingo_numbers[k])
    ## check for bingo winners - add function here
    #print(current_number)
    current_bingos <- check_for_all_bingos(num_of_cards, bingo_answers, card_size_x, card_size_y)
    if (sum(current_bingos[,1]) > 0) {  ## BINGO!
      print("BINGO")
      ## need to keep track of the current number
      winning_number <- previous_number
      print(current_bingos)
      print(winning_number)
      print(bingo_cards)
      n <- as.integer(current_bingos |> filter(bingo_cnt == 1) |> select(card_number)) - 1
      winning_card <- bingo_cards[(1:card_size_y)+(card_size_y * n),]
      solution <- score_bingo(winning_card, card_size_x, winning_number)
      break
    } else {
      ## assuming no winner exists, check for matches - add function here
      bingo_answers <- update_answers(current_number, bingo_cards, bingo_answers)
      bingo_cards <- update_cards(current_number, bingo_cards, bingo_answers)
    }
  }
  solution
}

answer <- bingo(bingo_numbers, num_of_cards, bingo_cards, card_size_x, card_size_y, bingo_answers)

