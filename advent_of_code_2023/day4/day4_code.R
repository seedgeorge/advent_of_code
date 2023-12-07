setwd("~/Documents/AoC/advent_of_code_2023/day4")
library(stringr)
library(readr)
input = day4_test <- read_table("day4_input.txt", col_names = FALSE)
input = day4_test <- read_table("day4_test.txt", col_names = FALSE)

# parse it
barpos = which(colSums(input == "|") > 0)
wins = input[,3:(barpos-1)]
nums = input[,(barpos+1):ncol(input)]

# part1
scorecard = data.frame(card = 1:nrow(input),winning = NA,score = NA)

for (card in 1:nrow(input)) {
  winv = as.vector(unlist(wins[card,]))
  numv = as.vector(unlist(nums[card,]))
  winning = sum(winv %in% numv)
  score = 0
  if(winning > 2) {
    score = 2^(winning - 1)
  }
  if (winning == 2) {
    score = 2
  }
  if (winning == 1) {
    score = 1
  }
  scorecard$score[card]  = score
  scorecard$winning[card]  = winning
  
}
sum(scorecard$score)

#part2
input
scorecard = data.frame(card = 1:nrow(input),winning = NA,score = NA)
scorecard$counted = 1
for (card in 1:nrow(input)) {
  winv = as.vector(unlist(wins[card,]))
  numv = as.vector(unlist(nums[card,]))
  winning = sum(winv %in% numv)
  score = 0
  if(winning > 2) {
    score = 2^(winning - 1)
  }
  if (winning == 2) {
    score = 2
  }
  if (winning == 1) {
    score = 1
  }
  scorecard$score[card] = score
  scorecard$winning[card] = winning
  if(winning > 0) {
  extras = rep((card+1):(card + winning), scorecard$counted[scorecard$card == card])
  for (c in extras) {
    scorecard[c, "counted"] = scorecard[c, "counted"] + 1
    }
  }
}
# this is slow as shit but uhh... technically seems to work?
# also this is super verbose but I got work to do
