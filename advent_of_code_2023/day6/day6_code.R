setwd("~/Documents/AoC/advent_of_code_2023/day6")
library(readr)
library(stringr)
#input = readLines("day6_test.txt")
input = readLines("day6_input.txt")
input_df = str_split_fixed(input,pattern = "\\h+",n = 5)
input_df = data.frame(t(input_df))
colnames(input_df) = input_df[1,]
input_df = input_df[-1,]

# 
time = 7
dist = 9
calc_ways = function(time,dist) {
  time = as.numeric(time)
  dist = as.numeric(dist)
  # calculate the distances
  speedlist = 1:(time-1)
  remainder = time-speedlist
  distances = speedlist*remainder
  # which are better than the record 
  ways_to_win = sum(distances > dist)
  return(ways_to_win)
}
options = apply(input_df,MARGIN = 1,FUN = function(x) calc_ways(x[1],x[2]))
prod(options)


##

input = readLines("day6_input.txt")
#input = readLines("day6_test.txt")
input_df = str_split_fixed(input,pattern = ":",n = 2)
input_df = data.frame(t(input_df))
input_df = apply(input_df,2,FUN = function(x)gsub(pattern=" ",replacement = "",x = x))
colnames(input_df) = input_df[1,]
input_df = input_df[-1,]

# 
calc_ways = function(time,dist) {
  time = as.numeric(time)
  dist = as.numeric(dist)
  # calculate the distances
  speedlist = 1:(time-1)
  remainder = time-speedlist
  distances = speedlist*remainder
  # which are better than the record 
  ways_to_win = sum(distances > dist)
  return(ways_to_win)
}
calc_ways(time = input_df[1],dist = input_df[2])

