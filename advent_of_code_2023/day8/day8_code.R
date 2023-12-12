setwd("~/Documents/AoC/advent_of_code_2023/day8")
library(stringr)
library(readr)
input = readLines("day8_input.txt")
# input = readLines("day8_test.txt")
#input = readLines("day8_test_2.txt")

commands = input[1]
commands = unlist(strsplit(commands,split = ""))

# make the map
map = input[-2:-1]
map = str_split_fixed(map,pattern = " |=|\\(|\\,|\\)",n = 8)
map = map[,c(1,5,7)]
rownames(map) = map[,1]
map = map[,-1]
map = data.frame(map)
colnames(map) = c("L","R")

## part1

i = 1
moves = 0
command_current = "AAA"
while(is.numeric(i)) {
  command = commands[i]
  next_pos = map[command_current,command]
  #command_count = which(rownames(map) == next_pos)
  command_current = next_pos
  #cat (i)
  if(i == length(commands)) {
    i = 1
  } else {
    i = i+1
  }
  # put a counter for moves
  moves = moves +1
  # break if the next_pos is ZZZ
  if (next_pos == "ZZZ") {
    break
  }
}

cat(moves)

## part2 
#new test
#input = readLines("day8_test_3.txt")
input = readLines("day8_input.txt")


commands = input[1]
commands = unlist(strsplit(commands,split = ""))

# make the map
map = input[-2:-1]
map = str_split_fixed(map,pattern = " |=|\\(|\\,|\\)",n = 8)
map = map[,c(1,5,7)]
rownames(map) = map[,1]
map = map[,-1]
map = data.frame(map)
colnames(map) = c("L","R")


starts = rownames(map)[sapply(rownames(map),FUN = function(x) unlist(strsplit(x,split = ""))[3]) == "A"] 
for (start in starts) {

i = 1
moves = 0
command_current = start
while(is.numeric(i)) {
  command = commands[i]
  next_pos = map[command_current,command]
  #command_count = which(rownames(map) == next_pos)
  command_current = next_pos
  #cat (i)
  if(i == length(commands)) {
    i = 1
  } else {
    i = i+1
  }
  # put a counter for moves
  moves = moves +1
  # break if the next_pos is ZZZ
  lastchar_nextpos = unlist(strsplit(next_pos,split = ""))[3]
  if (lastchar_nextpos == "Z") {
    break
  }
}

cat(start,"\t",moves,"\n")
}
# they printed out fine
vals = c(20569,18727,14429,13201,18113,22411)

lcm_vector <- function(x) Reduce(pracma::Lcm, x) # use Lcm, this is some maths shit
lcm_vector(vals)
