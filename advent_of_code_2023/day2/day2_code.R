setwd("~/Documents/AoC/advent_of_code_2023/day2/")

# solution 1
#input = readLines("day2_test.txt")
input = readLines("day2_input.txt")

redlimit = 12
greenlimit = 13
bluelimit = 14

df = data.frame(string = input,game = NA)
df$game = as.numeric(gsub(x=str_split_fixed(df$string,pattern = ":",2)[,1],pattern = "Game ",replacement = ""))
gamestrings = gsub(x=input,pattern = "Game \\d+: ",replacement = "")

resultlist = str_split(gamestrings,pattern = ";|,")

getmaxscore = function(games,colour) {
  games = games[grep(x=games,pattern = colour)]
  games = gsub(x=games,pattern = colour,replacement = "")
  games = as.numeric(gsub(x=games,pattern = " ",replacement = ""))
  colmax = max(games)
  return(colmax)
}

df$redmax= unlist(lapply(resultlist,FUN=function(x) getmaxscore(x,colour = "red")))
df$bluemax = unlist(lapply(resultlist,FUN=function(x) getmaxscore(x,colour = "blue")))
df$greenmax = unlist(lapply(resultlist,FUN=function(x) getmaxscore(x,colour = "green")))

sum(df$game[df$redmax <= redlimit & df$greenmax <= greenlimit & df$bluemax <= bluelimit])


# solution 2
#input = readLines("day2_test.txt")
input = readLines("day2_input.txt")

# make a df
df = data.frame(string = input,game = NA)
df$game = as.numeric(gsub(x=str_split_fixed(df$string,pattern = ":",2)[,1],pattern = "Game ",replacement = ""))
gamestrings = gsub(x=input,pattern = "Game \\d+: ",replacement = "")

resultlist = str_split(gamestrings,pattern = ";|,")

getminscore = function(games,colour) {
  games = games[grep(x=games,pattern = colour)]
  games = gsub(x=games,pattern = colour,replacement = "")
  games = as.numeric(gsub(x=games,pattern = " ",replacement = ""))
  colmax = max(games)
  return(colmax)
}

df$redmin = unlist(lapply(resultlist,FUN=function(x) getminscore(x,colour = "red")))
df$bluemin = unlist(lapply(resultlist,FUN=function(x) getminscore(x,colour = "blue")))
df$greenmin = unlist(lapply(resultlist,FUN=function(x) getminscore(x,colour = "green")))

df$power = df$redmin*df$bluemin*df$greenmin
sum(df$power)


