setwd("~/Documents/AoC/advent_of_code_2023/day10")
library(stringr)
input = readLines("day10_test3.txt")
input = readLines("day10_input.txt")

input_df = str_split(string = input,pattern = "",simplify = T)
track_matrix = matrix(NA,ncol=ncol(input_df) , nrow = nrow(input_df)  )
mult = 3
track_matrix_big = matrix(NA,ncol=ncol(input_df) *mult, nrow = nrow(input_df) *mult)

#track_matrix = matrix(NA,ncol=ncol(input_df) * 3, nrow = nrow(input_df) * 3 )
track_matrix
input_df

# walk over the track
# need some pointers
calc_move = function (character,lastmove) {
  # leftright
  if (character == "-") {
    if (lastmove == "right") {
      return ("right")
    } else {
      return("left")
    }
  }
  # updown 
  if (character == "|") {
    if (lastmove == "down") {
      return ("down")
    } else {
      return("up")
    }
  }
  # J  
  if (character == "J") {
    if (lastmove == "right") {
      return ("up")
    } else {
      return("left")
    }
  }
  # L
  if (character == "L") {
    if (lastmove == "left") {
      return ("up")
    } else {
      return("right")
    }
  }
  # 7
  if (character == "7") {
    if (lastmove == "right") {
      return ("down")
    } else {
      return("left")
    }
  }
  # F
  if (character == "F") {
    if (lastmove == "left") {
      return ("down")
    } else {
      return("right")
    }
  }
}

startcol = which(colSums(input_df == "S") == 1)
startrow = which(rowSums(input_df == "S") == 1)
startpos = c(startrow,startcol)
currentlocation = startpos
start = FALSE
moves = 0
last_move = ""
biglocation = currentlocation * mult
while(start == FALSE) {
  # get where we are
  whatchar = input_df[currentlocation[1],currentlocation[2]]
  if (last_move != "") {
    next_move = calc_move(whatchar,last_move)
  } else {
    next_move ="down"
  }
  # adjust position
  if(next_move == "up") {
    currentlocation[1] = currentlocation[1] - 1 }
  if(next_move == "down") {
    currentlocation[1] = currentlocation[1] + 1 }
  if(next_move == "left") {
    currentlocation[2] = currentlocation[2] - 1 }
  if(next_move == "right") {
    currentlocation[2] = currentlocation[2] + 1 }
  # add to counter
  moves=moves + 1
  track_matrix[currentlocation[1],currentlocation[2]] = moves
  
  # adjust the biglocation appropriately
  nextbig = currentlocation * mult
  track_matrix_big[biglocation[1]:nextbig[1],biglocation[2]:nextbig[2]] = moves
  biglocation = nextbig
  
  # record the moves
  last_move = next_move
  nextchar = input_df[currentlocation[1],currentlocation[2]]
  
  
  if (nextchar == "S") { 
    start = TRUE}
}

## part 2
# we can use the track matrix we created
track_matrix_binary = !is.na(track_matrix_big)
#track_matrix_binary_numeric = apply(track_matrix_binary,2,as.numeric)
# bind extra empty rows
track_matrix_binary = cbind(FALSE,FALSE,track_matrix_binary,FALSE,FALSE)
track_matrix_binary = rbind(FALSE,FALSE,track_matrix_binary,FALSE,FALSE)
# make numeric for flood fill
track_matrix_binary_numeric = apply(track_matrix_binary,2,as.numeric)
track_matrix_binary_numeric[,c(1,ncol(track_matrix_binary_numeric))] = 1
track_matrix_binary_numeric[c(1,nrow(track_matrix_binary_numeric)),] = 1
pheatmap::pheatmap((track_matrix_binary_numeric),cluster_rows = F,cluster_cols = F)


M = track_matrix_binary_numeric
## ugh, floodfill it instead? steal this old code from someone
floodfill <- function(row, col, tcol, rcol) {
  if (tcol == rcol) return()
  if (M[row, col] != tcol) return()
  Q <- matrix(c(row, col), 1, 2)
  while (dim(Q)[1] > 0) {
    n <- Q[1, , drop = FALSE]
    west  <- cbind(n[1]    , n[2] - 1)
    east  <- cbind(n[1]    , n[2] + 1)
    north <- cbind(n[1] + 1, n[2]    )
    south <- cbind(n[1] - 1, n[2]    )
    Q <- Q[-1, , drop = FALSE]
    if (M[n] == tcol) {
      M[n] <<- rcol
      if (M[west] == tcol)  Q <- rbind(Q, west)
      if (M[east] == tcol)  Q <- rbind(Q, east)
      if (M[north] == tcol) Q <- rbind(Q, north)
      if (M[south] == tcol) Q <- rbind(Q, south)
    }
  }
  return("filling completed")
}

floodfill(row = 2, col = 2, 0, 2)
pheatmap::pheatmap((M),cluster_rows = F,cluster_cols = F)

# ok let us identify cells that are 'inside' BUT adjacent to a pipe
# these are pipe walls
checktile = function(row,col,matrix) {
  tile = matrix[c(row-1,row,row+1),c(col-1,col,col+1)]
  if (sum(as.vector(tile) == 1) != 0){
    return(3)  
  } else {
    return(0)
  }
}
for (r in 1:nrow(M)) {
  for (c in 1:ncol(M)) {
    if (M[r,c] == 0) {
      tileval = checktile(r,c,M)
      M[r,c] = tileval
    }
  }
}
pheatmap::pheatmap((M),cluster_rows = F,cluster_cols = F)
sum(M == 0)/9


