setwd("~/Documents/AoC/advent_of_code_2023/day11")
library(stringr)
library(tidyr)
#input = readLines("day11_test.txt")
#input = readLines("day11_input.txt")
input

input_df = str_split(input,pattern = "",simplify = T)

# expand it
starmap = input_df
empty_rows = rowSums(starmap == ".") == ncol(starmap)
starmap = as.data.frame(starmap)

for (e in rev(which(empty_rows))) {
  starmap = starmap %>% dplyr::add_row(.before = e)
}
starmap = as.data.frame(t(starmap))
starmap[is.na(starmap)] = "."

empty_cols = rowSums(starmap == ".") == ncol(starmap)
starmap = as.data.frame(starmap)
for (e in rev(which(empty_cols))) {
  starmap = starmap %>% dplyr::add_row(.before = e)
}
starmap = as.data.frame(t(starmap))
starmap[is.na(starmap)] = "."


# melt to row column df
colnames(starmap) = paste(rep("col",ncol(starmap)),1:ncol(starmap),sep="")
rownames(starmap) = paste(rep("row",nrow(starmap)),1:nrow(starmap),sep="")
starmap = data.matrix(starmap)
starmap_vis = starmap
starmap_vis[starmap_vis == "1"] = "."

# melt and edit, calculate star IDs
starmap_melted = reshape2::melt(starmap,byrow=T)
starmap_melted = subset(starmap_melted,value == 2)
starmap_melted = starmap_melted[order(starmap_melted$Var1,starmap_melted$Var2),]
starmap_melted$ID = 1:nrow(starmap_melted)
#starmap_melted = starmap_melted[order(starmap_melted$Var1,starmap_melted$Var2),]
starmap_melted = starmap_melted[,c("Var1","Var2","ID")]
colnames(starmap_melted) = c("row","col","ID")
starmap_melted$row = as.numeric(gsub(x=starmap_melted$row ,pattern = "row",replacement = ""))
starmap_melted$col = as.numeric(gsub(x=starmap_melted$col ,pattern = "col",replacement = ""))

# pair data frame
star_pairs = as.data.frame(t(combn(starmap_melted$ID,2)))
for(pair in 1:nrow(star_pairs)) {
  from = star_pairs[pair,1]
  to = star_pairs[pair,2]
  # calc manhattan distance
  data  = starmap_melted[starmap_melted$ID %in% c(from,to),]
  dist = dist(data[,1:2],method = "man")
  star_pairs$dist[pair] = dist
}
sum(star_pairs$dist)


# part2
input = readLines("day11_test.txt")
#input = readLines("day11_input.txt")
input

input_df = str_split(input,pattern = "",simplify = T)
starmap = input_df

#don't expoand it

# melt to row column df
colnames(starmap) = paste(rep("col",ncol(starmap)),1:ncol(starmap),sep="")
rownames(starmap) = paste(rep("row",nrow(starmap)),1:nrow(starmap),sep="")
starmap = data.matrix(starmap)
starmap_vis = starmap
starmap_vis[starmap_vis == "1"] = "."

# melt and edit, calculate star IDs
starmap_melted = reshape2::melt(starmap,byrow=T)
starmap_melted = subset(starmap_melted,value == "#")
starmap_melted = starmap_melted[order(starmap_melted$Var1,starmap_melted$Var2),]
starmap_melted$ID = 1:nrow(starmap_melted)
#starmap_melted = starmap_melted[order(starmap_melted$Var1,starmap_melted$Var2),]
starmap_melted = starmap_melted[,c("Var1","Var2","ID")]
colnames(starmap_melted) = c("row","col","ID")
starmap_melted$row = as.numeric(gsub(x=starmap_melted$row ,pattern = "row",replacement = ""))
starmap_melted$col = as.numeric(gsub(x=starmap_melted$col ,pattern = "col",replacement = ""))

empty_rows = which(rowSums(starmap == ".") == ncol(starmap))
empty_cols = which(colSums(starmap == ".") == nrow(starmap))

expf = 1000000
# pair data frame
star_pairs = as.data.frame(t(combn(starmap_melted$ID,2)))
for(pair in 1:nrow(star_pairs)) {
  from = star_pairs[pair,1]
  to = star_pairs[pair,2]
  # calc manhattan distance
  data  = starmap_melted[starmap_melted$ID %in% c(from,to),]
  
  # 
  # data$row = data$row - (sapply(data$row,FUN = function(x) sum(empty_rows > x)) * expf)
  # data$col = data$col - (sapply(data$col,FUN = function(x) sum(empty_cols > x)) * expf)
  # 

  # add distance by exp factor
  start_rows = data$row[1] : data$row[2]
  far_row = which.max(data$row)
  newrow = data$row[far_row] - sum(start_rows %in% empty_rows)
  newrow = newrow + (sum(start_rows %in% empty_rows) * expf)
  data$row[far_row] = newrow
  
  # expand columns 
  start_cols = data$col[1] : data$col[2]
  far_col = which.max(data$col)
  newcol = data$col[far_col] - sum(start_cols %in% empty_cols)
  newcol = newcol + (sum(start_cols %in% empty_cols) * expf)
  data$col[far_col] = newcol
  
  dist = dist(data[,1:2], method = "man")
  
  star_pairs$dist[pair] = dist
}
sum(star_pairs$dist)

