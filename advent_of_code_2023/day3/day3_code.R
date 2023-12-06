setwd("~/Documents/AoC/advent_of_code_2023/day3/")

library(stringr)

# solution 1
input = readLines("day3_test.txt")
input_df = str_split(string = input,pattern = "",simplify = T)
symbols = c("*","|","/","#","+","%","@","$","=","-","_")
# try a big dumb loop
for (row in 1:nrow(input_df)) {
  #check_row = input_df[row,]
  for (col in 1:ncol(input_df)) {
    #check_col =  input_df[row,]
    character = input_df[row,col]
    if (character %in% symbols) {
      cat (character,"-",row,"-",col,"\n")
      # find any adjacent numbers
      input_df[]
      
    }
  }
}

row = 2
col = 4

checksymbol = function(row,col) { 
  input_df[row,col]
  n = suppressWarnings(!is.na(as.numeric(input_df[row-1,col])))
  ne = suppressWarnings(!is.na(as.numeric(input_df[row-1,col+1])))
  e = suppressWarnings(!is.na(as.numeric(input_df[row,col+1])))
  se = suppressWarnings(!is.na(as.numeric(input_df[row+1,col+1])))
  s = suppressWarnings(!is.na(as.numeric(input_df[row+1,col])))
  sw = suppressWarnings(!is.na(as.numeric(input_df[row+1,col-1])))
  w = suppressWarnings(!is.na(as.numeric(input_df[row,col-1])))
  nw = suppressWarnings(!is.na(as.numeric(input_df[row-1,col-1])))
  adjacent_numbers = c("n"=n,"ne"=ne,"e"=e,"se"=se,"s"=s,"sw"=sw,"w"=w,"nw"=nw)
  return(adjacent_numbers)
}

adjacent_hits = checksymbol(row,col)
adjacent_hits = which(adjacent_hits == TRUE)
adjacent_hits

conv_to_coord = function(pos,row,col) {
  if()
}

# 
# #
# df_melted = reshape2::melt(input_df)
# df_melted = df_melted[df_melted$value != ".",]
# colnames(df_melted) = c("row","col","value")
# df_melted = df_melted[order(df_melted$row,df_melted$col),]
# for (df_melted$value %in% symbols) {
#   
# }
