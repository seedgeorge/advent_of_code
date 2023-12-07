setwd("~/Documents/AoC/advent_of_code_2023/day3/")

library(stringr)

# solution 1
#input = readLines("day3_test.txt")
input = readLines("day3_input.txt")
input_df = str_split(string = input,pattern = "",simplify = T)
boolean_df = matrix(FALSE,ncol = ncol(input_df),nrow = nrow(input_df))
symbols = c("-" ,"@", "*", "/", "&", "#", "%", "+", "=", "$")

# some utility functions
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
conv_to_coord = function(pos,row,col) {
  newrow = NA
  newcol = NA
  if(pos == "n") {
    newrow = row-1
    newcol = col
  }
  if(pos == "ne") {
    newrow = row-1
    newcol = col+1
  }
  if(pos == "e") {
    newrow = row
    newcol = col+1
  }
  if(pos == "se") {
    newrow = row+1
    newcol = col+1
  }
  if(pos == "s") {
    newrow = row+1
    newcol = col
  }
  if(pos == "sw") {
    newrow = row+1
    newcol = col-1
  }
  if(pos == "w") {
    newrow = row
    newcol = col-1
  }
  if(pos == "nw") {
    newrow = row-1
    newcol = col-1
  }
  return(c(newrow,newcol))
}

# loop over rows and columns
for (row in 1:nrow(input_df)) {
  for (col in 1:ncol(input_df)) {
    character = input_df[row,col] 
    # check the character
    if (character %in% symbols) {
      cat (character,"-",row,"-",col,"\n")
      # find any adjacent numbers
      adjacent_hits = checksymbol(row,col)
      adjacent_hits = which(adjacent_hits == TRUE)
      # mark them in the boolean df 
      for (hit in 1:length(adjacent_hits)) {
        pos = names(adjacent_hits)[hit]
        coords = conv_to_coord(pos,row,col)
        boolean_df[coords[1], coords[2]] = TRUE
      }
    }
  }
}

# now based on hits from the boolean df
# expand left and right if the value can be numeric
# to 'catch' multi-digit numbers
boolean_df_expanded = boolean_df
boolean_melted = reshape2::melt(boolean_df) %>% subset(value == TRUE) 
boolean_melted = boolean_melted[order(boolean_melted$Var1,boolean_melted$Var2),]
#row = 1
#col = 3
boolean_df_expanded = boolean_df

for(hit in 1:nrow(boolean_melted)) {
  row = boolean_melted[hit,"Var1"]
  col = boolean_melted[hit,"Var2"]
  
  #expand right
  startcol = col
  startrow = row
  val = boolean_df[startrow,startcol]
  while(val == TRUE) {
    startcol = startcol+1
    if(startcol == 0 | startcol == ncol(boolean_df)+1) {
      break }
    numval = input_df[startrow,startcol]
    if (suppressWarnings(!is.na(as.numeric(numval)))) {
      boolean_df_expanded[startrow,startcol] = TRUE
    } else { val = FALSE }
  }
  
  # expand left
  startcol = col
  startrow = row
  val = boolean_df[startrow,startcol]
  while(val == TRUE) {
    startcol = startcol-1
    if(startcol == 0 | startcol == ncol(boolean_df)+1) {
      break }
    numval = input_df[startrow,startcol]
    if (suppressWarnings(!is.na(as.numeric(numval)))) {
      boolean_df_expanded[startrow,startcol] = TRUE
    } else { val = FALSE }
  }
}

just_number_df = suppressWarnings(matrix(as.numeric(input_df),byrow = F,ncol=ncol(input_df)))

drop_df = replace(just_number_df, !boolean_df_expanded, ".")
keep_numbers = apply(drop_df,1,paste,collapse="")
sum(as.numeric(unlist(str_split(paste(keep_numbers,collapse = "."),pattern = "\\."))),na.rm=T)


## part 2 
# just keep the gears
input = readLines("day3_input.txt")
#input = readLines("day3_input.txt")
input_df = str_split(string = input,pattern = "",simplify = T)
boolean_df = matrix(FALSE,ncol = ncol(input_df),nrow = nrow(input_df))
newsymbols = "*"
boolean_gear_df  = matrix(FALSE, ncol = ncol(input_df),nrow = nrow(input_df))

# some utility functions
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

conv_to_coord = function(pos,row,col) {
  newrow = NA
  newcol = NA
  if(pos == "n") {
    newrow = row-1
    newcol = col
  }
  if(pos == "ne") {
    newrow = row-1
    newcol = col+1
  }
  if(pos == "e") {
    newrow = row
    newcol = col+1
  }
  if(pos == "se") {
    newrow = row+1
    newcol = col+1
  }
  if(pos == "s") {
    newrow = row+1
    newcol = col
  }
  if(pos == "sw") {
    newrow = row+1
    newcol = col-1
  }
  if(pos == "w") {
    newrow = row
    newcol = col-1
  }
  if(pos == "nw") {
    newrow = row-1
    newcol = col-1
  }
  return(c(newrow,newcol))
}

# loop over rows and columns
gearvals = c()
for (row in 1:nrow(input_df)) {
  for (col in 1:ncol(input_df)) {
    character = input_df[row,col] 
    # check the character
    if (character %in% newsymbols) {
      cat (character,"-",row,"-",col,"\n")
      # find any adjacent numbers
      adjacent_hits = checksymbol(row,col)
      adjacent_hits = which(adjacent_hits == TRUE)
      if (sum(names(adjacent_hits) %in% c("s","sw") ) == 2) {
        adjacent_hits = adjacent_hits[!names(adjacent_hits) %in% "sw"]
      }
      if (sum(names(adjacent_hits) %in% c("s","se") ) == 2) {
        adjacent_hits = adjacent_hits[!names(adjacent_hits) %in% "se"]
      }
      if (sum(names(adjacent_hits) %in% c("n","ne") ) == 2) {
        adjacent_hits = adjacent_hits[!names(adjacent_hits) %in% "ne"]
      }
      if (sum(names(adjacent_hits) %in% c("n","nw") ) == 2) {
        adjacent_hits = adjacent_hits[!names(adjacent_hits) %in% "nw"]
      }
      # mark them in the boolean df 
      if (length(adjacent_hits) == 2) {
        hit_numbers = c(NA,NA)
        for (hit in 1:length(adjacent_hits)) {
          pos = names(adjacent_hits)[hit]
          coords = conv_to_coord(pos,row,col)
          
          # get numbers right
          startcol = coords[2]
          startrow = coords[1]
          val = input_df[startrow,startcol]
          valnumeric = suppressWarnings(!is.na(as.numeric(val)))
          
          while(valnumeric == TRUE) {
            startcol = startcol+1
            if(startcol == 0 | startcol == ncol(boolean_df)+1 ) {
              break }
            numval = input_df[startrow,startcol]
            if (suppressWarnings(!is.na(as.numeric(numval)))) {
              val = paste (val,numval,sep="")
            } else { 
              valnumeric = FALSE }
          }
          
          # get numbers left
          startcol = coords[2]
          startrow = coords[1]
          # val = input_df[startrow,startcol]
          valnumeric = suppressWarnings(!is.na(as.numeric(val)))
          
          while(valnumeric == TRUE) {
            startcol = startcol-1
            if(startcol == 0 | startcol == ncol(boolean_df)+1 ) {
              break }
            numval = input_df[startrow,startcol]
            if (suppressWarnings(!is.na(as.numeric(numval)))) {
              val = paste (numval, val, sep="")
            } else { 
              valnumeric = FALSE }
          }
          # save value
          hit_numbers[hit] = val
        }
        gearvals = c(gearvals,prod(as.numeric(hit_numbers)) )
      }
    }
  }
}
sum(gearvals)

