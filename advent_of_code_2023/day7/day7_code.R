setwd("~/Documents/AoC/advent_of_code_2023/day7")

cards = c("A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2")
classes = c("fivekind","fourkind","fullhouse","threekind","twopair","onepair","highcard")

# solution 1
input = read.delim("day7_input.txt",header = F,sep = " ")
colnames(input) = c("hand","bid")

# detect type
detect_type = function(hand) {
  handv = unlist(str_split(hand,pattern = ""))
  if (sum(table(handv) == 5) == 1) {
    return("fivekind")
  }
  if(sum(table(handv) == 4) == 1) {
    return("fourkind")
  }
  if(sum(table(handv) == 3) == 1 & sum(table(handv) == 2) == 1) {
    return("fullhouse")
  }
  if (sum(table(handv) == 3) == 1 & sum(table(handv) == 2) == 0 ) {
    return("threekind")
  }
  if(sum(table(handv) == 2) == 2){
    return("twopair")
  }
  if (sum(table(handv) == 2) == 1) {
    return("onepair")
  }
  if (length(unique(handv)) == 5 ) {
    return("highcard")
  }
}

# get types
input$types = sapply(input$hand,detect_type)

# process by group
input_l = split(input,f = input$types)
internal_order = function(i_sub) {
  f_matrix = as.data.frame(str_split_fixed(i_sub$hand,pattern = "",n = 5))
  f_matrix[,1:5] = lapply(f_matrix[,1:5],FUN = factor,levels=c(rev(cards)))
  i_sub = cbind(i_sub,f_matrix)
  i_sub = i_sub[order(i_sub$V1,i_sub$V2,i_sub$V3,i_sub$V4,i_sub$V5),]
  i_sub$internal_order = 1:nrow(i_sub)
  return(i_sub)
}
# compress to df
input_ordered = lapply(input_l,internal_order)
input_ordered = do.call(rbind,input_ordered)

# order it
input_ordered$types = factor(input_ordered$types,levels = rev(classes))
ordered = input_ordered[order(input_ordered$types,input_ordered$internal_order,decreasing = F),]

# return result 
ordered$rank = 1:nrow(ordered)
sum(ordered$rank * ordered$bid)


## part 2
#change the factor
cards = c("A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2","J")

# input is same as before
input = read.delim("day7_input.txt",header = F,sep = " ")
colnames(input) = c("hand","bid")

# detect type function remains the same 
# need a new function to offer the best IF they are higher rank

joker_alternative = function(hand,type) {
  handv = unlist(str_split(hand,pattern = ""))
  if (!grepl(x=hand,pattern = "J")) {
    return(type) # if no J present, just return original type
  } 
  if (type == "fivekind") {
    return(type) # no improvement possible from fivekind
  }
  handt = table(handv[handv != "J"]) # get the frequency table
  if(type == "highcard") { # what if they're all different, but there's a J anyway? make it the next best thing
    other_vals = factor(names(handt),levels = cards)
    best_other = as.character(other_vals[which.min(other_vals)])
    newtype = detect_type(gsub(x = hand,pattern = "J",replacement = best_other))
    return(newtype)
  }
  max_val = names(sort(handt))[length(handt)]
  newtype = detect_type(gsub(x = hand,pattern = "J",replacement = max_val))
  return(newtype)
}
# apply the two functions
input$types = sapply(input$hand,detect_type)
input$joker_alt = mapply(joker_alternative,input$hand,input$types)

# split differently now
input_l = split(input,f = input$joker_alt)

# rest should be similar
internal_order = function(i_sub) {
  f_matrix = as.data.frame(str_split_fixed(i_sub$hand,pattern = "",n = 5))
  f_matrix[,1:5] = lapply(f_matrix[,1:5],FUN = factor,levels=c(rev(cards)))
  i_sub = cbind(i_sub,f_matrix)
  i_sub = i_sub[order(i_sub$V1,i_sub$V2,i_sub$V3,i_sub$V4,i_sub$V5),]
  i_sub$internal_order = 1:nrow(i_sub)
  return(i_sub)
}
input_ordered = lapply(input_l,internal_order)
input_ordered = do.call(rbind,input_ordered)

# order it by joker alt
input_ordered$joker_alt = factor(input_ordered$joker_alt,levels = rev(classes))
ordered = input_ordered[order(input_ordered$joker_alt,input_ordered$internal_order,decreasing = F),]

# get final calculation
ordered$rank = 1:nrow(ordered)
sum(ordered$rank * ordered$bid)
