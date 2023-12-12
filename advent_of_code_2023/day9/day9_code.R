setwd("~/Documents/AoC/advent_of_code_2023/day9")
library(stringr)
library(readr)
#input = read_delim("day9_test.txt", col_names = F)
input = read_delim("day9_input.txt", col_names = F)

# part1
#input_df = str_split_fixed(input,pattern = " ",n = 6) # n may have to change
#input_df = apply(input_df,2,as.numeric)
input_df = input

extrapolate_seq = function(sequence) {
  seq = sequence
  not0 = TRUE
  seqlist = list()
  counter = 1
  seqlist[[counter]] = seq
  # build the list
  while(not0 == TRUE) {
    counter = counter + 1
    seq = diff(seq)
    seqlist[[counter]] = seq
    if (sum(seq == 0) == length(seq)) {
      not0 = FALSE
    }
  }
  # then add values
  for(entry in length(seqlist):1) {
    if (entry == length(seqlist)) {
      seqlist[[entry]] = c(seqlist[[entry]],0)
    } else {
      seqlist[[entry]] = c(seqlist[[entry]], 
                           tail(seqlist[[entry]],1) + seqlist[[entry + 1]][length(seqlist[[entry + 1]])]  )
    }
  }
  return(tail(seqlist[[1]],1))
}

sum(apply(input_df,MARGIN = 1,extrapolate_seq))


# part 2
#change the function and voila

input_df = input

extrapolate_seq = function(sequence) {
  seq = sequence
  not0 = TRUE
  seqlist = list()
  counter = 1
  seqlist[[counter]] = seq
  # build the list
  while(not0 == TRUE) {
    counter = counter + 1
    seq = diff(seq)
    seqlist[[counter]] = seq
    if (sum(seq == 0) == length(seq)) {
      not0 = FALSE
    }
  }
  # then add values
  for(entry in length(seqlist):1) {
    if (entry == length(seqlist)) {
      seqlist[[entry]] = c(0, seqlist[[entry]])
    } else {
      seqlist[[entry]] = c(head(seqlist[[entry]],1) - head(seqlist[[entry + 1]],1),
                           seqlist[[entry]])
    }
  }
  return(head(seqlist[[1]],1))
}

sum(apply(input_df,MARGIN = 1,extrapolate_seq))
