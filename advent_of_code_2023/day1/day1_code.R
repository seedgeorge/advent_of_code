setwd("/Volumes/ICRSHARED/Cancer Biomarkers/George/Current/Projects/Work_Adjacent/advent_of_code_2023/day1")

# solution 1
input = readLines("day1_input.txt")
nocharacter = gsub(x=input,pattern = "[a-zA-Z]+",replacement = "")
num1 = as.numeric(substr(nocharacter,1,1))
num2 = as.numeric(substr(nocharacter,nchar(nocharacter),nchar(nocharacter)))
sum(as.numeric(paste(num1,num2,sep="")))

## solution 2
library(stringr)
input = readLines("day1_input.txt")

replaces = c("one" = "1","two" = "2","three" = "3" ,
             "four" = "4","five" = "5","six" = "6",
             "seven" = "7","eight" = "8","nine" = "9")
replaces2 = replaces
names(replaces2) = stri_reverse(names(replaces2))
strings1 = stri_extract_all(input,regex = c("one|1|two|2|three|3|four|4|five|5|six|6|seven|7|eight|8|nine|9"))
firstchar = str_replace_all(string = unlist(lapply(strings,FUN = function(x) x[1])),pattern = replaces)
strings2 = stri_extract_all(stringi::stri_reverse(input),regex = stri_reverse(c("one|1|two|2|three|3|four|4|five|5|six|6|seven|7|eight|8|nine|9")))
lastchar = str_replace_all(string = unlist(lapply(strings2,FUN = function(x) x[1])),pattern = replaces2)
sum(as.numeric(paste(firstchar,lastchar,sep="")))