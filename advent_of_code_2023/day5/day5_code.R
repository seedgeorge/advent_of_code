setwd("~/Documents/AoC/advent_of_code_2023/day5")
library(readr)
library(stringr)
library(parallel)
input = readLines("day5_test.txt")
input = readLines("day5_input.txt")

# #seeds
# seedline = input[1]
# seedline = gsub(x = seedline,pattern = "seeds: ",replacement = "")
# seedstarts = unlist(str_split(seedline,pattern = " "))
# seedstarts = as.numeric(seedstarts)
# seedstarts = seedstarts+1
# 
# # find white spaces
# spaces = which(input == "")
# # find seed-soil map
# seedsoil = which(input == "seed-to-soil map:")
# seedspaces = min(spaces[spaces > seedsoil])
# seedsoil_data = input[(seedsoil+1):(seedspaces -1)]
# seedsoil_data = apply(str_split_fixed(seedsoil_data,pattern = " ",n = 3),MARGIN = 2,as.numeric)
# seedsoil_data[,1:2] = seedsoil_data[,1:2] + 1
# 
# # find soil-fert map
# soilfert = which(input == "soil-to-fertilizer map:")
# soilspaces = min(spaces[spaces > soilfert])
# soilfert_data = input[(soilfert+1):(soilspaces -1)]
# soilfert_data = apply(str_split_fixed(soilfert_data,pattern = " ",n = 3),2,as.numeric)
# soilfert_data[,1:2] = soilfert_data[,1:2] + 1
# 
# # find fert-water map
# fertwater = which(input == "fertilizer-to-water map:")
# fertspaces = min(spaces[spaces > fertwater])
# fertwater_data = input[(fertwater+1):(fertspaces -1)]
# fertwater_data = apply(str_split_fixed(fertwater_data,pattern = " ",n = 3),2,as.numeric)
# fertwater_data[,1:2] = fertwater_data[,1:2] + 1
# 
# # find water-light map
# waterlight = which(input == "water-to-light map:")
# waterspaces = min(spaces[spaces > waterlight])
# waterlight_data = input[(waterlight+1):(waterspaces -1)]
# waterlight_data = apply(str_split_fixed(waterlight_data,pattern = " ",n = 3),2,as.numeric)
# waterlight_data[,1:2] = waterlight_data[,1:2] + 1
# 
# # find light-temp map
# lighttemp = which(input == "light-to-temperature map:")
# lightspaces = min(spaces[spaces > lighttemp])
# lighttemp_data = input[(lighttemp+1):(lightspaces -1)]
# lighttemp_data = apply(str_split_fixed(lighttemp_data,pattern = " ",n = 3),2,as.numeric)
# lighttemp_data[,1:2] = lighttemp_data[,1:2] + 1
# 
# # find temp-hum map
# temphum = which(input == "temperature-to-humidity map:")
# tempspaces = min(spaces[spaces > temphum])
# temphum_data = input[(temphum+1):(tempspaces -1)]
# temphum_data = apply(str_split_fixed(temphum_data,pattern = " ",n = 3),2,as.numeric)
# temphum_data[,1:2] = temphum_data[,1:2] + 1
# 
# # find hum-loc map
# humloc = which(input == "humidity-to-location map:")
# humspaces = length(input)
# humloc_data = input[(humloc+1):(humspaces)]
# humloc_data = apply(str_split_fixed(humloc_data,pattern = " ",n = 3),2,as.numeric)
# humloc_data[,1:2] = humloc_data[,1:2] + 1
# 
# # build the full data frame
# df = data.frame(matrix(nrow = 0, ncol = 7))
# colnames(df) = c("soil","fert","water","light","temp","hum","loc")
# 
# # map seed/soil
# for (row in 1:nrow(seedsoil_data)) {
#   seed = seedsoil_data[row,2]:(seedsoil_data[row,2] + (seedsoil_data[row,3] -1))
#   soil = seedsoil_data[row,1]:(seedsoil_data[row,1] + (seedsoil_data[row,3] -1))
#   df[seed,"soil"] = soil
# }
# df$soil[is.na(df$soil)] = rownames(df)[is.na(df$soil)]
# 
# # soil fert
# for (row in 1:nrow(soilfert_data)) {
#   soil = soilfert_data[row,2]:(soilfert_data[row,2] + (soilfert_data[row,3] -1))
#   fert = soilfert_data[row,1]:(soilfert_data[row,1] + (soilfert_data[row,3] -1))
#   df$fert[match(soil, df$soil)] = fert
# }
# df$fert[is.na(df$fert)] = df$soil[is.na(df$fert)]
# 
# # fert water
# for (row in 1:nrow(fertwater_data)) {
#   fert = fertwater_data[row,2]:(fertwater_data[row,2] + (fertwater_data[row,3] -1))
#   water = fertwater_data[row,1]:(fertwater_data[row,1] + (fertwater_data[row,3] -1))
#   df$water[match(fert, df$fert)] = water
# }
# df$water[is.na(df$water)] = df$fert[is.na(df$water)]
# 
# # water light
# for (row in 1:nrow(waterlight_data)) {
#   water = waterlight_data[row,2]:(waterlight_data[row,2] + (waterlight_data[row,3] -1))
#   light = waterlight_data[row,1]:(waterlight_data[row,1] + (waterlight_data[row,3] -1))
#   df$light[match(water, df$water)] = light
# }
# df$light[is.na(df$light)] = df$water[is.na(df$light)]
# 
# # light temp
# for (row in 1:nrow(lighttemp_data)) {
#   light = lighttemp_data[row,2]:(lighttemp_data[row,2] + (lighttemp_data[row,3] -1))
#   temp = lighttemp_data[row,1]:(lighttemp_data[row,1] + (lighttemp_data[row,3] -1))
#   df$temp[match(light, df$light)] = temp
# }
# df$temp[is.na(df$temp)] = df$light[is.na(df$temp)]
# 
# # light temp
# for (row in 1:nrow(temphum_data)) {
#   temp = temphum_data[row,2]:(temphum_data[row,2] + (temphum_data[row,3] -1))
#   hum = temphum_data[row,1]:(temphum_data[row,1] + (temphum_data[row,3] -1))
#   df$hum[match(temp, df$temp)] = hum
# }
# df$hum[is.na(df$hum)] = df$temp[is.na(df$hum)]
# 
# # light temp
# for (row in 1:nrow(humloc_data)) {
#   hum = humloc_data[row,2]:(humloc_data[row,2] + (humloc_data[row,3] -1))
#   loc = humloc_data[row,1]:(humloc_data[row,1] + (humloc_data[row,3] -1))
#   df$loc[match(hum, df$hum)] = loc
# }
# df$loc[is.na(df$loc)] = df$hum[is.na(df$loc)]
# 
# # conver to numeric
# df_m1 = apply(df,2,as.numeric)
# df_m1 = df_m1[seedstarts,] -1
# #
# minloc = min(df_m1[,"loc"])



####### part 1
seedline = input[1]
seedline = gsub(x = seedline,pattern = "seeds: ",replacement = "")
seedstarts = unlist(str_split(seedline,pattern = " "))
seedstarts = as.numeric(seedstarts)  

# find white spaces
spaces = which(input == "")
# find seed-soil map
seedsoil = which(input == "seed-to-soil map:")
seedspaces = min(spaces[spaces > seedsoil])
seedsoil_data = input[(seedsoil+1):(seedspaces -1)]
seedsoil_data = apply(str_split_fixed(seedsoil_data,pattern = " ",n = 3),MARGIN = 2,as.numeric) 

# find soil-fert map
soilfert = which(input == "soil-to-fertilizer map:")
soilspaces = min(spaces[spaces > soilfert])
soilfert_data = input[(soilfert+1):(soilspaces -1)]
soilfert_data = apply(str_split_fixed(soilfert_data,pattern = " ",n = 3),2,as.numeric) 

# find fert-water map
fertwater = which(input == "fertilizer-to-water map:")
fertspaces = min(spaces[spaces > fertwater])
fertwater_data = input[(fertwater+1):(fertspaces -1)]
fertwater_data = apply(str_split_fixed(fertwater_data,pattern = " ",n = 3),2,as.numeric)

# find water-light map
waterlight = which(input == "water-to-light map:")
waterspaces = min(spaces[spaces > waterlight])
waterlight_data = input[(waterlight+1):(waterspaces -1)]
waterlight_data = apply(str_split_fixed(waterlight_data,pattern = " ",n = 3),2,as.numeric)

# find light-temp map
lighttemp = which(input == "light-to-temperature map:")
lightspaces = min(spaces[spaces > lighttemp])
lighttemp_data = input[(lighttemp+1):(lightspaces -1)]
lighttemp_data = apply(str_split_fixed(lighttemp_data,pattern = " ",n = 3),2,as.numeric)

# find temp-hum map
temphum = which(input == "temperature-to-humidity map:")
tempspaces = min(spaces[spaces > temphum])
temphum_data = input[(temphum+1):(tempspaces -1)]
temphum_data = apply(str_split_fixed(temphum_data,pattern = " ",n = 3),2,as.numeric)

# find hum-loc map
humloc = which(input == "humidity-to-location map:")
humspaces = length(input)
humloc_data = input[(humloc+1):(humspaces)]
humloc_data = apply(str_split_fixed(humloc_data,pattern = " ",n = 3),2,as.numeric)


map_to_ranges = function(map) {
  range = as.data.frame(map)
  colnames(range) = c("tostart","fromstart","width")
  range$toend = range[,1] + (range[,3] )
  range$fromend = range[,2] + (range[,3] )
  return(range)
}
seedsoil_ranges = map_to_ranges(seedsoil_data)
soilfert_ranges = map_to_ranges(soilfert_data)
fertwater_ranges = map_to_ranges(fertwater_data)
waterlight_ranges = map_to_ranges(waterlight_data)
lighttemp_ranges = map_to_ranges(lighttemp_data)
temphum_ranges = map_to_ranges(temphum_data)
humloc_ranges = map_to_ranges(humloc_data)

convert_val = function(val,ranges) {
  rangesub = ranges[val >= ranges$fromstart & val < ranges$fromend,]
  if(nrow (rangesub) > 0) {
    diffval = val - rangesub$fromstart
    newval = rangesub$tostart + diffval
    return(newval)
  } else {
    return(val)
  }
}

convert_seed = function (seed) {
  val = seed
  loc = convert_val(val = convert_val(
    val = convert_val(
      val = convert_val(
        val = convert_val(
          val = convert_val(
            val = convert_val(val = val, 
                              ranges = seedsoil_ranges),
            ranges = soilfert_ranges
          ),
          ranges = fertwater_ranges
        ),
        ranges = waterlight_ranges
      ),
      ranges = lighttemp_ranges
    ),
    ranges = temphum_ranges
  ),
  ranges = humloc_ranges)
  return(loc)
}

# minloc 
system.time(min(sapply(seedstarts,convert_seed)))




## part 2
input = readLines("day5_test.txt")
input = readLines("day5_input.txt")

seedline = input[1]
seedline = gsub(x = seedline,pattern = "seeds: ",replacement = "")
seedstarts = unlist(str_split(seedline,pattern = " "))
seedstarts = as.numeric(seedstarts)  
seedstarts = seedstarts 
  
# find white spaces
spaces = which(input == "")
# find seed-soil map
seedsoil = which(input == "seed-to-soil map:")
seedspaces = min(spaces[spaces > seedsoil])
seedsoil_data = input[(seedsoil+1):(seedspaces -1)]
seedsoil_data = apply(str_split_fixed(seedsoil_data,pattern = " ",n = 3),MARGIN = 2,as.numeric) 

# find soil-fert map
soilfert = which(input == "soil-to-fertilizer map:")
soilspaces = min(spaces[spaces > soilfert])
soilfert_data = input[(soilfert+1):(soilspaces -1)]
soilfert_data = apply(str_split_fixed(soilfert_data,pattern = " ",n = 3),2,as.numeric) 

# find fert-water map
fertwater = which(input == "fertilizer-to-water map:")
fertspaces = min(spaces[spaces > fertwater])
fertwater_data = input[(fertwater+1):(fertspaces -1)]
fertwater_data = apply(str_split_fixed(fertwater_data,pattern = " ",n = 3),2,as.numeric)

# find water-light map
waterlight = which(input == "water-to-light map:")
waterspaces = min(spaces[spaces > waterlight])
waterlight_data = input[(waterlight+1):(waterspaces -1)]
waterlight_data = apply(str_split_fixed(waterlight_data,pattern = " ",n = 3),2,as.numeric)

# find light-temp map
lighttemp = which(input == "light-to-temperature map:")
lightspaces = min(spaces[spaces > lighttemp])
lighttemp_data = input[(lighttemp+1):(lightspaces -1)]
lighttemp_data = apply(str_split_fixed(lighttemp_data,pattern = " ",n = 3),2,as.numeric)

# find temp-hum map
temphum = which(input == "temperature-to-humidity map:")
tempspaces = min(spaces[spaces > temphum])
temphum_data = input[(temphum+1):(tempspaces -1)]
temphum_data = apply(str_split_fixed(temphum_data,pattern = " ",n = 3),2,as.numeric)

# find hum-loc map
humloc = which(input == "humidity-to-location map:")
humspaces = length(input)
humloc_data = input[(humloc+1):(humspaces)]
humloc_data = apply(str_split_fixed(humloc_data,pattern = " ",n = 3),2,as.numeric)


bigfrom = seedstarts[c(TRUE,FALSE)]
bigranges = seedstarts[c(FALSE,TRUE)]
bigto = bigfrom + (bigranges -1 )

# hmm
## test it, not using iranges? 
input_ranges = data.frame(start = bigfrom[1],end = bigto[i])
input_ranges = data.frame(start = 30,end = 60)

reference_ranges = data.frame(start = seedsoil_data[,2], 
                              end = seedsoil_data[,2] + (seedsoil_data[,3] - 1 ),
                              offset = seedsoil_data[,1] - seedsoil_data[,2])

input_ranges = setDT(input_ranges)
reference_ranges = setDT(reference_ranges)
setkey(input_ranges,start,end)
over = foverlaps(y = input_ranges,x = reference_ranges, )

calc_next_ranges = function(startrange,reference_ranges) {
  reference_ranges$offset = reference_ranges$tostart - reference_ranges$fromstart 
  inrange = IRanges(start=reference_ranges$fromstart,width = reference_ranges$width,offset = reference_ranges$offset)
  
  splitranges = disjoin(c(inrange[poverlaps(startrange,inrange)],startrange))
  #splitranges = splitranges[poverlaps(splitranges,startrange),]
  splitranges = subsetByOverlaps(splitranges,startrange)
  offsets = mcols(inrange)$offset[subjectHits(findOverlaps(splitranges,inrange))]
  if(length(offsets) > 0) { 
    mcols(splitranges)$offset[queryHits(findOverlaps(splitranges,inrange))] = offsets
    mcols(splitranges)$offset[is.na(mcols(splitranges)$offset)] = 0
    newranges = shift(splitranges,shift = mcols(splitranges)$offset)
  } else {
    newranges = splitranges
  }
  return(newranges)
}

for (i in 1:length(bigfrom)) {
  input_ranges = IRanges(start = bigfrom[i],width=bigranges[i],)
  
  soil = calc_next_ranges(startrange = input_ranges, 
                          reference_ranges = seedsoil_ranges)
  fert = calc_next_ranges(startrange = soil, 
                            reference_ranges = soilfert_ranges)
  water = calc_next_ranges(startrange = fert, 
                             reference_ranges = fertwater_ranges)
  light = calc_next_ranges(startrange = water, 
                           reference_ranges = waterlight_ranges)
  temp = calc_next_ranges(startrange = light, 
                           reference_ranges = lighttemp_ranges)
  hum = calc_next_ranges(startrange = temp, 
                          reference_ranges = temphum_ranges)
  loc = calc_next_ranges(startrange = hum, 
                         reference_ranges = humloc_ranges)
  print(min(start(loc)))
  
  # 
  # final_loc = calc_next_ranges(calc_next_ranges(
  #   calc_next_ranges(
  #     calc_next_ranges(
  #       calc_next_ranges(
  #         calc_next_ranges(
  #           calc_next_ranges(startrange = input_ranges, 
  #                            reference_ranges = seedsoil_ranges),
  #           reference_ranges = soilfert_ranges
  #         ),
  #         reference_ranges = fertwater_ranges
  #       ),
  #       waterlight_ranges
  #     ),
  #     lighttemp_ranges
  #   ),
  #   temphum_ranges
  # ),
  # humloc_ranges)
  # 
  #print(final_loc)
}

5 = 1343486056
8 = 1482842780
