
# read in dataset
ganges <- read.csv("data/Ganges.csv")

# put into data frame, replacing months with numbers for sorting
dfg = data.frame(ganges)
colnames(dfg) = c("Year", seq(1,12))
# Year    1    2    3    4    5    6     7     8     9    10   11   12
# 1 1934 2778 2458 2228 2138 1987 3613 19775 36277 40084 18625 6197 3432
# 2 1935 2389 2056 1625 1888 1654 2918 13086 38239 27645 13908 4531 3356
# 3 1936 2858 2312 2442 1434 1778 5189 22873 39497 38061 16688 6665 3830


# melt dataset
dfg = melt(dfg, id.vars="Year")
# Year variable value
# 1 1934        1  2778
# 2 1935        1  2389
# 3 1936        1  2858


# sort dataset
dfg = dfg[order(dfg$Year, dfg$variable),]
# Year variable value
# 1   1934        1  2778
# 80  1934        2  2458
# 159 1934        3  2228

# Rename columns
colnames(dfg) = c("Year", "MID", "Flow")

# Create empty Month column, and populate. 
dfg$Month = rep(0, 948)
dfg$Month[which(dfg$MID==1)] = "January"
dfg$Month[which(dfg$MID==2)] = "February"
dfg$Month[which(dfg$MID==3)] = "March"
dfg$Month[which(dfg$MID==4)] = "April"
dfg$Month[which(dfg$MID==5)] = "May"
dfg$Month[which(dfg$MID==6)] = "June"
dfg$Month[which(dfg$MID==7)] = "July"
dfg$Month[which(dfg$MID==8)] = "August"
dfg$Month[which(dfg$MID==9)] = "September"
dfg$Month[which(dfg$MID==10)] = "October"
dfg$Month[which(dfg$MID==11)] = "November"
dfg$Month[which(dfg$MID==12)] = "December"

# Create empty short month column, and populate. 
dfg$Mon = rep(0, 948)
dfg$Mon[which(dfg$MID==1)] = "Jan"
dfg$Mon[which(dfg$MID==2)] = "Feb"
dfg$Mon[which(dfg$MID==3)] = "Mar"
dfg$Mon[which(dfg$MID==4)] = "Apr"
dfg$Mon[which(dfg$MID==5)] = "May"
dfg$Mon[which(dfg$MID==6)] = "Jun"
dfg$Mon[which(dfg$MID==7)] = "Jul"
dfg$Mon[which(dfg$MID==8)] = "Aug"
dfg$Mon[which(dfg$MID==9)] = "Sep"
dfg$Mon[which(dfg$MID==10)] = "Oct"
dfg$Mon[which(dfg$MID==11)] = "Nov"
dfg$Mon[which(dfg$MID==12)] = "Dec"

# Create index column
dfg$idx = seq(1,948)

# Yearly Flows
years = list();
flows = list(); 
index = 1; 

for (year in (1934:2012)){
  years[index] = year; 
  
  day_y = 0; 
  sum = 0;
  
  for ( MIDt in (1:12)){
    day_m = days.in.month(year, MIDt)
    day_y = day_y + day_m
    sum = sum + (dfg$Flow[intersect(which(dfg$Year==year), which(dfg$MID==MIDt))] * day_m)
  }
  
  print(sum)
  
  flows[index] = sum / (day_y)
  index = index + 1
}

dfgb = data.frame(unlist(years), unlist(flows))

#Nov - April
select = c("November", "December", "January", "February", "March", "April")
dfgc = dfg[dfg$Month %in% select,]

#June - Sept
select = c("May", "June", "July", "August", "September")
dfgd = dfg[dfg$Month %in% select,]
