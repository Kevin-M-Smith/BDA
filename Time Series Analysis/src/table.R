## TABLE 1 - By Month, Mean, SD
means = function(){
  ms = list()
  for(i in 1:12){
    ms[i] = mean(dfg$Flow[which(dfg$MID==i)])
  }  
  unlist(ms)
}

stdvs = function(){
  ss = list()
  for(i in 1:12){
    ss[i] = sd(dfg$Flow[which(dfg$MID==i)])
  }
  unlist(ss)
}

cvs = function(){
  cv = list()
  for(i in 1:12){
    cv[i] = sd(dfg$Flow[which(dfg$MID==i)]) / mean(dfg$Flow[which(dfg$MID==i)])
  }
  unlist(cv)
}

tab1 = data.frame(1:12)
colnames(tab1) <- "Months"
tab1$Months = unique(dfg$Month)
colnames(tab1) <- ""
tab1$Mean_CFS = signif(means(), 4)
tab1$Stdev_CFS = signif(stdvs(), 4)
tab1$CV = signif(cvs(), 4)
tab1[13,] = c("__Overall__", paste("__",signif(mean(dfg$Flow),4),"__",sep=""), paste("__",signif(sd(dfg$Flow),4), "__",sep=""), paste("__",signif(sd(dfg$Flow)/mean(dfg$Flow),4),"__",sep=""))
colnames(tab1) <- c("", "Mean (CFS)", "SD (CFS)", "CV")





