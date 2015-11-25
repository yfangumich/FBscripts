DateRange=function(y,x){
  dates=which(y$Id %in% x)
  start=y$SleepDay[dates[1]]
  end=y$SleepDay[dates[length(dates)]]
  out=data.frame(x,start,end)
  colnames(out)=c("id","start","end")
  return(out)
}
# day sleep
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
SubjIDs=unique(Sleep2014$Id)
DR=data.frame(id=character(),
                     start=character(),
                     end=character())
for (i in 1:length(SubjIDs)){
t=DateRange(Sleep2014,SubjIDs[i])
t$id=as.character(t$id)
t$start=as.character(t$start)
t$end=as.character(t$end)
DR=rbind(DR,t)
}
