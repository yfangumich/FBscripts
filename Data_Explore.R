setClass(Class="Subject",representation(id="factor",
                                        start="factor",
                                        end="factor"))
DateRange=function(y,x){
  dates=which(y$Id %in% x)
  start=y$SleepDay[dates[1]]
  end=y$SleepDay[dates[length(dates)]]
  return(new("Subject",id=x,start=start, end=end))
}
# day sleep
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
SubjIDs=unique(Sleep2014$Id)
t=DateRange(Sleep2014,SubjIDs[1])

