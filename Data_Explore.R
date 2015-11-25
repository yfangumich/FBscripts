## Functions
# sleep
SleepSummary=function(dayfile,subj,allcols){
  dates=which(dayfile$Id %in% subj)
  sub=dayfile[dates,]
  subeffect=sub[which(sub$TotalMinutesAsleep!=0 & sub$TotalTimeInBed!=0),]
  start=dayfile$SleepDay[dates[1]];start=substr(as.character(start),1,nchar(as.character(start))-12)
  end=dayfile$SleepDay[dates[length(dates)]];end=substr(as.character(end),1,nchar(as.character(end))-12)
  total=nrow(sub)
  effect=nrow(subeffect)
  meanasleep=mean(subeffect$TotalMinutesAsleep);meaninbed=mean(subeffect$TotalTimeInBed);meanratio=meanasleep/meaninbed
  sdasleep=sd(subeffect$TotalMinutesAsleep);sdinbed=sd(subeffect$TotalTimeInBed)
  longestasleep=max(subeffect$TotalMinutesAsleep);longestinbed=max(subeffect$TotalTimeInBed)
  shortestasleep=min(subeffect$TotalMinutesAsleep);shortestinbed=min(subeffect$TotalTimeInBed)
  out=data.frame(subj,start,end,total,effect,meanasleep,meaninbed,meanratio,sdasleep,sdinbed,
                 longestasleep,shortestasleep,longestinbed,shortestinbed)
  colnames(out)=allcols
  return(out)
}
# step
StepSummary=function(dayfile,subj,allcols){
  dates=which(dayfile$Id %in% subj)
  sub=dayfile[dates,]
  subeffect=sub[which(sub$StepTotal!=0),]
  start=dayfile$ActivityDay[dates[1]]
  end=dayfile$ActivityDay[dates[length(dates)]]
  total=nrow(sub)
  effect=nrow(subeffect)
  meanstep=mean(subeffect$StepTotal);sdstep=sd(subeffect$StepTotal)
  moststep=max(subeffect$StepTotal);leaststep=min(subeffect$StepTotal)
  out=data.frame(subj,start,end,total,effect,meanstep,sdstep,moststep,leaststep)
  colnames(out)=allcols
  return(out)
}

## SLEEP
# day sleep
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
SubjIDs=unique(Sleep2014$Id)

# minute sleep
Sleepmin2014=read.csv('work//Fitbit//2014_Cohort_all//minuteSleep_merged.csv')

# Summary for each subject
sleep.charcols=c("id","start","end")
sleep.numcols=c("total","effect","meanMinAsleep","meanMinInbed","meanAsleepInbedratio","sdAsleep","sdInbed",
          "longestAsleep","shortestAsleep","longestInbed","shortestInbed")
sleep.allcols=c(sleep.charcols,sleep.numcols)
for (i in 1:length(SubjIDs)){
  t=SleepSummary(Sleep2014,SubjIDs[i],sleep.allcols)
  for (tcol in sleep.charcols){
    t[[tcol]]=as.character(t[[tcol]])
  }
  for (tcol in sleep.numcols){
    t[[tcol]]=as.numeric(t[[tcol]])
  }
  if (i==1){
    Summary.sleep=t}
  else{
    Summary.sleep=rbind(Summary.sleep,t)}
}

## Step
# day step
Step2014=read.csv('work/Fitbit/2014_Cohort_all/dailySteps_merged.csv')

# Summary for each subject
step.charcols=c("id","start","end")
step.numcols=c("total","effect","meanSteps","sdSteps","longestSteps","shortestSteps")
step.allcols=c(step.charcols,step.numcols)
for (i in 1:length(SubjIDs)){
  t=StepSummary(Step2014,SubjIDs[i],step.allcols)
  for (tcol in step.charcols){
    t[[tcol]]=as.character(t[[tcol]])
  }
  for (tcol in step.numcols){
    t[[tcol]]=as.numeric(t[[tcol]])
  }
  if (i==1){
    Summary.step=t}
  else{
    Summary.step=rbind(Summary.step,t)}
}

