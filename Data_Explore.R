# Initialization
library("Hmisc")
library("ggplot2")
library("reshape2")
library("lme4")
library("nlme")
InternStart.2014=as.Date("07/01/2014",'%m/%d/%Y')
InternStart.2015=as.Date("07/01/2015",'%m/%d/%Y')
####################################### Functions #######################################
## sleep
SleepSummary=function(dayfile,subj,allcols,InternStart){
  sub=dayfile[which(dayfile$Id %in% subj),]
  subvalid=sub[which(sub$TotalMinutesAsleep!=0 & sub$TotalTimeInBed!=0),]
  subvalid$SleepDay=as.Date(substr(as.character(subvalid$SleepDay),1,nchar(as.character(subvalid$SleepDay))-12),'%m/%d/%Y')
  start=subvalid$SleepDay[1]; end=subvalid$SleepDay[nrow(subvalid)]
  total=nrow(sub);valid=nrow(subvalid)
  
  meanasleep=mean(subvalid$TotalMinutesAsleep);meaninbed=mean(subvalid$TotalTimeInBed);meanratio=meanasleep/meaninbed
  sdasleep=sd(subvalid$TotalMinutesAsleep);sdinbed=sd(subvalid$TotalTimeInBed)
  longestasleep=max(subvalid$TotalMinutesAsleep);longestinbed=max(subvalid$TotalTimeInBed)
  shortestasleep=min(subvalid$TotalMinutesAsleep);shortestinbed=min(subvalid$TotalTimeInBed)
  
  subvalid.bf=subvalid[which(subvalid$SleepDay < InternStart),]
  meanasleep.bf=mean(subvalid.bf$TotalMinutesAsleep);meaninbed.bf=mean(subvalid.bf$TotalTimeInBed);meanratio.bf=meanasleep.bf/meaninbed.bf
  sdasleep.bf=sd(subvalid.bf$TotalMinutesAsleep);sdinbed.bf=sd(subvalid.bf$TotalTimeInBed)
  longestasleep.bf=max(subvalid.bf$TotalMinutesAsleep);longestinbed.bf=max(subvalid.bf$TotalTimeInBed)
  shortestasleep.bf=min(subvalid.bf$TotalMinutesAsleep);shortestinbed.bf=min(subvalid.bf$TotalTimeInBed)
  
  subvalid.af=subvalid[which(subvalid$SleepDay > InternStart),]
  meanasleep.af=mean(subvalid.af$TotalMinutesAsleep);meaninbed.af=mean(subvalid.af$TotalTimeInBed);meanratio.af=meanasleep.af/meaninbed.af
  sdasleep.af=sd(subvalid.af$TotalMinutesAsleep);sdinbed.af=sd(subvalid.af$TotalTimeInBed)
  longestasleep.af=max(subvalid.af$TotalMinutesAsleep);longestinbed.af=max(subvalid.af$TotalTimeInBed)
  shortestasleep.af=min(subvalid.af$TotalMinutesAsleep);shortestinbed.af=min(subvalid.af$TotalTimeInBed)
  
  out=data.frame(subj,start,end,total,valid,
                 meanasleep,meaninbed,meanratio,sdasleep,sdinbed,longestasleep,shortestasleep,longestinbed,shortestinbed,
                 meanasleep.bf,meaninbed.bf,meanratio.bf,sdasleep.bf,sdinbed.bf,longestasleep.bf,shortestasleep.bf,longestinbed.bf,shortestinbed.bf,
                 meanasleep.af,meaninbed.af,meanratio.af,sdasleep.af,sdinbed.af,longestasleep.af,shortestasleep.af,longestinbed.af,shortestinbed.af
                 )
  colnames(out)=allcols
  return(out)
}
## Activity
ActivitySummary=function(dayfile,subj,allcols,InternStart){
  sub=dayfile[which(dayfile$Id %in% subj),]
  subvalid=sub[which(sub$TotalSteps!=0),]
  subvalid$ActivityDate=as.Date(subvalid$ActivityDate,'%m/%d/%Y')
  start=subvalid$ActivityDate[1];end=subvalid$ActivityDate[nrow(subvalid)]
  total=nrow(sub);valid=nrow(subvalid)
  subvalid.bf=subvalid[which(subvalid$ActivityDate < InternStart),]
  subvalid.af=subvalid[which(subvalid$ActivityDate > InternStart),]
  
  meanstep=mean(subvalid$TotalSteps);sdstep=sd(subvalid$TotalSteps);moststep=max(subvalid$TotalSteps);leaststep=min(subvalid$TotalSteps)
  meanstep.bf=mean(subvalid.bf$TotalSteps);sdstep.bf=sd(subvalid.bf$TotalSteps);moststep.bf=max(subvalid.bf$TotalSteps);leaststep.bf=min(subvalid.bf$TotalSteps)
  meanstep.af=mean(subvalid.af$TotalSteps);sdstep.af=sd(subvalid.af$TotalSteps);moststep.af=max(subvalid.af$TotalSteps);leaststep.af=min(subvalid.af$TotalSteps)
  
  meancalories=mean(subvalid$Calories);sdcalories=sd(subvalid$Calories);
  meancalories.bf=mean(subvalid.bf$Calories);sdcalories.bf=sd(subvalid.bf$Calories);
  meancalories.af=mean(subvalid.af$Calories);sdcalories.af=sd(subvalid.af$Calories);
  
  meandistance=mean(subvalid$TotalDistance);sddistance=sd(subvalid$TotalDistance);
  meandistance.bf=mean(subvalid.bf$TotalDistance);sddistance.bf=sd(subvalid.bf$TotalDistance);
  meandistance.af=mean(subvalid.af$TotalDistance);sddistance.af=sd(subvalid.af$TotalDistance);
  
  out=data.frame(subj,start,end,total,valid,
                 meanstep,sdstep,moststep,leaststep,meanstep.bf,sdstep.bf,moststep.bf,leaststep.bf,meanstep.af,sdstep.af,moststep.af,leaststep.af,
                 meancalories,sdcalories,meancalories.bf,sdcalories.bf,meancalories.af,sdcalories.af,
                 meandistance,sddistance,meandistance.bf,sddistance.bf,meandistance.af,sddistance.af)
  
  subvalid$weekday=weekdays(as.Date(subvalid$ActivityDate,'%m/%d/%Y'))
  weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  for (w in weekdays){
    subvalidw=subvalid[which(subvalid$weekday==w),]
    out[[w]]=mean(subvalidw$TotalSteps)
  }  
  colnames(out)=allcols
  return(out)
}

####################################### Sleep #######################################
# day sleep 2014
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
sleep.SubjIDs.2014=unique(Sleep2014$Id)
# minute sleep 2014
Sleepmin2014=read.csv('work//Fitbit//2014_Cohort_all//minuteSleep_merged.csv')
# day sleep 2015
Sleep2015=read.csv('work//Fitbit//2015_Cohort_all/sleepDay_merged.csv')
sleep.SubjIDs.2015=unique(Sleep2015$Id)

# Summary for each subject 2014
sleep.charcols=c("id");sleep.datecols=c("start","end")
sleep.numcols=c("total","valid",
                "meanMinAsleep","meanMinInbed","meanAsleepInbedratio","sdAsleep","sdInbed","longestAsleep","shortestAsleep","longestInbed","shortestInbed",
                "meanMinAsleep.bf","meanMinInbed.bf","meanAsleepInbedratio.bf","sdAsleep.bf","sdInbed.bf","longestAsleep.bf","shortestAsleep.bf","longestInbed.bf","shortestInbed.bf",
                "meanMinAsleep.af","meanMinInbed.af","meanAsleepInbedratio.af","sdAsleep.af","sdInbed.af","longestAsleep.af","shortestAsleep.af","longestInbed.af","shortestInbed.af")
sleep.allcols=c(sleep.charcols,sleep,datecols,sleep.numcols)
for (i in 1:length(sleep.SubjIDs.2014)){
  t=SleepSummary(Sleep2014,sleep.SubjIDs.2014[i],sleep.allcols,InternStart.2014)
  for (tcol in sleep.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in sleep.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.sleep.2014=t}
  else{Summary.sleep.2014=rbind(Summary.sleep.2014,t)} 
}
for (i in 1:length(sleep.SubjIDs.2015)){
  t=SleepSummary(Sleep2015,sleep.SubjIDs.2015[i],sleep.allcols,InternStart.2015)
  for (tcol in sleep.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in sleep.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.sleep.2015=t}
  else{Summary.sleep.2015=rbind(Summary.sleep.2015,t)} 
}
# exclude subject with less than x day's record (x=sleep.recordday)
sleep.recordday=10
Summary.sleep.2014=Summary.sleep.2014[which(Summary.sleep.2014$valid>sleep.recordday),]
Summary.sleep.2015=Summary.sleep.2015[which(Summary.sleep.2015$valid>sleep.recordday),]
Summary.sleep=rbind(Summary.sleep.2014,Summary.sleep.2015)
# paired ttest of before and after
sleep.model.paired.meanasleep.2014=t.test(Summary.sleep.2014$meanMinAsleep.bf,Summary.sleep.2014$meanMinAsleep.af,paired=T)
sleep.model.paired.meanasleep.2015=t.test(Summary.sleep.2015$meanMinAsleep.bf,Summary.sleep.2015$meanMinAsleep.af,paired=T)
sleep.model.paired.meaninbed.2014=t.test(Summary.sleep.2014$meanMinInbed.bf,Summary.sleep.2014$meanMinInbed.af,paired=T)
sleep.model.paired.meaninbed.2015=t.test(Summary.sleep.2015$meanMinInbed.bf,Summary.sleep.2015$meanMinInbed.af,paired=T)
sleep.model.paired.meanasleep=t.test(Summary.sleep$meanMinAsleep.bf,Summary.sleep$meanMinAsleep.af,paired=T)
sleep.model.paired.meaninbed=t.test(Summary.sleep$meanMinInbed.bf,Summary.sleep$meanMinInbed.af,paired=T)
sleep.model.paired.meanratio=t.test(Summary.sleep$meanAsleepInbedratio.bf,Summary.sleep$meanAsleepInbedratio.af,paired=T)

####################################### Activity #######################################
## daily activity
Activity2014=read.csv('work/Fitbit/2014_Cohort_all/dailyActivity_merged.csv')
Activity2015=read.csv('work//Fitbit//2015_Cohort_all//dailyActivity_merged.csv')
activity.SubjIDs.2014=unique(Activity2014$Id);activity.SubjIDs.2015=unique(Activity2015$Id)
## Steps Summary
activity.charcols=c("id");activity.datecols=c("start","end")
activity.numcols=c("total","valid","meanSteps","sdSteps","longestSteps","shortestSteps",
               "meanSteps.bf","sdSteps.bf","longestSteps.bf","shortestSteps.bf","meanSteps.af","sdSteps.af","longestSteps.af","shortestSteps.af",
               "meancalories","sdcalories","meancalories.bf","sdcalories.bf","meancalories.af","sdcalories.af",
               "meandistance","sddistance","meandistance.bf","sddistance.bf","meandistance.af","sddistance.af",
               "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
activity.allcols=c(activity.charcols,activity.datecols,activity.numcols)
for (i in 1:length(activity.SubjIDs.2014)){
  t=ActivitySummary(Activity2014,activity.SubjIDs.2014[i],activity.allcols,InternStart.2014)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.activity.2014=t}
  else{Summary.activity.2014=rbind(Summary.activity.2014,t)}
}
for (i in 1:length(activity.SubjIDs.2015)){
  t=ActivitySummary(Activity2015,activity.SubjIDs.2015[i],activity.allcols,InternStart.2015)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.activity.2015=t}
  else{Summary.activity.2015=rbind(Summary.activity.2015,t)}
}
# exclude subject with less than x day's record (x=sleep.recordday)
activity.recordday=10
Summary.activity.2014=Summary.activity.2014[which(Summary.activity.2014$valid>activity.recordday),]
Summary.activity.2015=Summary.activity.2015[which(Summary.activity.2015$valid>activity.recordday),]
Summary.activity=rbind(Summary.activity.2014,Summary.activity.2015)
# paired ttest of steps/calories/distance before and after 
activity.model.paired.meanstep.2014=t.test(Summary.activity.2014$meanSteps.bf,Summary.activity.2014$meanSteps.af,paired=T)
activity.model.paired.meanstep.2015=t.test(Summary.activity.2015$meanSteps.bf,Summary.activity.2015$meanSteps.af,paired=T)
activity.model.paired.meanstep=t.test(Summary.activity$meanSteps.bf,Summary.activity$meanSteps.af,paired=T)
activity.model.paired.meancal.2014=t.test(Summary.activity.2014$meancalories.bf,Summary.activity.2014$meancalories.af,paired=T)
activity.model.paired.meancal.2015=t.test(Summary.activity.2015$meancalories.bf,Summary.activity.2015$meancalories.af,paired=T)
activity.model.paired.meancal=t.test(Summary.activity$meancalories.bf,Summary.activity$meancalories.af,paired=T)
activity.model.paired.meandist.2014=t.test(Summary.activity.2014$meandistance.bf,Summary.activity.2014$meandistance.af,paired=T)
activity.model.paired.meandist.2015=t.test(Summary.activity.2015$meandistance.bf,Summary.activity.2015$meandistance.af,paired=T)
activity.model.paired.meandist=t.test(Summary.activity$meandistance.bf,Summary.activity$meandistance.af,paired=T)
# correlations of steps vs. calories/distance
activity.corr.stepvscal=rcorr(Summary.activity$meanSteps,Summary.activity$meancalories)
activity.corr.stepvsdist=rcorr(Summary.activity$meanSteps,Summary.activity$meandistance)

# Plot weekday scatter plot for each subj
weekdaynames=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
step.weekday=Summary.activity[weekdaynames]
step.weekday.tm=melt(t(step.weekday))
ggplot(step.weekday.tm,aes(x=Var1,y=value,group=factor(Var2))) + 
  geom_line(aes(color=factor(Var2))) +
  labs(x="Weekdays",y="Steps",color="Subject")
# Plot weekday mean and sd
step.weekday=step.weekday[-c(7),]
step.weekday.summary=data.frame(index=c(1:length(step.weekday)),
                                mean=apply(step.weekday,2,mean,na.rm=TRUE),
                                sd=apply(step.weekday,2,sd,na.rm=TRUE))
plot(step.weekday.summary$index,step.weekday.summary$mean,
     ylim=range(2000,15000),xaxt="n",
     xlab="weekdays",ylab="meanStep")
axis(1,at=step.weekday.summary$index,labels=rownames(step.weekday.summary))
with(data=step.weekday.summary,
     expr=errbar(index,mean,mean+sd,mean-sd,add=T,pch=1,cap=.1))
#"growth" curve
step.weekday.withid=Summary.activity[c("id","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")]
step.weekday.withid.m=melt(step.weekday.withid,id="id",variable.name="weekdays",value.name = "meansteps")
stepmodel.base=lmer(meansteps~weekdayind+(weekdayind|id),data=step.weekday.withid.m,REML=F)
step.weekday.withid.m$id=factor(step.weekday.withid.m$id)
step.weekday.withid.m$weekdayind=as.numeric(pmatch(step.weekday.withid.m$weekdays,weekdaynames,dup=TRUE))
stepmodel.linear=lme(meansteps~weekdayind, data=step.weekday.withid.m, random = ~ 1 |id, method="ML")

