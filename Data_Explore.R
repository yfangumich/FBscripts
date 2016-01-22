# Initialization
library("Hmisc")
library("ggplot2")
library("reshape2")
library("lme4")
library("lmerTest")
library("nlme")
library("plyr")
library("multcomp");library("multcompView")
library("TukeyC")
library("lsmeans")
library("stargazer")
library("sjPlot") # table functions
library("sjmisc")
library("texreg")
library("gtools")
####################################### Functions #######################################
## Sleep
SleepSummary=function(dayfile,subj,allcols,InternStart){
  sub=dayfile[which(dayfile$Id %in% subj),]
  start=subvalid$SleepDay[1]; end=subvalid$SleepDay[nrow(subvalid)]
  total=nrow(sub);valid=nrow(subvalid)
  
  meanasleep=mean(subvalid$TotalMinutesAsleep);meaninbed=mean(subvalid$TotalTimeInBed);meanratio=meanasleep/meaninbed
  sdasleep=sd(subvalid$TotalMinutesAsleep);sdinbed=sd(subvalid$TotalTimeInBed)
  longestasleep=max(subvalid$TotalMinutesAsleep);longestinbed=max(subvalid$TotalTimeInBed)
  shortestasleep=min(subvalid$TotalMinutesAsleep);shortestinbed=min(subvalid$TotalTimeInBed)
  
  subvalid.pre=subvalid[which(subvalid$SleepDay < InternStart),]
  meanasleep.pre=mean(subvalid.pre$TotalMinutesAsleep);meaninbed.pre=mean(subvalid.pre$TotalTimeInBed);meanratio.pre=meanasleep.pre/meaninbed.pre
  sdasleep.pre=sd(subvalid.pre$TotalMinutesAsleep);sdinbed.pre=sd(subvalid.pre$TotalTimeInBed)
  longestasleep.pre=max(subvalid.pre$TotalMinutesAsleep);longestinbed.pre=max(subvalid.pre$TotalTimeInBed)
  shortestasleep.pre=min(subvalid.pre$TotalMinutesAsleep);shortestinbed.pre=min(subvalid.pre$TotalTimeInBed)
  
  subvalid.post=subvalid[which(subvalid$SleepDay >= InternStart),]
  meanasleep.post=mean(subvalid.post$TotalMinutesAsleep);meaninbed.post=mean(subvalid.post$TotalTimeInBed);meanratio.post=meanasleep.post/meaninbed.post
  sdasleep.post=sd(subvalid.post$TotalMinutesAsleep);sdinbed.post=sd(subvalid.post$TotalTimeInBed)
  longestasleep.post=max(subvalid.post$TotalMinutesAsleep);longestinbed.post=max(subvalid.post$TotalTimeInBed)
  shortestasleep.post=min(subvalid.post$TotalMinutesAsleep);shortestinbed.post=min(subvalid.post$TotalTimeInBed)
  
  out=data.frame(subj,start,end,total,valid,
                 meanasleep,meaninbed,meanratio,sdasleep,sdinbed,longestasleep,shortestasleep,longestinbed,shortestinbed,
                 meanasleep.pre,meaninbed.pre,meanratio.pre,sdasleep.pre,sdinbed.pre,longestasleep.pre,shortestasleep.pre,longestinbed.pre,shortestinbed.pre,
                 meanasleep.post,meaninbed.post,meanratio.post,sdasleep.post,sdinbed.post,longestasleep.post,shortestasleep.post,longestinbed.post,shortestinbed.post
                 )
  colnames(out)=allcols
  return(out)
}
## DailyActivity
ActivitySummary=function(dayfile,subj,allcols,InternStart){
  sub=dayfile[which(dayfile$Id %in% subj),]
  start=subvalid$ActivityDate[1];end=subvalid$ActivityDate[nrow(subvalid)]
  total=nrow(sub);valid=nrow(subvalid)
  subvalid.pre=subvalid[which(subvalid$ActivityDate < InternStart),]
  subvalid.post=subvalid[which(subvalid$ActivityDate >= InternStart),]
  
  meanstep=mean(subvalid$TotalSteps);sdstep=sd(subvalid$TotalSteps);moststep=max(subvalid$TotalSteps);leaststep=min(subvalid$TotalSteps)
  meanstep.pre=mean(subvalid.pre$TotalSteps);sdstep.pre=sd(subvalid.pre$TotalSteps);moststep.pre=max(subvalid.pre$TotalSteps);leaststep.pre=min(subvalid.pre$TotalSteps)
  meanstep.post=mean(subvalid.post$TotalSteps);sdstep.post=sd(subvalid.post$TotalSteps);moststep.post=max(subvalid.post$TotalSteps);leaststep.post=min(subvalid.post$TotalSteps)
  
  meancalories=mean(subvalid$Calories);sdcalories=sd(subvalid$Calories);
  meancalories.pre=mean(subvalid.pre$Calories);sdcalories.pre=sd(subvalid.pre$Calories);
  meancalories.post=mean(subvalid.post$Calories);sdcalories.post=sd(subvalid.post$Calories);
  
  meandistance=mean(subvalid$TotalDistance);sddistance=sd(subvalid$TotalDistance);
  meandistance.pre=mean(subvalid.pre$TotalDistance);sddistance.pre=sd(subvalid.pre$TotalDistance);
  meandistance.post=mean(subvalid.post$TotalDistance);sddistance.post=sd(subvalid.post$TotalDistance);
  
  meanveryactmin=mean(subvalid$VeryActiveMinutes);sdveryactmin=sd(subvalid$VeryActiveMinutes);
  meanveryactmin.pre=mean(subvalid.pre$VeryActiveMinutes);sdveryactmin.pre=sd(subvalid.pre$VeryActiveMinutes);
  meanveryactmin.post=mean(subvalid.post$VeryActiveMinutes);sdveryactmin.post=sd(subvalid.post$VeryActiveMinutes);
  
  meanfairactmin=mean(subvalid$FairlyActiveMinutes);sdfairactmin=sd(subvalid$FairlyActiveMinutes);
  meanfairactmin.pre=mean(subvalid.pre$FairlyActiveMinutes);sdfairactmin.pre=sd(subvalid.pre$FairlyActiveMinutes);
  meanfairactmin.post=mean(subvalid.post$FairlyActiveMinutes);sdfairactmin.post=sd(subvalid.post$FairlyActiveMinutes);
  
  meanlightactmin=mean(subvalid$LightlyActiveMinutes);sdlightactmin=sd(subvalid$LightlyActiveMinutes);
  meanlightactmin.pre=mean(subvalid.pre$LightlyActiveMinutes);sdlightactmin.pre=sd(subvalid.pre$LightlyActiveMinutes);
  meanlightactmin.post=mean(subvalid.post$LightlyActiveMinutes);sdlightactmin.post=sd(subvalid.post$LightlyActiveMinutes);
  
  meansedmin=mean(subvalid$SedentaryMinutes);sdsedmin=sd(subvalid$SedentaryMinutes);
  meansedmin.pre=mean(subvalid.pre$SedentaryMinutes);sdsedmin.pre=sd(subvalid.pre$SedentaryMinutes);
  meansedmin.post=mean(subvalid.post$SedentaryMinutes);sdsedmin.post=sd(subvalid.post$SedentaryMinutes);
    
  out=data.frame(subj,start,end,total,valid,
                 meanstep,sdstep,moststep,leaststep,meanstep.pre,sdstep.pre,moststep.pre,leaststep.pre,meanstep.post,sdstep.post,moststep.post,leaststep.post,
                 meancalories,sdcalories,meancalories.pre,sdcalories.pre,meancalories.post,sdcalories.post,
                 meandistance,sddistance,meandistance.pre,sddistance.pre,meandistance.post,sddistance.post,
                 meanveryactmin,sdveryactmin,meanveryactmin.pre,sdveryactmin.pre,meanveryactmin.post,sdveryactmin.post,
                 meanfairactmin,sdfairactmin,meanfairactmin.pre,sdfairactmin.pre,meanfairactmin.post,sdfairactmin.post,
                 meanlightactmin,sdlightactmin,meanlightactmin.pre,sdlightactmin.pre,meanlightactmin.post,sdlightactmin.post,
                 meansedmin,sdsedmin,meansedmin.pre,sdsedmin.pre,meansedmin.post,sdsedmin.post)
  
  subvalid$weekday=weekdays(as.Date(subvalid$ActivityDate,'%m/%d/%Y'))
  weekdays=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  for (w in weekdays){
    subvalidw=subvalid[which(subvalid$weekday==w),]
    out[[w]]=mean(subvalidw$TotalSteps)
  }  
  colnames(out)=allcols
  return(out)
}
## HourlyStep
StephrSummary=function(hrfile,subj,allcols,InternStart){
  subvalid=hrfile[which(hrfile$Id %in% subj),]
  subvalid=subvalid[,!(names(subvalid) %in% c("StepTotal","totalstep","totalhour"))]
  subvalid.pre=subvalid[which(subvalid$ActivityDate < InternStart),]
  subvalid.post=subvalid[which(subvalid$ActivityDate >= InternStart),]
  mean.pre=aggregate(subvalid.pre[,3:26],subvalid.pre["Id"],function(x) mean(x,na.rm=T))
  mean.post=aggregate(subvalid.post[,3:26],subvalid.post["Id"],function(x) mean(x,na.rm=T))
  out=cbind(mean.pre,mean.post[,-1])
  colnames(out)=allcols
  return(out)
}
## Mood
MoodSummary=function(dayfile,subj,allcols,InternStart){
  sub=dayfile[which(dayfile$userid %in% subj),]
  subvalid=sub[which(!is.na(sub$mood)),]
  start=sort(subvalid$Date_mood)[1]; end=sort(subvalid$Date_mood)[nrow(subvalid)]
  total=nrow(sub);valid=nrow(subvalid)
    
  subvalid.pre=subvalid[which(subvalid$Date_mood < InternStart),]
  meanmood.pre=mean(subvalid.pre$mood);
  sdmood.pre=sd(subvalid.pre$mood);
  
  subvalid.post=subvalid[which(subvalid$Date_mood >= InternStart),]
  meanmood.post=mean(subvalid.post$mood);
  sdmood.post=sd(subvalid.post$mood);
  
  out=data.frame(subj,start,end,total,valid,
                 meanmood.pre,sdmood.pre,
                 meanmood.post,sdmood.post)
  colnames(out)=allcols
  return(out)
}

####################################### StartDate ####################################### 
StartDate2014=read.csv('Z:././././Data Analysis/Yu Fang/data/2014BioShort1.csv')
StartDate2015=read.csv('Z:./././././Data Analysis/Yu Fang/data/2015BioShort1.csv')
StartDate2014=StartDate2014[c("USERID","StartDate")]
StartDate2015=StartDate2015[c("USERID","StartDate")];StartDate2015$USERID=as.character(StartDate2015$USERID)
StartDates=rbind(StartDate2014,StartDate2015)

####################################### Sleep #######################################
# day sleep 2014
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
Sleep2014$SleepDay=as.Date(substr(as.character(Sleep2014$SleepDay),1,nchar(as.character(Sleep2014$SleepDay))-4),'%m/%d/%Y')
sleep.SubjIDs.2014=unique(Sleep2014$Id)
# day sleep 2015
Sleep2015=read.csv('work//Fitbit//2015_Cohort_all/sleepDay_merged.csv')
Sleep2015$SleepDay=as.Date(substr(as.character(Sleep2015$SleepDay),1,nchar(as.character(Sleep2015$SleepDay))-12),'%m/%d/%Y')
sleep.SubjIDs.2015=unique(Sleep2015$Id)
Sleep=rbind(Sleep2014,Sleep2015)
Sleep=Sleep[which(Sleep$TotalMinutesAsleep!=0 & Sleep$TotalTimeInBed!=0),]
# Summary for each subject 2014
sleep.charcols=c("id");sleep.datecols=c("start","end")
sleep.numcols=c("total","valid",
                "meanMinAsleep","meanMinInbed","meanAsleepInbedratio","sdAsleep","sdInbed","longestAsleep","shortestAsleep","longestInbed","shortestInbed",
                "meanMinAsleep.pre","meanMinInbed.pre","meanAsleepInbedratio.pre","sdAsleep.pre","sdInbed.pre","longestAsleep.pre","shortestAsleep.pre","longestInbed.pre","shortestInbed.pre",
                "meanMinAsleep.post","meanMinInbed.post","meanAsleepInbedratio.post","sdAsleep.post","sdInbed.post","longestAsleep.post","shortestAsleep.post","longestInbed.post","shortestInbed.post")
sleep.allcols=c(sleep.charcols,sleep.datecols,sleep.numcols)
# needs to combine the below two parts
for (i in 1:length(sleep.SubjIDs.2014)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% sleep.SubjIDs.2014[i]],"%m/%d/%Y")
  t=SleepSummary(Sleep2014,sleep.SubjIDs.2014[i],sleep.allcols,startdate)
  for (tcol in sleep.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in sleep.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.sleep.2014=t}
  else{Summary.sleep.2014=rbind(Summary.sleep.2014,t)} 
}
for (i in 1:length(sleep.SubjIDs.2015)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% sleep.SubjIDs.2015[i]],"%m/%d/%Y")
  t=SleepSummary(Sleep2015,sleep.SubjIDs.2015[i],sleep.allcols,startdate)
  for (tcol in sleep.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in sleep.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.sleep.2015=t}
  else{Summary.sleep.2015=rbind(Summary.sleep.2015,t)} 
}
# exclude subject with less than x day's record (x=sleep.recordday)
sleep.recordday=10
Summary.sleep.2014=Summary.sleep.2014[which(Summary.sleep.2014$valid>sleep.recordday),]
Summary.sleep.2015=Summary.sleep.2015[which(Summary.sleep.2015$valid>sleep.recordday & !is.na(Summary.sleep.2015$meanMinAsleep.pre)),]
Summary.sleep=rbind(Summary.sleep.2014,Summary.sleep.2015)
# paired ttest of before and after
sleep.model.paired.meanasleep.2014=t.test(Summary.sleep.2014$meanMinAsleep.pre,Summary.sleep.2014$meanMinAsleep.post,paired=T)
sleep.model.paired.meanasleep.2015=t.test(Summary.sleep.2015$meanMinAsleep.pre,Summary.sleep.2015$meanMinAsleep.post,paired=T)
sleep.model.paired.meaninbed.2014=t.test(Summary.sleep.2014$meanMinInbed.pre,Summary.sleep.2014$meanMinInbed.post,paired=T)
sleep.model.paired.meaninbed.2015=t.test(Summary.sleep.2015$meanMinInbed.pre,Summary.sleep.2015$meanMinInbed.post,paired=T)
sleep.model.paired.meanasleep=t.test(Summary.sleep$meanMinAsleep.pre,Summary.sleep$meanMinAsleep.post,paired=T)
sleep.model.paired.meaninbed=t.test(Summary.sleep$meanMinInbed.pre,Summary.sleep$meanMinInbed.post,paired=T)
sleep.model.paired.meanratio.2014=t.test(Summary.sleep.2014$meanAsleepInbedratio.pre,Summary.sleep.2014$meanAsleepInbedratio.post,paired=T)
sleep.model.paired.meanratio.2015=t.test(Summary.sleep.2015$meanAsleepInbedratio.pre,Summary.sleep.2015$meanAsleepInbedratio.post,paired=T)
sleep.model.paired.meanratio=t.test(Summary.sleep$meanAsleepInbedratio.pre,Summary.sleep$meanAsleepInbedratio.post,paired=T)

####################################### Activity #######################################
#### daily activity ####
Activity2014=read.csv('work/Fitbit/2014_Cohort_all/dailyActivity_merged.csv')
Activity2015=read.csv('work//Fitbit//2015_Cohort_all//dailyActivity_merged.csv')
activity.SubjIDs.2014=unique(Activity2014$Id);activity.SubjIDs.2015=unique(Activity2015$Id);
Activity2014$ActivityDate=as.Date(Activity2014$ActivityDate,'%m/%d/%Y')
Activity2015$ActivityDate=as.Date(Activity2015$ActivityDate,'%m/%d/%Y')
Activity=rbind(Activity2014,Activity2015)
Activity=Activity[which(Activity$TotalSteps!=0),]
## Steps Summary
activity.charcols=c("id");activity.datecols=c("start","end")
activity.numcols=c("total","valid","meanSteps","sdSteps","longestSteps","shortestSteps",
               "meanSteps.pre","sdSteps.pre","longestSteps.pre","shortestSteps.pre","meanSteps.post","sdSteps.post","longestSteps.post","shortestSteps.post",
               "meancalories","sdcalories","meancalories.pre","sdcalories.pre","meancalories.post","sdcalories.post",
               "meandistance","sddistance","meandistance.pre","sddistance.pre","meandistance.post","sddistance.post",
               "meanveryactmin","sdveryactmin","meanveryactmin.pre","sdveryactmin.pre","meanveryactmin.post","sdveryactmin.post",
               "meanfairactmin","sdfairactmin","meanfairactmin.pre","sdfairactmin.pre","meanfairactmin.post","sdfairactmin.post",
               "meanlightactmin","sdlightactmin","meanlightactmin.pre","sdlightactmin.pre","meanlightactmin.post","sdlightactmin.post",
               "meansedmin","sdsedmin","meansedmin.pre","sdsedmin.pre","meansedmin.post","sdsedmin.post",
               "Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
activity.allcols=c(activity.charcols,activity.datecols,activity.numcols)
for (i in 1:length(activity.SubjIDs.2014)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% activity.SubjIDs.2014[i]],"%m/%d/%Y")
  t=ActivitySummary(Activity2014,activity.SubjIDs.2014[i],activity.allcols,startdate)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.activity.2014=t}
  else{Summary.activity.2014=rbind(Summary.activity.2014,t)}
}
for (i in 1:length(activity.SubjIDs.2015)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% activity.SubjIDs.2015[i]],"%m/%d/%Y")
  t=ActivitySummary(Activity2015,activity.SubjIDs.2015[i],activity.allcols,startdate)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.activity.2015=t}
  else{Summary.activity.2015=rbind(Summary.activity.2015,t)}
}
#### exclude subject with less than x day's record (x=sleep.recordday) ####
activity.recordday=10
Summary.activity.2014=Summary.activity.2014[which(Summary.activity.2014$valid>activity.recordday),]
Summary.activity.2015=Summary.activity.2015[which(Summary.activity.2015$valid>activity.recordday),]
Summary.activity=rbind(Summary.activity.2014,Summary.activity.2015)
#### paired ttest of steps/calories/distance before and after ####
activity.model.paired.meanstep.2014=t.test(Summary.activity.2014$meanSteps.pre,Summary.activity.2014$meanSteps.post,paired=T)
activity.model.paired.meanstep.2015=t.test(Summary.activity.2015$meanSteps.pre,Summary.activity.2015$meanSteps.post,paired=T)
activity.model.paired.meanstep=t.test(Summary.activity$meanSteps.pre,Summary.activity$meanSteps.post,paired=T)
activity.model.paired.meancal.2014=t.test(Summary.activity.2014$meancalories.pre,Summary.activity.2014$meancalories.post,paired=T)
activity.model.paired.meancal.2015=t.test(Summary.activity.2015$meancalories.pre,Summary.activity.2015$meancalories.post,paired=T)
activity.model.paired.meancal=t.test(Summary.activity$meancalories.pre,Summary.activity$meancalories.post,paired=T)
activity.model.paired.meandist.2014=t.test(Summary.activity.2014$meandistance.pre,Summary.activity.2014$meandistance.post,paired=T)
activity.model.paired.meandist.2015=t.test(Summary.activity.2015$meandistance.pre,Summary.activity.2015$meandistance.post,paired=T)
activity.model.paired.meandist=t.test(Summary.activity$meandistance.pre,Summary.activity$meandistance.post,paired=T)
activity.model.paired.meanveryactmin.2014=t.test(Summary.activity.2014$meanveryactmin.pre,Summary.activity.2014$meanveryactmin.post,paired=T)
activity.model.paired.meanfairactmin.2014=t.test(Summary.activity.2014$meanfairactmin.pre,Summary.activity.2014$meanfairactmin.post,paired=T)
activity.model.paired.meanlightactmin.2014=t.test(Summary.activity.2014$meanlightactmin.pre,Summary.activity.2014$meanlightactmin.post,paired=T)
activity.model.paired.meansedmin.2014=t.test(Summary.activity.2014$meansedmin.pre,Summary.activity.2014$meansedmin.post,paired=T)
activity.model.paired.meanveryactmin.2015=t.test(Summary.activity.2015$meanveryactmin.pre,Summary.activity.2015$meanveryactmin.post,paired=T)
activity.model.paired.meanfairactmin.2015=t.test(Summary.activity.2015$meanfairactmin.pre,Summary.activity.2015$meanfairactmin.post,paired=T)
activity.model.paired.meanlightactmin.2015=t.test(Summary.activity.2015$meanlightactmin.pre,Summary.activity.2015$meanlightactmin.post,paired=T)
activity.model.paired.meansedmin.2015=t.test(Summary.activity.2015$meansedmin.pre,Summary.activity.2015$meansedmin.post,paired=T)
activity.model.paired.meanveryactmin=t.test(Summary.activity$meanveryactmin.pre,Summary.activity$meanveryactmin.post,paired=T)
activity.model.paired.meanfairactmin=t.test(Summary.activity$meanfairactmin.pre,Summary.activity$meanfairactmin.post,paired=T)
activity.model.paired.meanlightactmin=t.test(Summary.activity$meanlightactmin.pre,Summary.activity$meanlightactmin.post,paired=T)
activity.model.paired.meansedmin=t.test(Summary.activity$meansedmin.pre,Summary.activity$meansedmin.post,paired=T)
##### correlations of steps vs. calories/distance ####
activity.corr.stepvscal=rcorr(Summary.activity$meanSteps,Summary.activity$meancalories)
activity.corr.stepvsdist=rcorr(Summary.activity$meanSteps,Summary.activity$meandistance)
#### weekdays ####
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
#### hourly step ####
stephr.charcols=c("id")
stephr.numcols=c(paste(formatC(0:23,width=2,flag="0"),":00:00.pre",sep=""),paste(formatC(0:23,width=2,flag="0"),":00:00.post",sep=""))
stephr.allcols=c(stephr.charcols,stephr.numcols)
Stephr2014=read.csv('work/Fitbit//2014_Cohort_all//hourlySteps_merged.csv')
Stephr2015=read.csv('work/Fitbit//2015_Cohort_all/hourlySteps_merged.csv');Stephr2015$Id=as.character(Stephr2015$Id)
Stephr=rbind(Stephr2014,Stephr2015)
activity.SubjIDs=unique(Stephr$Id)
Stephr$ActivityHour=as.character(as.POSIXct(strptime(as.character(Stephr$ActivityHour),"%m/%d/%Y %I:%M:%S %p")))
Stephr$ActivityDate=substr(Stephr$ActivityHour,1,10)
Stephr$ActivityHour=substr(Stephr$ActivityHour,12,end)
Stephr.collapse=ddply(Stephr,.(Id,ActivityDate),summarise,StepTotal=paste(StepTotal,collapse=','))
for (i in 1:nrow(Stephr.collapse)){
  tmp=Stephr.collapse$StepTotal[i]
  hrs=as.numeric(unlist(strsplit(tmp,",")))
  Stephr.collapse$totalstep[i]=sum(hrs)
  Stephr.collapse$totalhour[i]=length(hrs)
}
Stephr.collapse=Stephr.collapse[which(Stephr.collapse$totalstep>0 & Stephr.collapse$totalhour==24 ),]
allhrs=t(matrix(as.numeric(unlist(strsplit(Stephr.collapse$StepTotal,","))),ncol=nrow(Stephr.collapse),nrow=24))
for (ihr in 0:23){
  Stephr.collapse[,paste(ihr,":00:00",sep="")]=allhrs[,ihr+1]
}
for (i in 1:length(activity.SubjIDs)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% activity.SubjIDs[i]],"%m/%d/%Y")
  if (length(startdate!=0)){
  t=StephrSummary(Stephr.collapse,activity.SubjIDs[i],stephr.allcols,startdate)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  if (i==1){ Summary.stephr=t
     } else { Summary.stephr=rbind(Summary.stephr,t)}
  } else {print(i)}
}
# temporarily create subsets
#Summary.stephr.backup=Summary.stephr
#Summary.stephr=Summary.stephr.backup
#Summary.stephr=Summary.stephr[which(Summary.stephr$id %in% activity.SubjIDs.2014),]
# calculate stats
Summary.stephr.mean=colMeans(Summary.stephr[,-1])
Summary.stephr.sd=apply(Summary.stephr[,-1],2,sd)
n=nrow(Summary.stephr)
Summary.stephr.error=qt(0.975,df=n-1)*Summary.stephr.sd/sqrt(n)
#### plot 24hr pattern ####
x=c(1:24);xaxislabel=names(Stephr.collapse)[6:29];xaxislabel[seq(2,24,2)]=""
plot(x,Summary.stephr.mean[1:24],ylim=c(0,900),type="l",xaxt="n",xlab="hour",ylab="average steps",main="Hourly step pattern before/after Internship starts")
axis(1,at=seq(1,24),labels=xaxislabel)
polygon(c(rev(x), x), 
        c(rev(Summary.stephr.mean[1:24]-Summary.stephr.error[1:24]), Summary.stephr.mean[1:24]+Summary.stephr.error[1:24]), 
          col = adjustcolor('grey80',alpha.f=0.2), border = NA)
lines(x,Summary.stephr.mean[1:24],lwd=2,col="red")
lines(x,Summary.stephr.mean[1:24]-Summary.stephr.error[1:24],lty='dashed',col='black')
lines(x,Summary.stephr.mean[1:24]+Summary.stephr.error[1:24],lty='dashed',col='black')
lines(x,Summary.stephr.mean[25:48])
polygon(c(rev(x), x), 
        c(rev(Summary.stephr.mean[25:48]-Summary.stephr.error[25:48]), Summary.stephr.mean[25:48]+Summary.stephr.error[25:48]), 
        col = adjustcolor('grey80',alpha.f=0.2), border = NA)
lines(x,Summary.stephr.mean[25:48],lwd=2,col="green")
lines(x,Summary.stephr.mean[25:48]+Summary.stephr.error[25:48],lty='dashed',col='black')
lines(x,Summary.stephr.mean[25:48]-Summary.stephr.error[25:48],lty='dashed',col='black')
legend("topright",bty="n",c("Before","After"),lty=c(1,1),lwd=c(2.5,2.5),col=c("red","green"))
#### Compare variance ####
## wilcoxon signed-rank test
# test average over subjects pre/post
wilcox.test(Summary.stephr.mean[1:24],Summary.stephr.mean[25:48],paired=TRUE)
## nested anova
Summary.stephr.nest=data.frame(timerange=as.character(),hour=as.numeric(),subj=as.character(),steps=as.numeric())
for (i in 1:nrow(Summary.stephr)){
  t=data.frame(timerange=character(),hour=integer(),subj=character(),steps=double(),stringsAsFactors = FALSE)
  for (j in 1:24){
    t[j,1]="pre"
    t[j,2]=j-1
    t[j,3]=Summary.stephr$id[i]
    t[j,4]=Summary.stephr[i,j+1]
  }
  for (j in 25:48){
    t[j,1]="post"
    t[j,2]=j-25
    t[j,3]=Summary.stephr$id[i]
    t[j,4]=Summary.stephr[i,j+1]
  }
  Summary.stephr.nest=rbind(Summary.stephr.nest,t)
}
Summary.stephr.nest$timerange=factor(Summary.stephr.nest$timerange,levels=c("pre","post"))
Summary.stephr.nest$hour=factor(Summary.stephr.nest$hour,levels=c(0:23))
with(Summary.stephr.nest,tapply(steps,list(timerange,subj),mean))
with(Summary.stephr.nest,boxplot(steps~subj+timerange))
with(Summary.stephr.nest,tapply(steps,list(timerange,hour),mean))
with(Summary.stephr.nest,boxplot(steps~hour+timerange))
with(Summary.stephr.nest,tapply(steps,list(timerange,subj,hour),mean))
with(Summary.stephr.nest,boxplot(steps~subj+hour+timerange))
# two-way within subjects ANOVA - subj serves as random factor
Summary.stephr.nest.aov=aov(steps ~ timerange * hour + Error(subj/(timerange*hour)),data=Summary.stephr.nest)
summary(Summary.stephr.nest.aov)
print(model.tables(Summary.stephr.nest.aov,"means"),digits=3)
stargazer(Summary.stephr.nest)
screenreg(summary(Summary.stephr.nest.aov))
write.csv(xtable(Summary.stephr.nest.aov),file="work/Fitbit/FBscripts/test.csv",sep=",")
xtable(summary(Summary.stephr.nest.aov))
####################################### Mood #######################################
# 2014 (remove duplicated rows, and take average if duplicated id & date have different mood ratings)
Mood2014=read.csv('Z:/Data Analysis/Yu Fang/data/Mood_2014_all.csv',stringsAsFactors=FALSE)
Mood2014$Date_mood=as.Date(as.character(Mood2014$Date_mood),'%m/%d/%Y')
Mood2014=Mood2014[which(!is.na(Mood2014$mood)),]
Mood2014=Mood2014[!duplicated(Mood2014),]
tmp2014=Mood2014;tmp2014$iddate=paste(tmp2014$userid,tmp2014$Date_mood,sep="_")
dup2014=tmp2014[duplicated(tmp2014$iddate),]
uniq2014=tmp2014[!duplicated(tmp2014$iddate),]
intersect2014=merge(dup2014,uniq2014,by=c("userid","Date_mood"))
intersect2014$mood=(intersect2014$mood.x+intersect2014$mood.y)/2
intersect2014=intersect2014[c("userid","Date_mood","mood")]
tmp2014=tmp2014[which(!(tmp2014$iddate %in% dup2014$iddate)),]
tmp2014=tmp2014[c("userid","Date_mood","mood")]
Mood2014=rbind(tmp2014,intersect2014)
# 2015 (checked, doesn't have duplicated rows)
Mood2015.raw=read.csv('Z:/Data Analysis/Yu Fang/data/Mood_2015_all.csv',stringsAsFactors=FALSE)
Mood2015.raw[Mood2015.raw=="null"]=NA
Mood2015=data.frame(userid=character(),Date_mood=character(),mood=integer())
fmood=function(df){
  df=data.frame(lapply(df,as.integer))
  df=df[,colSums(is.na(df))<nrow(df)]
  subframe=data.frame(userid=rep(as.character(df$USERID),ncol(df)-1),Date_mood=rep(NA,ncol(df)-1),mood=rep(NA,ncol(df)-1))
  subframe$Date_mood=as.Date(names(df)[2:ncol(df)],"X%m.%d.%Y")
  subframe$mood=as.matrix(df)[2:ncol(df)]
  return(subframe)
}
for (isub in 1:nrow(Mood2015.raw)){
  Mood2015=rbind(Mood2015,fmood(Mood2015.raw[isub,]))
}
Mood2015$userid=as.character(Mood2015$userid)
# combine two years
Mood=rbind(Mood2014,Mood2015)
Mood=Mood[which(!is.na(Mood$mood)),]
#### Mood Summary ####
mood.charcols=c("id");mood.datecols=c("start","end")
mood.numcols=c("total","valid","meanMood.pre","sdMood.pre","meanMood.post","sdMood.post")
mood.allcols=c(mood.charcols,mood.datecols,mood.numcols)
mood.SubjIDs=unique(Mood$userid)
for (i in 1:length(mood.SubjIDs)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% mood.SubjIDs[i]],"%m/%d/%Y")
  t=MoodSummary(Mood,mood.SubjIDs[i],mood.allcols,startdate)
  for (tcol in mood.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in mood.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.mood=t}
  else{Summary.mood=rbind(Summary.mood,t)}
}
Summary.mood=Summary.mood[which(!is.na(Summary.mood$meanMood.pre)),]
mood.model.paired=t.test(Summary.mood$meanMood.pre,Summary.mood$meanMood.post,paired=T)
Summary.mood.2014=Summary.mood[which(Summary.mood$end < as.Date("2015-01-01")),]
mood.model.paired.2014=t.test(Summary.mood.2014$meanMood.pre,Summary.mood.2014$meanMood.post,paired=T)
Summary.mood.2015=Summary.mood[which(Summary.mood$end > as.Date("2015-01-01")),]
mood.model.paired.2015=t.test(Summary.mood.2015$meanMood.pre,Summary.mood.2015$meanMood.post,paired=T)
###### Mood & Activity ######
MoodAct=merge(Mood,Activity,by.x=c("userid","Date_mood"),by.y=c("Id","ActivityDate"),sort=TRUE)
MoodAct2014=MoodAct[which(MoodAct$Date_mood < as.Date("2015-01-01")),]
MoodAct2015=MoodAct[which(MoodAct$Date_mood > as.Date("2015-01-01")),]
MoodAct.daily=data.frame(Date_mood=as.Date(character(),'%Y-%m-%d'),sample_num=integer(),mood_mean=integer(),mood_sd=double(),
                        step_mean=integer(),step_sd=double(),veryactmin_mean=integer(),veryactmin_sd=double(),
                        fairactmin_mean=integer(),fairactmin_sd=double(),lightactmin_mean=integer(),lightactmin_sd=double(),
                        sedmin_mean=integer(),sedmin_sd=double(),cal_mean=integer(),cal_sd=double())
MoodAct.Dates=sort(unique(MoodAct$Date_mood))
for (idate in 1:length(MoodAct.Dates)){
  subdate=MoodAct[which(MoodAct$Date_mood==MoodAct.Dates[idate]),]
  MoodAct.daily[idate,"Date_mood"]=MoodAct.Dates[idate]
  MoodAct.daily[idate,"sample_num"]=nrow(subdate)
  MoodAct.daily[idate,"mood_mean"]=mean(subdate$mood)
  MoodAct.daily[idate,"mood_sd"]=sd(subdate$mood)
  MoodAct.daily[idate,"step_mean"]=mean(subdate$TotalSteps)
  MoodAct.daily[idate,"step_sd"]=sd(subdate$TotalSteps)
  MoodAct.daily[idate,"veryactmin_mean"]=mean(subdate$VeryActiveMinutes)
  MoodAct.daily[idate,"veryactmin_sd"]=sd(subdate$VeryActiveMinutes)
  MoodAct.daily[idate,"fairactmin_mean"]=mean(subdate$FairlyActiveMinutes)
  MoodAct.daily[idate,"fairactmin_sd"]=sd(subdate$FairlyActiveMinutes)
  MoodAct.daily[idate,"lightactmin_mean"]=mean(subdate$LightlyActiveMinutes)
  MoodAct.daily[idate,"lightactmin_sd"]=sd(subdate$LightlyActiveMinutes)
  MoodAct.daily[idate,"sedmin_mean"]=mean(subdate$SedentaryMinutes)
  MoodAct.daily[idate,"sedmin_sd"]=sd(subdate$SedentaryMinutes)
  MoodAct.daily[idate,"cal_mean"]=mean(subdate$Calories)
  MoodAct.daily[idate,"cal_sd"]=sd(subdate$Calories)
}
MoodAct2014.daily=MoodAct.daily[which(MoodAct.daily$Date_mood < as.Date("2015-01-01")),]
MoodAct2015.daily=MoodAct.daily[which(MoodAct.daily$Date_mood > as.Date("2015-01-01")),]
#### correlation ####
MoodAct2014.corr.stepvsmood=rcorr(MoodAct2014$mood,MoodAct2014$TotalSteps)
MoodAct2014.corr.veryvsmood=rcorr(MoodAct2014$mood,MoodAct2014$VeryActiveMinutes)
MoodAct2014.corr.fairvsmood=rcorr(MoodAct2014$mood,MoodAct2014$FairlyActiveMinutes)
MoodAct2014.corr.lightvsmood=rcorr(MoodAct2014$mood,MoodAct2014$LightlyActiveMinutes)
MoodAct2014.corr.sedvsmood=rcorr(MoodAct2014$mood,MoodAct2014$SedentaryMinutes)
MoodAct2014.corr.calvsmood=rcorr(MoodAct2014$mood,MoodAct2014$Calories)
MoodAct2015.corr.stepvsmood=rcorr(MoodAct2015$mood,MoodAct2015$TotalSteps)
MoodAct2015.corr.veryvsmood=rcorr(MoodAct2015$mood,MoodAct2015$VeryActiveMinutes)
MoodAct2015.corr.fairvsmood=rcorr(MoodAct2015$mood,MoodAct2015$FairlyActiveMinutes)
MoodAct2015.corr.lightvsmood=rcorr(MoodAct2015$mood,MoodAct2015$LightlyActiveMinutes)
MoodAct2015.corr.sedvsmood=rcorr(MoodAct2015$mood,MoodAct2015$SedentaryMinutes)
MoodAct2015.corr.calvsmood=rcorr(MoodAct2015$mood,MoodAct2015$Calories)
#### plot mood/steps ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$step_mean,type="o",axes=FALSE,bty="n",xlab="",ylab="",col="green")
axis(side=4,at=pretty(range(MoodAct2014.daily$step_mean)))
mtext("steps",side=4,line=3)
legend("topright",bty="n",c("Mood","Steps"),lty=c(1,1),lwd=c(2,2),col=c("blue","green"))
title("Mood vs Step 2014")

par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$step_mean,type="o",axes=FALSE,bty="n",xlab="",ylab="",col="green")
axis(side=4,at=pretty(range(MoodAct2015.daily$step_mean)))
mtext("steps",side=4,line=3)
legend("topright",bty="n",c("Mood","Steps"),lty=c(1,1),lwd=c(2,2),col=c("blue","green"))
title("Mood vs Step 2015")

#### plot mood/sedentary minutes ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="l",xlab="Date",ylab="Mood",main="Mood and sedentary minutes 2014",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$sedmin_mean,type="l",axes=FALSE,ylim=newrange,bty="n",xlab="",ylab="",col="black")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
MoodAct2014.daily.nonsed=MoodAct2014.daily$veryactmin_mean+MoodAct2014.daily$fairactmin_mean+MoodAct2014.daily$lightactmin_mean
#lines(MoodAct2014.daily$Date_mood,MoodAct2014.daily.nonsed,type="l",col="violet")
legend("topleft",bty="n",c("Mood","sedentary_minute"),lty=c(1,1),lwd=c(2,2),col=c("blue","black"))

par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$mood_mean,type="l",xlab="Date",ylab="Mood",main="Mood and sedentary minutes 2015",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$sedmin_mean,type="l",axes=FALSE,ylim=newrange,bty="n",xlab="",ylab="",col="black")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
MoodAct2015.daily.nonsed=MoodAct2015.daily$veryactmin_mean+MoodAct2015.daily$fairactmin_mean+MoodAct2015.daily$lightactmin_mean
lines(MoodAct2015.daily$Date_mood,MoodAct2015.daily.nonsed,type="l",col="violet")
legend("topleft",bty="n",c("Mood","sedentary_minute","non-sed_minute"),lty=c(1,1),lwd=c(2,2),col=c("blue","black","violet"))
#### plot mood/activity hours ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE);newrange=range(c(MoodAct2014.daily$veryactmin_mean,MoodAct2014.daily$fairactmin_mean,MoodAct2014.daily$lightactmin_mean))
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$lightactmin_mean,type="o",axes=FALSE,bty="n",ylim=newrange,xlab="",ylab="",col="cyan")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
lines(MoodAct2014.daily$Date_mood,MoodAct2014.daily$veryactmin_mean,type="o",col="red")
lines(MoodAct2014.daily$Date_mood,MoodAct2014.daily$fairactmin_mean,type="o",col="orange")
legend("topright",bty="n",c("Mood","very_active","fair_active","light_active"),lty=c(1,1),lwd=c(1,1),col=c("blue","red","orange","cyan"))
title("Mood vs Activity 2014")

par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE);newrange=range(c(MoodAct2015.daily$veryactmin_mean,MoodAct2015.daily$fairactmin_mean,MoodAct2015.daily$lightactmin_mean))
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$lightactmin_mean,type="o",axes=FALSE,bty="n",ylim=newrange,xlab="",ylab="",col="cyan")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
lines(MoodAct2015.daily$Date_mood,MoodAct2015.daily$veryactmin_mean,type="o",col="red")
lines(MoodAct2015.daily$Date_mood,MoodAct2015.daily$fairactmin_mean,type="o",col="orange")
legend("topright",bty="n",c("Mood","very_active","fair_active","light_active"),lty=c(1,1),lwd=c(1,1),col=c("blue","red","orange","cyan"))
title("Mood vs Activity 2015")
#### plot mood/calories ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="l",xlab="Date",ylab="Mood",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$cal_mean,type="l",axes=FALSE,bty="n",xlab="",ylab="",col="purple")
axis(side=4,at=pretty(range(MoodAct2014.daily$cal_mean)))
mtext("C",side=4,line=3)
legend("topright",bty="n",c("Mood","Calories"),lty=c(1,1),lwd=c(2,2),col=c("blue","purple"))
title("Mood vs Calories 2014")

par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$mood_mean,type="l",xlab="Date",ylab="Mood",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2015.daily$Date_mood,MoodAct2015.daily$cal_mean,type="l",axes=FALSE,bty="n",xlab="",ylab="",col="purple")
axis(side=4,at=pretty(range(MoodAct2015.daily$cal_mean)))
mtext("C",side=4,line=3)
legend("topright",bty="n",c("Mood","Calories"),lty=c(1,1),lwd=c(2,2),col=c("blue","purple"))
title("Mood vs Calories 2015")

###### Mood & Sleep ######
MoodSleep=merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","SleepDay"),sort=TRUE)
MoodSleep=MoodSleep[which(MoodSleep$TotalMinutesAsleep!=0 & !is.na(MoodSleep$mood)),]
MoodSleep$Ratio=MoodSleep$TotalMinutesAsleep/MoodSleep$TotalTimeInBed
MoodSleep.daily=data.frame(Date_mood=as.Date(character(),'%Y-%m-%d'),sample_num=integer(),mood_mean=integer(),mood_sd=double(),
                             asleep_mean=integer(),asleep_sd=double(),inbed_mean=integer(),inbed_sd=double(),
                             ratio_mean=double(),ratio_sd=double())
MoodSleep.Dates=sort(unique(MoodSleep$Date_mood))
for (idate in 1:length(MoodSleep.Dates)){
  subdate=MoodSleep[which(MoodSleep$Date_mood==MoodSleep.Dates[idate]),]
  MoodSleep.daily[idate,"Date_mood"]=MoodSleep.Dates[idate]
  MoodSleep.daily[idate,"sample_num"]=nrow(subdate)
  MoodSleep.daily[idate,"mood_mean"]=mean(subdate$mood)
  MoodSleep.daily[idate,"mood_sd"]=sd(subdate$mood)
  MoodSleep.daily[idate,"asleep_mean"]=mean(subdate$TotalMinutesAsleep)
  MoodSleep.daily[idate,"asleep_sd"]=sd(subdate$TotalMinutesAsleep)
  MoodSleep.daily[idate,"inbed_mean"]=mean(subdate$TotalTimeInBed)
  MoodSleep.daily[idate,"inbed_sd"]=sd(subdate$TotalTimeInBed)
  MoodSleep.daily[idate,"ratio_mean"]=mean(subdate$Ratio)
  MoodSleep.daily[idate,"ratio_sd"]=sd(subdate$Ratio)
}
MoodSleep2014=MoodSleep[which(MoodSleep$Date_mood < as.Date("2015-01-01")),]
MoodSleep2015=MoodSleep[which(MoodSleep$Date_mood > as.Date("2015-01-01")),]
MoodSleep2014.daily=MoodSleep.daily[which(MoodSleep.daily$Date_mood < as.Date("2015-01-01")),]
MoodSleep2015.daily=MoodSleep.daily[which(MoodSleep.daily$Date_mood > as.Date("2015-01-01")),]
#### correlation ####
MoodSleep2014.corr.asleepvsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$TotalMinutesAsleep)
MoodSleep2014.corr.inbedvsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$TotalTimeInBed)
MoodSleep2014.corr.ratiovsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$Ratio)
MoodSleep2014.corr.asleepvsinbed=rcorr(MoodSleep2014$TotalMinutesAsleep,MoodSleep2014$TotalTimeInBed)
MoodSleep2015.corr.asleepvsmood=rcorr(MoodSleep2015$mood,MoodSleep2015$TotalMinutesAsleep)
MoodSleep2015.corr.inbedvsmood=rcorr(MoodSleep2015$mood,MoodSleep2015$TotalTimeInBed)
MoodSleep2015.corr.ratiovsmood=rcorr(MoodSleep2015$mood,MoodSleep2015$Ratio)
MoodSleep2015.corr.asleepvsinbed=rcorr(MoodSleep2015$TotalMinutesAsleep,MoodSleep2015$TotalTimeInBed)

#### plot mood/sleep ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE);newrange=c(200,1300)
plot(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$asleep_mean,type="o",axes=FALSE,bty="n",ylim=newrange,xlab="",ylab="",col="plum")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
lines(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$inbed_mean,type="o",col="peru")
legend("topright",bty="n",c("Mood","asleep","inbed"),lty=c(1,1),lwd=c(1,1),col=c("blue","plum","peru"))
title("Mood vs Sleep 2014")

par(mar=c(5,4,4,6)+0.1)
plot(MoodSleep2015.daily$Date_mood,MoodSleep2015.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE);newrange=c(200,1300)
plot(MoodSleep2015.daily$Date_mood,MoodSleep2015.daily$asleep_mean,type="o",axes=FALSE,bty="n",ylim=newrange,xlab="",ylab="",col="plum")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
lines(MoodSleep2015.daily$Date_mood,MoodSleep2015.daily$inbed_mean,type="o",col="peru")
legend("topright",bty="n",c("Mood","asleep","inbed"),lty=c(1,1),lwd=c(1,1),col=c("blue","plum","peru"))
title("Mood vs Sleep 2015")

#### personal average data ####
# Activity
ncolMA=ncol(MoodAct)
for (isubj in 1:length(unique(MoodAct$userid))){
  subMoodAct=MoodAct[which(MoodAct$userid==unique(MoodAct$userid)[isubj]),]
  subaveragedata=colMeans(subMoodAct[,3:ncolMA])
  subaverage=cbind(subMoodAct[1,1],data.frame(as.list(subaveragedata)))
  names(subaverage)[1]="userid"
  if (isubj==1)
    MoodActAverage=subaverage
  else
    MoodActAverage=rbind(MoodActAverage,subaverage)
}
MoodActModel=lm(mood~TotalSteps+TotalDistance+VeryActiveDistance+ModeratelyActiveDistance+LightActiveDistance+SedentaryActiveDistance+VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+SedentaryMinutes+Calories,
                data=MoodActAverage)
summary(MoodActModel)
# Sleep
ncolMS=ncol(MoodSleep)
for (isubj in 1:length(unique(MoodSleep$userid))){
  subMoodSleep=MoodSleep[which(MoodSleep$userid==unique(MoodSleep$userid)[isubj]),]
  subaveragedata=colMeans(subMoodSleep[,3:ncolMS])
  subaverage=cbind(subMoodSleep[1,1],data.frame(as.list(subaveragedata)))
  names(subaverage)[1]="userid"
  if (isubj==1)
    MoodSleepAverage=subaverage
  else
    MoodSleepAverage=rbind(MoodSleepAverage,subaverage)
}
MoodSleepModel=lm(mood~TotalMinutesAsleep,data=MoodSleepAverage)
summary(MoodSleepModel)
#### consider subject as a random factor ####
# Mood-Sleep
MoodSleep$userid=as.factor(MoodSleep$userid)
MoodSleep$TotalhrAsleep=MoodSleep$TotalMinutesAsleep/60
MoodSleep.Mixed=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleep,REML=FALSE)
summary(MoodSleep.Mixed)
MoodSleep.null=lmer(mood ~ (1|userid),data=MoodSleep,REML=FALSE)
anova(MoodSleep.null,MoodSleep.Mixed)
coef(MoodSleep.Mixed)
MoodSleep.Mixed.randslope=lmer(mood ~ TotalhrAsleep + (1+TotalhrAsleep|userid),data=MoodSleep,REML=FALSE)
MoodSleep.null.randslope=lmer(mood ~ (1+TotalhrAsleep|userid),data=MoodSleep,REML=FALSE)
coef(MoodSleep.Mixed.randslope)
anova(MoodSleep.null.randslope,MoodSleep.Mixed.randslope)
hist(residuals(MoodSleep.Mixed.randslope))
qqnorm(residuals(MoodSleep.Mixed.randslope))
# Mood-Activity
MoodAct$userid=as.factor(MoodAct$userid)
MoodAct$TotalStepslog=log10(MoodAct$TotalSteps)
MoodAct.null=lmer(mood ~ (1|userid),data=MoodAct,REML=FALSE)
MoodAct.Mixed=lmer(mood ~ TotalStepslog + (1|userid),data=MoodAct,REML=FALSE)
summary(MoodAct.Mixed)
anova(MoodAct.null,MoodAct.Mixed)
MoodAct.Mixed.randslope=lmer(mood ~ TotalStepslog + (1+TotalStepslog|userid),data=MoodAct, REML=FALSE)
MoodAct.null.randslope=lmer(mood ~ (1+TotalStepslog|userid),data=MoodAct, REML=FALSE)
summary(MoodAct.Mixed.randslope)
anova(MoodAct.null.randslope,MoodAct.Mixed.randslope)
rcorr(MoodAct$TotalSteps,MoodAct$SedentaryMinutes)
# Mood - Activity & Sleep
MoodActSleep=merge(MoodAct,MoodSleep,by=c("userid","Date_mood","mood"),all=FALSE)
MAS.null=lmer(mood ~ (1|userid),data=MoodActSleep,REML=FALSE)
MAS.act=lmer(mood ~ TotalStepslog + (1|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep=lmer(mood ~ TotalStepslog + TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep.inter=lmer(mood ~ TotalStepslog*TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE)
summary(MAS.actsleep)
anova(MAS.null,MAS.act,MAS.actsleep,MAS.actsleep.inter)
hist(residuals(MAS.actsleep))
qqnorm(residuals(MAS.actsleep))

MAS.null.rs=lmer(mood ~ (1+TotalStepslog|userid),data=MoodActSleep,REML=FALSE)
MAS.act.rs=lmer(mood ~ TotalStepslog + (1+TotalStepslog|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep.rs=lmer(mood ~ TotalStepslog + TotalhrAsleep + (1+TotalStepslog|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep.inter.rs=lmer(mood ~ TotalStepslog*TotalhrAsleep + (1+TotalStepslog|userid),data=MoodActSleep,REML=FALSE)
anova(MAS.null.rs,MAS.act.rs,MAS.actsleep.rs,MAS.actsleep.inter.rs)
hist(residuals(MAS.actsleep.rs))
qqnorm(residuals(MAS.actsleep.rs))

MAS.null.rs=lmer(mood ~ (1+TotalhrAsleep|userid),data=MoodActSleep,REML=FALSE)
MAS.sleep.rs=lmer(mood ~ TotalhrAsleep + (1+TotalhrAsleep|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep.rs=lmer(mood ~ TotalStepslog + TotalhrAsleep + (1+TotalhrAsleep|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep.inter.rs=lmer(mood ~ TotalStepslog*TotalhrAsleep + (1+TotalhrAsleep|userid),data=MoodActSleep,REML=FALSE)
screenreg(MAS.actsleep.rs)
anova(MAS.null.rs,MAS.sleep.rs,MAS.actsleep.rs,MAS.actsleep.inter.rs)
hist(residuals(MAS.actsleep.rs))
qqnorm(residuals(MAS.actsleep.rs))
####################################### PHQ #######################################
PHQ.2014=read.csv('z:/Data Analysis/Yu Fang/data/2014data.csv')
PHQ.BS1.2014=read.csv('z:/Data Analysis/Yu Fang/data/2014BioShort1.csv')
PHQ.BS2.2014=read.csv('z:/Data Analysis/Yu Fang/data/2014BioShort2.csv')
colnames(PHQ.BS1.2014)=paste(colnames(PHQ.BS1.2014),"BS1",sep="_")
colnames(PHQ.BS2.2014)=paste(colnames(PHQ.BS2.2014),"BS2",sep="_")
PHQ.BS.2014=merge(PHQ.2014,PHQ.BS1.2014,by.x="USERID",by.y="USERID_BS1",all.y=TRUE)
PHQ.BS.2014=merge(PHQ.BS.2014,PHQ.BS2.2014,by.x="USERID",by.y="USERID_BS2",all.y=TRUE)
PHQ.2015=read.csv('z:/Data Analysis/Yu Fang/data/2015BLQ1.csv')
PHQ.BS1.2015=read.csv('z:/Data Analysis/Yu Fang/data/2015BioShort1.csv')
PHQ.BS2.2015=read.csv('z:/Data Analysis/Yu Fang/data/2015BioShort2.csv')
colnames(PHQ.BS1.2015)=paste(colnames(PHQ.BS1.2015),"BS1",sep="_")
colnames(PHQ.BS2.2015)=paste(colnames(PHQ.BS2.2015),"BS2",sep="_")
PHQ.BS.2015=merge(PHQ.2015,PHQ.BS1.2015,by.x="USERID",by.y="USERID_BS1",all.y=TRUE)
PHQ.BS.2015=merge(PHQ.BS.2015,PHQ.BS2.2015,by.x="USERID",by.y="USERID_BS2",all.y=TRUE)
PHQ.BS=smartbind(PHQ.BS.2014,PHQ.BS.2015)
PHQ.datecol1=c("PHQdate0","PHQdate1","PHQdate2","PHQdate3","PHQdate4")
PHQ.datecol2=c("PHQdate_BS1","PHQdate_BS2","StartDate_BS1")
for (icol in PHQ.datecol1){PHQ.BS[[icol]]=as.Date(PHQ.BS[[icol]],format='%d%b%y')}
for (icol in PHQ.datecol2){PHQ.BS[[icol]]=as.Date(PHQ.BS[[icol]],format='%m/%d/%Y')}
#### PHQ & Sleep & Activity - is fitbit measure consistent with subjective feelings?####
PHQ.fitbit=PHQ.BS
PHQ.daterange=14 #PHQ measures last two weeks
PHQ.datecols=c("PHQdate0","PHQdate1","PHQdate2","PHQdate3","PHQdate4","PHQdate_BS1","PHQdate_BS2")
PHQ.sleepcols=c("sleep0","sleep1","sleep2","sleep3","sleep4","sleep_BS1","sleep_BS2")
PHQ.stepcols=c("step0","step1","step2","step3","step4","step_BS1","step_BS2")
PHQ.phqcols=c("PHQtot0","PHQtot1","PHQtot2","PHQtot3","PHQtot4","PHQtot_BS1","PHQtot_BS2")
for (icol in PHQ.sleepcols){PHQ.fitbit[[icol]]=NA}
for (icol in PHQ.stepcols){PHQ.fitbit[[icol]]=NA}
for (isub in 1:nrow(PHQ.fitbit)){
  sub=PHQ.fitbit$USERID[isub]
  for (idate in 1:length(PHQ.datecols)){
    datetmp=PHQ.fitbit[[PHQ.datecols[idate]]][isub]
    if (!is.null(datetmp)){
      sleeptmp=Sleep[which(Sleep$Id==sub & datetmp-Sleep$SleepDay<PHQ.daterange & datetmp-Sleep$SleepDay>=0),]
      steptmp=Activity[which(Activity$Id==sub & datetmp-Activity$ActivityDate<PHQ.daterange & datetmp-Activity$ActivityDate>=0),]
      if (nrow(sleeptmp)!=0) {
        PHQ.fitbit[[PHQ.sleepcols[idate]]][isub]=mean(sleeptmp$TotalMinutesAsleep,na.rm = TRUE)
      }
      if (nrow(steptmp)!=0) {
        PHQ.fitbit[[PHQ.stepcols[idate]]][isub]=mean(steptmp$TotalSteps[steptmp$TotalSteps!=0],na.rm=TRUE)
      }
    }
  }
}
# simple association between total PHQ and sleep/step
PHQ.fitbit.subcols=c(PHQ.phqcols,PHQ.sleepcols,PHQ.stepcols)
PHQ.fitbit.sub=PHQ.fitbit[PHQ.fitbit.subcols]
PHQ.fitbit.sub.long=mapply(c,PHQ.fitbit.sub[c(1,8,15)],PHQ.fitbit.sub[c(2,9,16)],PHQ.fitbit.sub[c(3,10,17)],
                           PHQ.fitbit.sub[c(4,11,18)],PHQ.fitbit.sub[c(5,12,19)],PHQ.fitbit.sub[c(6,13,20)],PHQ.fitbit.sub[c(7,14,21)])
PHQ.fitbit.sub.sleep=PHQ.fitbit.sub.long[which(!is.na(PHQ.fitbit.sub.long[,1]) & !is.na(PHQ.fitbit.sub.long[,2])),]
PHQ.fitbit.sub.step=PHQ.fitbit.sub.long[which(!is.na(PHQ.fitbit.sub.long[,1]) & !is.na(PHQ.fitbit.sub.long[,3])),]
PHQ.fitbit.sub.all=PHQ.fitbit.sub.long[which(!is.na(PHQ.fitbit.sub.long[,1]) & !is.na(PHQ.fitbit.sub.long[,2]) & !is.na(PHQ.fitbit.sub.long[,3])),]
PHQ.corr.sleep=rcorr(PHQ.fitbit.sub.sleep[,1],PHQ.fitbit.sub.sleep[,2])
PHQ.corr.step=rcorr(PHQ.fitbit.sub.step[,1],PHQ.fitbit.sub.step[,3])
# linear regression of step and sleep
PHQ.lm=lm(PHQ.fitbit.sub.all[,1]~PHQ.fitbit.sub.all[,2]+PHQ.fitbit.sub.all[,3])
summary(PHQ.lm)
# PHQ-3 sleep vs fitbit sleep (categorical?)
PHQ3subcols=c("asleep_BS1","asleep_BS2","sleep_BS1","sleep_BS2")
PHQ.fitbit.3=PHQ.fitbit[PHQ3subcols]
PHQ.fitbit.3.long=
####################################### Self reported Sleep #######################################
SleepSR=read.csv("Z:././././Data Analysis/Yu Fang/data/data1415sleep_01212016.csv")
SleepSRPHQ=merge(SleepSR,PHQ.BS,by.y=c("USERID","Year"),by.x=c("UserID","Year"))
SleepPHQ=merge(SleepSRPHQ,Sleep,by.x=c("UserID","PHQdate1"),by.y=c("Id","SleepDay"))
SleepPHQ$SleepRatio=SleepPHQ1$sleep24h1/(SleepPHQ1$TotalMinutesAsleep/60)
m1=lm(SleepRatio ~ ,data=SleepPHQ)
summary(m1)
