# Initialization
library("Hmisc")
library("ggplot2")
library("reshape2")
library("lme4")
library("nlme")
library("plyr")
####################################### Functions #######################################
## Sleep
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
  subvalid=sub[which(sub$TotalSteps!=0),]
  subvalid$ActivityDate=as.Date(subvalid$ActivityDate,'%m/%d/%Y')
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
MoodSummary=function(dayfile,suj,allcols,InternStart){
  
}

####################################### StartDate ####################################### 
StartDate2014=read.csv('Z:././././Data Analysis/Yu Fang/data/2014BioShort1.csv')
StartDate2015=read.csv('Z:./././././Data Analysis/Yu Fang/data/2015BioShort1_add2missing.csv')
StartDate2014=StartDate2014[c("USERID","StartDate")]
StartDate2015=StartDate2015[c("USERID","StartDate")];StartDate2015$USERID=as.character(StartDate2015$USERID)
StartDates=rbind(StartDate2014,StartDate2015)

####################################### Sleep #######################################
# day sleep 2014
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
sleep.SubjIDs.2014=unique(Sleep2014$Id)
# day sleep 2015
Sleep2015=read.csv('work//Fitbit//2015_Cohort_all/sleepDay_merged.csv')
sleep.SubjIDs.2015=unique(Sleep2015$Id)

# Summary for each subject 2014
sleep.charcols=c("id");sleep.datecols=c("start","end")
sleep.numcols=c("total","valid",
                "meanMinAsleep","meanMinInbed","meanAsleepInbedratio","sdAsleep","sdInbed","longestAsleep","shortestAsleep","longestInbed","shortestInbed",
                "meanMinAsleep.pre","meanMinInbed.pre","meanAsleepInbedratio.pre","sdAsleep.pre","sdInbed.pre","longestAsleep.pre","shortestAsleep.pre","longestInbed.pre","shortestInbed.pre",
                "meanMinAsleep.post","meanMinInbed.post","meanAsleepInbedratio.post","sdAsleep.post","sdInbed.post","longestAsleep.post","shortestAsleep.post","longestInbed.post","shortestInbed.post")
sleep.allcols=c(sleep.charcols,sleep.datecols,sleep.numcols)
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
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% sleep.SubjIDs.2014[i]],"%m/%d/%Y")
  t=ActivitySummary(Activity2014,activity.SubjIDs.2014[i],activity.allcols,startdate)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.activity.2014=t}
  else{Summary.activity.2014=rbind(Summary.activity.2014,t)}
}
for (i in 1:length(activity.SubjIDs.2015)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% sleep.SubjIDs.2015[i]],"%m/%d/%Y")
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
plot(x,Summary.stephr.mean[1:24],ylim=c(0,1100),type="l",xaxt="n",xlab="hour",ylab="average steps",main="Hourly step pattern before/after Internship starts")
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
legend(20.5,880,bty="n",c("Before","After"),lty=c(1,1),lwd=c(2.5,2.5),col=c("red","green"))

####################################### Mood #######################################
Mood2014=read.csv('Z:/Data Analysis/Yu Fang/data/Mood_2014_all.csv')
MoodAct2014=merge(Mood2014,Activity2014,by.x=c("userid","Date_mood"),by.y=c("Id","ActivityDate"),sort=TRUE)
MoodAct2014=MoodAct2014[which(MoodAct2014$TotalSteps!=0 & !is.na(MoodAct2014$mood)),]
MoodAct2014$Date_mood=as.Date(MoodAct2014$Date_mood,'%m/%d/%Y')
MoodAct2014.daily=data.frame(Date_mood=as.Date(character(),'%Y-%m-%d'),sample_num=integer(),mood_mean=integer(),mood_sd=double(),
                        step_mean=integer(),step_sd=double(),veryactmin_mean=integer(),veryactmin_sd=double(),
                        fairactmin_mean=integer(),fairactmin_sd=double(),lightactmin_mean=integer(),lightactmin_sd=double(),
                        sedmin_mean=integer(),sedmin_sd=double(),cal_mean=integer(),cal_sd=double())
MoodAct2014.Dates=sort(unique(MoodAct2014$Date_mood))
for (idate in 1:length(MoodAct2014.Dates)){
  subdate=MoodAct2014[which(MoodAct2014$Date_mood==MoodAct2014.Dates[idate]),]
  MoodAct2014.daily[idate,"Date_mood"]=MoodAct2014.Dates[idate]
  MoodAct2014.daily[idate,"sample_num"]=nrow(subdate)
  MoodAct2014.daily[idate,"mood_mean"]=mean(subdate$mood)
  MoodAct2014.daily[idate,"mood_sd"]=sd(subdate$mood)
  MoodAct2014.daily[idate,"step_mean"]=mean(subdate$TotalSteps)
  MoodAct2014.daily[idate,"step_sd"]=sd(subdate$TotalSteps)
  MoodAct2014.daily[idate,"veryactmin_mean"]=mean(subdate$VeryActiveMinutes)
  MoodAct2014.daily[idate,"veryactmin_sd"]=sd(subdate$VeryActiveMinutes)
  MoodAct2014.daily[idate,"fairactmin_mean"]=mean(subdate$FairlyActiveMinutes)
  MoodAct2014.daily[idate,"fairactmin_sd"]=sd(subdate$FairlyActiveMinutes)
  MoodAct2014.daily[idate,"lightactmin_mean"]=mean(subdate$LightlyActiveMinutes)
  MoodAct2014.daily[idate,"lightactmin_sd"]=sd(subdate$LightlyActiveMinutes)
  MoodAct2014.daily[idate,"sedmin_mean"]=mean(subdate$SedentaryMinutes)
  MoodAct2014.daily[idate,"sedmin_sd"]=sd(subdate$SedentaryMinutes)
  MoodAct2014.daily[idate,"cal_mean"]=mean(subdate$Calories)
  MoodAct2014.daily[idate,"cal_sd"]=sd(subdate$Calories)
}
#### correlation ####
MoodAct2014.corr.stepvsmood=rcorr(MoodAct2014$mood,MoodAct2014$TotalSteps)
MoodAct2014.corr.veryvsmood=rcorr(MoodAct2014$mood,MoodAct2014$VeryActiveMinutes)
MoodAct2014.corr.fairvsmood=rcorr(MoodAct2014$mood,MoodAct2014$FairlyActiveMinutes)
MoodAct2014.corr.lightvsmood=rcorr(MoodAct2014$mood,MoodAct2014$LightlyActiveMinutes)
MoodAct2014.corr.sedvsmood=rcorr(MoodAct2014$mood,MoodAct2014$SedentaryMinutes)
MoodAct2014.corr.calvsmood=rcorr(MoodAct2014$mood,MoodAct2014$Calories)
#### plot mood/steps ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$step_mean,type="o",axes=FALSE,bty="n",xlab="",ylab="",col="green")
axis(side=4,at=pretty(range(MoodAct2014.daily$step_mean)))
mtext("steps",side=4,line=3)
legend("topright",bty="n",c("Mood","Steps"),lty=c(1,1),lwd=c(2,2),col=c("blue","green"))
#### plot mood/sedentary minutes ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="l",xlab="Date",ylab="Mood",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$sedmin_mean,type="l",axes=FALSE,ylim=newrange,bty="n",xlab="",ylab="",col="black")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
legend("topleft",bty="n",c("Mood","sedentary_minute"),lty=c(1,1),lwd=c(2,2),col=c("blue","black"))
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
#### plot mood/calories ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$mood_mean,type="l",xlab="Date",ylab="Mood",ylim=c(2,10),col="blue")
par(new=TRUE);newrange=c(100,1300)
plot(MoodAct2014.daily$Date_mood,MoodAct2014.daily$cal_mean,type="l",axes=FALSE,bty="n",xlab="",ylab="",col="purple")
axis(side=4,at=pretty(range(MoodAct2014.daily$cal_mean)))
mtext("C",side=4,line=3)
legend("topright",bty="n",c("Mood","Calories"),lty=c(1,1),lwd=c(2,2),col=c("blue","purple"))
###### Mood & Sleep ######
Sleep2014$SleepDate=substr(Sleep2014$SleepDay,1,nchar(as.character(Sleep2014$SleepDay))-12)
MoodSleep2014=merge(Mood2014,Sleep2014,by.x=c("userid","Date_mood"),by.y=c("Id","SleepDate"),sort=TRUE)
MoodSleep2014=MoodSleep2014[which(MoodSleep2014$TotalMinutesAsleep!=0 & !is.na(MoodSleep2014$mood)),]
MoodSleep2014$Date_mood=as.Date(MoodSleep2014$Date_mood,'%m/%d/%Y')
MoodSleep2014$Ratio=MoodSleep2014$TotalMinutesAsleep/MoodSleep2014$TotalTimeInBed
MoodSleep2014.daily=data.frame(Date_mood=as.Date(character(),'%Y-%m-%d'),sample_num=integer(),mood_mean=integer(),mood_sd=double(),
                             asleep_mean=integer(),asleep_sd=double(),inbed_mean=integer(),inbed_sd=double(),
                             ratio_mean=double(),ratio_sd=double())
MoodSleep2014.Dates=sort(unique(MoodSleep2014$Date_mood))
for (idate in 1:length(MoodSleep2014.Dates)){
  subdate=MoodSleep2014[which(MoodSleep2014$Date_mood==MoodSleep2014.Dates[idate]),]
  MoodSleep2014.daily[idate,"Date_mood"]=MoodSleep2014.Dates[idate]
  MoodSleep2014.daily[idate,"sample_num"]=nrow(subdate)
  MoodSleep2014.daily[idate,"mood_mean"]=mean(subdate$mood)
  MoodSleep2014.daily[idate,"mood_sd"]=sd(subdate$mood)
  MoodSleep2014.daily[idate,"asleep_mean"]=mean(subdate$TotalMinutesAsleep)
  MoodSleep2014.daily[idate,"asleep_sd"]=sd(subdate$TotalMinutesAsleep)
  MoodSleep2014.daily[idate,"inbed_mean"]=mean(subdate$TotalTimeInBed)
  MoodSleep2014.daily[idate,"inbed_sd"]=sd(subdate$TotalTimeInBed)
  MoodSleep2014.daily[idate,"ratio_mean"]=mean(subdate$Ratio)
  MoodSleep2014.daily[idate,"ratio_sd"]=sd(subdate$Ratio)
}
#### correlation ####
MoodSleep2014.corr.asleepvsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$TotalMinutesAsleep)
MoodSleep2014.corr.inbedvsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$TotalTimeInBed)
MoodSleep2014.corr.ratiovsmood=rcorr(MoodSleep2014$mood,MoodSleep2014$Ratio)
MoodAct2014.corr.asleepvsinbed=rcorr(MoodSleep2014$TotalMinutesAsleep,MoodSleep2014$TotalTimeInBed)
#### plot mood/sleep ####
par(mar=c(5,4,4,6)+0.1)
plot(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$mood_mean,type="o",xlab="Date",ylab="Mood",ylim=c(0,10),col="blue")
par(new=TRUE);newrange=c(200,1300)
plot(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$asleep_mean,type="o",axes=FALSE,bty="n",ylim=newrange,xlab="",ylab="",col="plum")
axis(side=4,at=pretty(newrange))
mtext("minutes",side=4,line=3)
lines(MoodSleep2014.daily$Date_mood,MoodSleep2014.daily$inbed_mean,type="o",col="peru")
legend("topright",bty="n",c("Mood","asleep","inbed"),lty=c(1,1),lwd=c(1,1),col=c("blue","plum","peru"))
