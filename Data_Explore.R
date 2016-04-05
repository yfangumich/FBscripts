#### Initialization ####
library("Hmisc")
library("ggplot2")
library("foreign")
library("MASS")
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
# multinomial logistic regression
library("nnet") 
library("mlogit")
library("funreg")
library("matrixStats")
library("leaps")
library("xtable")
library("gridExtra")
####################################### Functions #######################################
## Sleep
SleepSummary=function(dayfile,subj,allcols,InternStart){
  subvalid=dayfile[which(dayfile$Id %in% subj),]
  start=subvalid$SleepDay[1]; end=subvalid$SleepDay[nrow(subvalid)]
  total=nrow(subvalid);valid=nrow(subvalid)
  
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
  subvalid=dayfile[which(dayfile$Id %in% subj),]
  start=subvalid$ActivityDate[1];end=subvalid$ActivityDate[nrow(subvalid)]
  total=nrow(subvalid);valid=nrow(subvalid)
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
  
  meanmood.both=mean(subvalid$mood)
    
  subvalid.pre=subvalid[which(subvalid$Date_mood < InternStart),]
  meanmood.pre=mean(subvalid.pre$mood);
  sdmood.pre=sd(subvalid.pre$mood);
  
  subvalid.post=subvalid[which(subvalid$Date_mood >= InternStart),]
  meanmood.post=mean(subvalid.post$mood);
  sdmood.post=sd(subvalid.post$mood);
  
  dmood.model=lm(mood ~ Date_mood, data=subvalid);dmood.slope=coef(dmood.model)[2]
  
  out=data.frame(subj,start,end,total,valid,meanmood.both,
                 meanmood.pre,sdmood.pre,
                 meanmood.post,sdmood.post,dmood.slope)
  colnames(out)=allcols
  return(out)
}
####################################### StartDate ####################################### 
StartDate2014=read.csv('Z:././././Data Analysis/Yu Fang/data/2014BioShort1.csv')
StartDate2015=read.csv('Z:./././././Data Analysis/Yu Fang/data/2015BioShort1.csv')
StartDate2014=StartDate2014[c("USERID","StartDate")]
StartDate2015=StartDate2015[c("USERID","StartDate")];StartDate2015$USERID=as.character(StartDate2015$USERID)
StartDates=rbind(StartDate2014,StartDate2015)
StartDates$StartDate=as.Date(StartDates$StartDate,"%m/%d/%Y")
####################################### Sleep #######################################
# day sleep 2014
Sleep2014=read.csv('work/Fitbit/2014_Cohort_all//sleepDay_merged.csv')
Sleep2014$SleepDay=as.Date(substr(as.character(Sleep2014$SleepDay),1,nchar(as.character(Sleep2014$SleepDay))-4),'%m/%d/%Y')
sleep.SubjIDs.2014=unique(Sleep2014$Id)
# day sleep 2015
Sleep2015=read.csv('work//Fitbit//2015_Cohort_all/sleepDay_merged.csv')
Sleep2015$SleepDay=as.Date(substr(as.character(Sleep2015$SleepDay),1,nchar(as.character(Sleep2015$SleepDay))-12),'%m/%d/%Y')
sleep.SubjIDs.2015=unique(Sleep2015$Id)
# day sleep
Sleep=rbind(Sleep2014,Sleep2015)
Sleep=Sleep[which(Sleep$TotalMinutesAsleep!=0 & Sleep$TotalTimeInBed!=0),]
Sleep.subjIDs=unique(Sleep$Id)
Sleep=merge(Sleep,StartDates,by.x="Id",by.y="USERID",all.x=TRUE)
Sleep$day=as.numeric(Sleep$SleepDay-Sleep$StartDate)
Sleep$TotalHrAsleep=Sleep$TotalMinutesAsleep / 60
Sleep$TotalHrInBed=Sleep$TotalTimeInBed / 60
Sleep$Efficiency=Sleep$TotalMinutesAsleep / Sleep$TotalTimeInBed
###### Plot Sleep as a function of time ######
SleepDay=aggregate(Efficiency~day,data=Sleep,mean)
SleepDaySD=aggregate(Efficiency~day,data=Sleep,sd)
SleepDaycount=as.data.frame(table(Sleep$day))
SleepDay=merge(SleepDay,SleepDaySD,by="day")
SleepDay=merge(SleepDay,SleepDaycount,by.x="day",by.y="Var1")
names(SleepDay)=c("day","Efficiency","sd","n")
SleepDay[,"se"]=0
SleepDay$se[which(!is.na(SleepDay$sd))]=qt(0.975,df=SleepDay$n[which(!is.na(SleepDay$sd))]-1)*
  SleepDay$sd[which(!is.na(SleepDay$sd))]/sqrt(SleepDay$n[which(!is.na(SleepDay$sd))])
SleepDay$upper=SleepDay$Efficiency+SleepDay$se
SleepDay$lower=SleepDay$Efficiency-SleepDay$se
SleepDay$sd[which(is.na(SleepDay$sd))]=0
# start plotting
par(mar=c(5,12,4,4)+0.1)
plot(SleepDay$day,SleepDay$Efficiency,xlab="Day(0=InternshipStart)",ylab="Sleep Efficiency",col="red",cex.lab=1.5,cex.axis=1.5)
loess_fit=loess(Efficiency~day,data=SleepDay,span=0.8)
lines(SleepDay$day,predict(loess_fit),col="blue");abline(v=0)
loess_fitu=loess(upper~day,data=SleepDay,span=0.8)
loess_fitl=loess(lower~day,data=SleepDay,span=0.8)
lines(SleepDay$day,predict(loess_fitu),lty='dashed',col='black')
lines(SleepDay$day,predict(loess_fitl),lty='dashed',col='black')
polygon(c(rev(SleepDay$day), SleepDay$day), 
        c(rev(predict(loess_fitu)),predict(loess_fitl)), 
        col = adjustcolor('grey80',alpha.f=0.2), border = NA)
par(new=T)
with(SleepDay,plot(day,n,xlab="",ylab="",axes=F,type="n"))
loess_fitn=loess(n~day,data=SleepDay,span=0.8)
lines(SleepDay$day,predict(loess_fitn),col="black")
par(new=T)
with(SleepDay,plot(day,sd,xlab="",ylab="",axes=F,type="n"));axis(side=4)
mtext(side=4,line=3,"standard deviation")
loess_fitsd=loess(sd~day,data=SleepDay,span=0.8)
lines(SleepDay$day,predict(loess_fitsd),col="green")
legend(70,250,c("mean","sd","nsub"),lty=c(1,1,1),lwd=c(2,2,2),col=c("blue","green","black"))
# end plotting
# Summary for each subject
sleep.charcols=c("id");sleep.datecols=c("start","end")
sleep.numcols=c("total","valid",
                "meanMinAsleep","meanMinInbed","meanAsleepInbedratio","sdAsleep","sdInbed","longestAsleep","shortestAsleep","longestInbed","shortestInbed",
                "meanMinAsleep.pre","meanMinInbed.pre","meanAsleepInbedratio.pre","sdAsleep.pre","sdInbed.pre","longestAsleep.pre","shortestAsleep.pre","longestInbed.pre","shortestInbed.pre",
                "meanMinAsleep.post","meanMinInbed.post","meanAsleepInbedratio.post","sdAsleep.post","sdInbed.post","longestAsleep.post","shortestAsleep.post","longestInbed.post","shortestInbed.post")
sleep.allcols=c(sleep.charcols,sleep.datecols,sleep.numcols)
# needs to combine the below two parts
for (i in 1:length(Sleep.subjIDs)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% Sleep.subjIDs[i]],"%m/%d/%Y")
  t=SleepSummary(Sleep,Sleep.subjIDs[i],sleep.allcols,startdate)
  for (tcol in sleep.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in sleep.numcols){t[[tcol]]=as.numeric(t[[tcol]])}
  if (i==1){Summary.sleep=t}
  else{Summary.sleep=rbind(Summary.sleep,t)} 
}
# exclude subject with less than x day's record (x=sleep.recordday)
sleep.recordday=10
Summary.sleep=Summary.sleep[which(Summary.sleep$valid>sleep.recordday),]
Summary.sleep.2014=Summary.sleep[which(Summary.sleep$end<"2015-01-01"),]
Summary.sleep.2015=Summary.sleep[which(Summary.sleep$end>"2015-01-01"),]
# paired ttest of before and after
sleep.model.paired.meanasleep.2014=t.test(Summary.sleep.2014$meanMinAsleep.pre,Summary.sleep.2014$meanMinAsleep.post,paired=T);sleep.model.paired.meanasleep.2014
sleep.model.paired.meanasleep.2015=t.test(Summary.sleep.2015$meanMinAsleep.pre,Summary.sleep.2015$meanMinAsleep.post,paired=T);sleep.model.paired.meanasleep.2015
sleep.model.paired.meanasleep=t.test(Summary.sleep$meanMinAsleep.pre,Summary.sleep$meanMinAsleep.post,paired=T);sleep.model.paired.meanasleep
sleep.model.paired.meaninbed.2014=t.test(Summary.sleep.2014$meanMinInbed.pre,Summary.sleep.2014$meanMinInbed.post,paired=T);sleep.model.paired.meaninbed.2014
sleep.model.paired.meaninbed.2015=t.test(Summary.sleep.2015$meanMinInbed.pre,Summary.sleep.2015$meanMinInbed.post,paired=T);sleep.model.paired.meaninbed.2015
sleep.model.paired.meaninbed=t.test(Summary.sleep$meanMinInbed.pre,Summary.sleep$meanMinInbed.post,paired=T);sleep.model.paired.meaninbed
sleep.model.paired.meanratio.2014=t.test(Summary.sleep.2014$meanAsleepInbedratio.pre,Summary.sleep.2014$meanAsleepInbedratio.post,paired=T);sleep.model.paired.meanratio.2014
sleep.model.paired.meanratio.2015=t.test(Summary.sleep.2015$meanAsleepInbedratio.pre,Summary.sleep.2015$meanAsleepInbedratio.post,paired=T);sleep.model.paired.meanratio.2015
sleep.model.paired.meanratio=t.test(Summary.sleep$meanAsleepInbedratio.pre,Summary.sleep$meanAsleepInbedratio.post,paired=T);sleep.model.paired.meanratio
####################################### Activity #######################################
#### daily activity ####
Activity2014=read.csv('work/Fitbit/2014_Cohort_all/dailyActivity_merged.csv')
Activity2015=read.csv('work//Fitbit//2015_Cohort_all//dailyActivity_merged.csv')
activity.SubjIDs.2014=unique(Activity2014$Id);activity.SubjIDs.2015=unique(Activity2015$Id);
Activity2014$ActivityDate=as.Date(Activity2014$ActivityDate,'%m/%d/%Y')
Activity2015$ActivityDate=as.Date(Activity2015$ActivityDate,'%m/%d/%Y')
Activity=rbind(Activity2014,Activity2015)
Activity=Activity[which(Activity$TotalSteps!=0),]
Activity=merge(Activity,StartDates,by.x="Id",by.y="USERID",all.x=TRUE)
Activity$day=as.numeric(Activity$ActivityDate-Activity$StartDate)
Activity$thousandSteps=Activity$TotalSteps / 1000
activity.SubjIDs=unique(Activity$Id)
###### Plot Activity as a function of time ######
ActDay=aggregate(TotalSteps~day,data=Activity,mean)
ActDaySD=aggregate(TotalSteps~day,data=Activity,sd)
ActDaycount=as.data.frame(table(Activity$day))
ActDay=merge(ActDay,ActDaySD,by="day")
ActDay=merge(ActDay,ActDaycount,by.x="day",by.y="Var1")
names(ActDay)=c("day","TotalSteps","sd","n")
ActDay[,"se"]=0
ActDay$se[which(!is.na(ActDay$sd))]=qt(0.975,df=ActDay$n[which(!is.na(ActDay$sd))]-1)*
  ActDay$sd[which(!is.na(ActDay$sd))]/sqrt(ActDay$n[which(!is.na(ActDay$sd))])
ActDay$upper=ActDay$TotalSteps+ActDay$se
ActDay$lower=ActDay$TotalSteps-ActDay$se
ActDay$sd[which(is.na(ActDay$sd))]=0
# start plotting
par(mar=c(5,12,4,4)+0.1)
plot(ActDay$day,ActDay$TotalSteps,xlab="Day(0=InternshipStart)",ylab="TotalSteps",col="red",cex.lab=1.5,cex.axis=1.5)
loess_fit=loess(TotalSteps~day,data=ActDay,span=0.8)
lines(ActDay$day,predict(loess_fit),col="blue")
abline(v=0)
loess_fitu=loess(upper~day,data=ActDay,span=0.8)
loess_fitl=loess(lower~day,data=ActDay,span=0.8)
lines(ActDay$day,predict(loess_fitu),lty='dashed',col='black')
lines(ActDay$day,predict(loess_fitl),lty='dashed',col='black')
polygon(c(rev(ActDay$day), ActDay$day), 
        c(rev(predict(loess_fitu)),predict(loess_fitl)), 
        col = adjustcolor('grey80',alpha.f=0.2), border = NA)
par(new=T)
with(ActDay,plot(day,n,xlab="",ylab="",axes=F,type="n"))
loess_fitn=loess(n~day,data=ActDay,span=0.8)
lines(ActDay$day,predict(loess_fitn),col="black")
par(new=T)
with(ActDay,plot(day,sd,xlab="",ylab="",axes=F,type="n"));axis(side=4)
mtext(side=4,line=3,"standard deviation")
loess_fitsd=loess(sd~day,data=ActDay,span=0.8)
lines(ActDay$day,predict(loess_fitsd),col="green")
legend(70,2000,c("mean","sd","nsub"),lty=c(1,1,1),lwd=c(2,2,2),col=c("blue","green","black"))
# end plotting
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
for (i in 1:length(activity.SubjIDs)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% activity.SubjIDs[i]],"%m/%d/%Y")
  t=ActivitySummary(Activity,activity.SubjIDs[i],activity.allcols,startdate)
  for (tcol in activity.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in activity.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.activity=t}
  else{Summary.activity=rbind(Summary.activity,t)}
}
#### exclude subject with less than x day's record (x=sleep.recordday) ####
activity.recordday=10
Summary.activity=Summary.activity[which(Summary.activity$valid>activity.recordday),]
Summary.activity.2014=Summary.activity[which(Summary.activity$end<"2015-01-01"),]
Summary.activity.2015=Summary.activity[which(Summary.activity$end>"2015-01-01"),]
#### paired ttest of steps/calories/distance before and after ####
activity.model.paired.meanstep.2014=t.test(Summary.activity.2014$meanSteps.pre,Summary.activity.2014$meanSteps.post,paired=T);activity.model.paired.meanstep.2014
activity.model.paired.meanstep.2015=t.test(Summary.activity.2015$meanSteps.pre,Summary.activity.2015$meanSteps.post,paired=T);activity.model.paired.meanstep.2015
activity.model.paired.meanstep=t.test(Summary.activity$meanSteps.pre,Summary.activity$meanSteps.post,paired=T);activity.model.paired.meanstep

activity.model.paired.meancal.2014=t.test(Summary.activity.2014$meancalories.pre,Summary.activity.2014$meancalories.post,paired=T);activity.model.paired.meancal.2014
activity.model.paired.meancal.2015=t.test(Summary.activity.2015$meancalories.pre,Summary.activity.2015$meancalories.post,paired=T);activity.model.paired.meancal.2015
activity.model.paired.meancal=t.test(Summary.activity$meancalories.pre,Summary.activity$meancalories.post,paired=T);activity.model.paired.meancal

activity.model.paired.meandist.2014=t.test(Summary.activity.2014$meandistance.pre,Summary.activity.2014$meandistance.post,paired=T);activity.model.paired.meandist.2014
activity.model.paired.meandist.2015=t.test(Summary.activity.2015$meandistance.pre,Summary.activity.2015$meandistance.post,paired=T);activity.model.paired.meandist.2015
activity.model.paired.meandist=t.test(Summary.activity$meandistance.pre,Summary.activity$meandistance.post,paired=T);activity.model.paired.meandist

activity.model.paired.meanveryactmin.2014=t.test(Summary.activity.2014$meanveryactmin.pre,Summary.activity.2014$meanveryactmin.post,paired=T);activity.model.paired.meanveryactmin.2014
activity.model.paired.meanveryactmin.2015=t.test(Summary.activity.2015$meanveryactmin.pre,Summary.activity.2015$meanveryactmin.post,paired=T);activity.model.paired.meanveryactmin.2015
activity.model.paired.meanveryactmin=t.test(Summary.activity$meanveryactmin.pre,Summary.activity$meanveryactmin.post,paired=T);activity.model.paired.meanveryactmin

activity.model.paired.meanfairactmin.2014=t.test(Summary.activity.2014$meanfairactmin.pre,Summary.activity.2014$meanfairactmin.post,paired=T);activity.model.paired.meanfairactmin.2014
activity.model.paired.meanfairactmin.2015=t.test(Summary.activity.2015$meanfairactmin.pre,Summary.activity.2015$meanfairactmin.post,paired=T);activity.model.paired.meanfairactmin.2015
activity.model.paired.meanfairactmin=t.test(Summary.activity$meanfairactmin.pre,Summary.activity$meanfairactmin.post,paired=T);activity.model.paired.meanfairactmin

activity.model.paired.meanlightactmin.2014=t.test(Summary.activity.2014$meanlightactmin.pre,Summary.activity.2014$meanlightactmin.post,paired=T);activity.model.paired.meanlightactmin.2014
activity.model.paired.meanlightactmin.2015=t.test(Summary.activity.2015$meanlightactmin.pre,Summary.activity.2015$meanlightactmin.post,paired=T);activity.model.paired.meanlightactmin.2015
activity.model.paired.meanlightactmin=t.test(Summary.activity$meanlightactmin.pre,Summary.activity$meanlightactmin.post,paired=T);activity.model.paired.meanlightactmin

activity.model.paired.meansedmin.2014=t.test(Summary.activity.2014$meansedmin.pre,Summary.activity.2014$meansedmin.post,paired=T);activity.model.paired.meansedmin.2014
activity.model.paired.meansedmin.2015=t.test(Summary.activity.2015$meansedmin.pre,Summary.activity.2015$meansedmin.post,paired=T);activity.model.paired.meansedmin.2015
activity.model.paired.meansedmin=t.test(Summary.activity$meansedmin.pre,Summary.activity$meansedmin.post,paired=T);activity.model.paired.meansedmin
##### correlations of steps vs. calories/distance ####
activity.corr.stepvscal=rcorr(Summary.activity$meanSteps,Summary.activity$meancalories)
activity.corr.stepvsdist=rcorr(Summary.activity$meanSteps,Summary.activity$meandistance)
Summary.activity.variables = Summary.activity[c("meanSteps","meancalories","meandistance","meanveryactmin","meanfairactmin","meanlightactmin","meansedmin")]
Summary.activity.variables.rcorr=rcorr(as.matrix(Summary.activity.variables))
write.table(data.frame(Summary.activity.variables.rcorr$P),"work/Fitbit/FBscripts/tmp.csv",sep=",")
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
Stephr2014$ActivityHour=as.character(as.POSIXct(strptime(as.character(Stephr2014$ActivityHour),"%m/%d/%Y %H:%M")))
Stephr2015=read.csv('work/Fitbit//2015_Cohort_all/hourlySteps_merged.csv');Stephr2015$Id=as.character(Stephr2015$Id)
Stephr2015$ActivityHour=as.character(as.POSIXct(strptime(as.character(Stephr2015$ActivityHour),"%m/%d/%Y %I:%M:%S %p")))
Stephr=rbind(Stephr2014,Stephr2015)
Stephr$ActivityDate=substr(Stephr$ActivityHour,1,10)
Stephr$ActivityHour=substr(Stephr$ActivityHour,12,end)

activity.SubjIDs=unique(Stephr$Id)
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
Summary.stephr.backup=Summary.stephr
Summary.stephr=Summary.stephr.backup
Summary.stephr=Summary.stephr[which(Summary.stephr$id %in% activity.SubjIDs.2014),]
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
mood.numcols=c("total","valid","meanMood","meanMood.pre","sdMood.pre","meanMood.post","sdMood.post","dmood.slope")
mood.allcols=c(mood.charcols,mood.datecols,mood.numcols)
mood.SubjIDs=unique(Mood$userid)
Mood.totaldays2014=as.numeric(max(Mood$Date_mood[which(Mood$Date_mood<"2015-01-01")])+1-min(Mood$Date_mood[which(Mood$Date_mood<"2015-01-01")])+1)
Mood.totaldays2015=as.numeric(max(Mood$Date_mood[which(Mood$Date_mood>="2015-01-01")])-min(Mood$Date_mood[which(Mood$Date_mood>="2015-01-01")])+1)
for (i in 1:length(mood.SubjIDs)){
  startdate=as.Date(StartDates$StartDate[StartDates$USERID %in% mood.SubjIDs[i]],"%m/%d/%Y")
  t=MoodSummary(Mood,mood.SubjIDs[i],mood.allcols,startdate)
  for (tcol in mood.charcols){t[[tcol]]=as.character(t[[tcol]])}
  for (tcol in mood.numcols){t[[tcol]]=as.numeric(t[[tcol]])  }
  if (i==1){Summary.mood=t}
  else{Summary.mood=rbind(Summary.mood,t)}
}
Summary.mood=Summary.mood[which(!is.na(Summary.mood$meanMood.pre)),]
Summary.mood$RR<-NA
Summary.mood$RR[which(Summary.mood$end<"2015-01-01")]<-Summary.mood$valid[which(Summary.mood$end<"2015-01-01")] / Mood.totaldays2014;
Summary.mood$RR[which(Summary.mood$end>="2015-01-01")]<-Summary.mood$valid[which(Summary.mood$end>="2015-01-01")] / Mood.totaldays2015;
Summary.mood$dmeanmood <- Summary.mood$meanMood.post-Summary.mood$meanMood.pre

m1<-lm(meanMood ~ RR, data=Summary.mood);summary(m1)
m2<-lm(meanMood.pre ~ RR, data=Summary.mood);summary(m2)
m3<-lm(meanMood.post ~ RR, data=Summary.mood);summary(m3)
m4<-lm(dmeanmood ~ RR, data=Summary.mood);summary(m4)
m5<-lm(dmood.slope ~ RR, data=Summary.mood);summary(m5)

mood.model.paired=t.test(Summary.mood$meanMood.pre,Summary.mood$meanMood.post,paired=T)
Summary.mood.2014=Summary.mood[which(Summary.mood$end < as.Date("2015-01-01")),]
mood.model.paired.2014=t.test(Summary.mood.2014$meanMood.pre,Summary.mood.2014$meanMood.post,paired=T)
Summary.mood.2015=Summary.mood[which(Summary.mood$end > as.Date("2015-01-01")),]
mood.model.paired.2015=t.test(Summary.mood.2015$meanMood.pre,Summary.mood.2015$meanMood.post,paired=T)
############## Add Control phenotypes to Mood ##############
tmpMood=merge(Mood,PHQ[c("UserID","Age","Sex","Ethnicity","Marital","Child")],by.x="userid",by.y="UserID",all.x=TRUE)
Mood=tmpMood
############## Add Time stamps ##############
Mood=merge(Mood,StartDates,by.x="userid",by.y="USERID")
Mood$day=as.integer(Mood$Date_mood-Mood$StartDate)
###### Plot mood as a function of time ######
MoodDay=aggregate(mood~day,data=Mood,mean)
MoodDaySD=aggregate(mood~day,data=Mood,sd)
MoodDaycount=as.data.frame(table(Mood$day))
MoodDay=merge(MoodDay,MoodDaySD,by="day")
MoodDay=merge(MoodDay,MoodDaycount,by.x="day",by.y="Var1")
names(MoodDay)=c("day","mood","sd","n")
MoodDay[,"se"]=0
MoodDay$se[which(!is.na(MoodDay$sd))]=qt(0.975,df=MoodDay$n[which(!is.na(MoodDay$sd))]-1)*
  MoodDay$sd[which(!is.na(MoodDay$sd))]/sqrt(MoodDay$n[which(!is.na(MoodDay$sd))])
MoodDay$sd[which(is.na(MoodDay$sd))]=0
MoodDay$upper=MoodDay$mood+MoodDay$se
MoodDay$lower=MoodDay$mood-MoodDay$se
# start plotting
par(mar=c(5,12,4,4)+0.1)
with(MoodDay,plot(day,mood,xlab="Day(0=InternshipStart)",ylab="Mood",col="red",cex.lab=1.5,cex.axis=1.5))
loess_fit=loess(mood~day,data=MoodDay,span=0.8)
lines(MoodDay$day,predict(loess_fit),col="blue")
abline(v=0)
loess_fitu=loess(upper~day,data=MoodDay,span=0.8)
loess_fitl=loess(lower~day,data=MoodDay,span=0.8)
lines(MoodDay$day,predict(loess_fitu),lty='dashed',col='black')
lines(MoodDay$day,predict(loess_fitl),lty='dashed',col='black')
polygon(c(rev(MoodDay$day), MoodDay$day), 
        c(rev(predict(loess_fitu)),predict(loess_fitl)), 
        col = adjustcolor('grey80',alpha.f=0.2), border = NA)
par(new=T)
with(MoodDay,plot(day,n,xlab="",ylab="",axes=F,type="n"))
loess_fitn=loess(n~day,data=MoodDay,span=0.8)
lines(MoodDay$day,predict(loess_fitn),col="black")
par(new=T)
with(MoodDay,plot(day,sd,xlab="",ylab="",axes=F,type="n"));axis(side=4)
mtext(side=4,line=3,"standard deviation")
loess_fitsd=loess(sd~day,data=MoodDay,span=0.8)
lines(MoodDay$day,predict(loess_fitsd),col="green")
legend(20,2.9,c("mean","sd","nsub"),lty=c(1,1,1),lwd=c(2,2,2),col=c("blue","green","black"))
par(mfrow=c(3,1))
with(MoodDay,plot(day,n,main="mood"))
with(SleepDay,plot(day,n,main="sleep"))
with(ActDay,plot(day,n,main="activity"))
par(mfrow=c(3,1))
with(MoodDay,plot(day,sd,main="mood"))
with(SleepDay,plot(day,sd,main="sleep"))
with(ActDay,plot(day,sd,main="activity"))
# end plotting
###### Mood & Activity ######
MoodAct=merge(Mood,Activity,by.x=c("userid","Date_mood"),by.y=c("Id","ActivityDate"),sort=TRUE)
MoodAct2014=MoodAct[which(MoodAct$Date_mood < as.Date("2015-01-01")),]
MoodAct2015=MoodAct[which(MoodAct$Date_mood > as.Date("2015-01-01")),]
MoodAct$userid=as.factor(MoodAct$userid)
MoodAct$TotalStepslog=log10(MoodAct$TotalSteps)
#### Aggregate to every day ####
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
############## Mood & Sleep ##############
MoodSleep=merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","SleepDay"),sort=TRUE)
MoodSleep=MoodSleep[which(MoodSleep$TotalMinutesAsleep!=0 & !is.na(MoodSleep$mood)),]
MoodSleep$Ratio=MoodSleep$TotalMinutesAsleep/MoodSleep$TotalTimeInBed
MoodSleep$userid=as.factor(MoodSleep$userid)
MoodSleep$TotalhrAsleep=MoodSleep$TotalMinutesAsleep/60
#### Aggregate to every day ####
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
######## personal average data ########
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
MoodActModel=lm(mood~TotalSteps,data=MoodActAverage)
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
######## consider subject as a random factor ########
#### Mood-Sleep ####
boxplot(mood ~ userid,MoodSleep,xlab="Subject",ylab="Mood")
boxplot(mood ~ TotalhrAsleepInt,MoodSleep)
boxplot(mood ~ Sex, MoodSleep,xlab="Gender",ylab="Mood")
boxplot(mood ~ Age, MoodSleep,xlab="Age",ylab="Mood")
boxplot(mood ~ Age * Sex,MoodSleep)
MoodSleep.null=lmer(mood ~ (1|userid),data=MoodSleep,REML=FALSE)
MoodSleep.null.AG=lmer(mood ~ Age + Sex + (1|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.null.AG)
MoodSleep.Mixed=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.Mixed)
MoodSleep.Mixed.AG=lmer(mood ~ Age + Sex + TotalhrAsleep + (1|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.Mixed.AG)
anova(MoodSleep.null,MoodSleep.Mixed)
anova(MoodSleep.null.AG,MoodSleep.Mixed.AG)
MoodSleep.Mixed.randslope=lmer(mood ~ TotalhrAsleep + (1+TotalhrAsleep|userid),data=MoodSleep,REML=FALSE)
MoodSleep.null.randslope=lmer(mood ~ (1+TotalhrAsleep|userid),data=MoodSleep,REML=FALSE)
coef(MoodSleep.Mixed.randslope)
anova(MoodSleep.null.randslope,MoodSleep.Mixed.randslope)
hist(residuals(MoodSleep.Mixed.randslope))
qqnorm(residuals(MoodSleep.Mixed.randslope))
##### Mood - Sleep efficiency ####
MoodSleep.effmixed=lmer(mood ~ Ratio + (1|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.effmixed)
anova(MoodSleep.null,MoodSleep.effmixed)
MoodSleep.effmixed2=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.effmixed2)
MoodSleep.effmixed2rs.null=lmer(mood ~ TotalhrAsleep  + (1+Ratio|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.effmixed2rs.null)
MoodSleep.effmixed2rs=lmer(mood ~ TotalhrAsleep + Ratio + (1+Ratio|userid),data=MoodSleep,REML=FALSE);summary(MoodSleep.effmixed2rs)
anova(MoodSleep.null,MoodSleep.Mixed,MoodSleep.effmixed2)
anova(MoodSleep.effmixed2rs.null,MoodSleep.effmixed2rs)
##### Mood-Activity ####
MoodAct.null=lmer(mood ~ (1|userid),data=MoodAct,REML=FALSE)
MoodAct.Mixed=lmer(mood ~ TotalStepslog + (1|userid),data=MoodAct,REML=FALSE)
summary(MoodAct.Mixed)
anova(MoodAct.null,MoodAct.Mixed)
MoodAct.Mixed.randslope=lmer(mood ~ TotalStepslog + (1+TotalStepslog|userid),data=MoodAct, REML=FALSE)
MoodAct.null.randslope=lmer(mood ~ (1+TotalStepslog|userid),data=MoodAct, REML=FALSE)
summary(MoodAct.Mixed.randslope)
anova(MoodAct.null.randslope,MoodAct.Mixed.randslope)
rcorr(MoodAct$TotalSteps,MoodAct$SedentaryMinutes)
#### Mood - Activity & Sleep ####
mergecols=c("userid","Date_mood","mood","Age","Sex","Ethnicity","Marital","Child","day","StartDate")
MoodActSleep=merge(MoodAct,MoodSleep,by=mergecols,all=FALSE)
MoodActSleep=MoodActSleep[which(!is.na(MoodActSleep$Age) & !is.na(MoodActSleep$Sex)),]

MASrsc<-MoodActSleep
MASrsc[,c("day")]<-scale(MASrsc[,c("day")])
MASrsc$daysquare=MASrsc$day^2
MASrsc=MASrsc[which(!is.na(MASrsc$Age) & !is.na(MASrsc$Sex)),]
factorcols=c("Sex","Ethnicity","Marital","Child")
MASrsc[,factorcols]=apply(MASrsc[,factorcols],2,function(x) as.factor(x))
MAS.null=lmer(mood ~ (1|userid),data=MASrsc,REML=FALSE)
MAS.fb=lmer(mood ~TotalStepslog + TotalhrAsleep + day + daysquare + (1+day|userid),data=MASrsc,REML=FALSE)
summary(MAS.fb)
MAS.full=lmer(mood ~ TotalStepslog + TotalhrAsleep + day + daysquare + (1+day|userid) + Age + Sex + Ethnicity + Marital + Child,data=MASrsc,REML=FALSE)
summary(MAS.full)
sjt.lmer(MAS.fb,MAS.full,showHeaderStrings=TRUE,stringB="Estimate",
         stringDependentVariables="Response",labelDependentVariables=c("model.fitbit","model.full"),
         labelPredictors=c("log10(totalsteps)","totalsleephr","day","day^2","Age","sex.female",
                           "ethnicity.Asian","ethnicity.Mixed","Marital.engaged","Marital.married","child.no"),
         separateConfColumn=FALSE, showStdBeta=TRUE,pvaluesAsNumbers=FALSE)
MAS.aov<-anova(MAS.null,MAS.fb,MAS.full)
capture.output(MAS.aov,file="work/Fitbit/FBscripts/tmp.doc")

MAS.null=lmer(mood ~ (1|userid),data=MoodActSleep,REML=FALSE)
MAS.act=lmer(mood ~ TotalStepslog + (1|userid),data=MoodActSleep,REML=FALSE)
MAS.sleep=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE)
MAS.actsleep=lmer(mood ~ TotalStepslog + TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.actsleep)
MAS.actsleep.inter=lmer(mood ~ TotalStepslog*TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.actsleep.inter)
anova(MAS.act,MAS.actsleep)
anova(MAS.sleep,MAS.actsleep)
anova(MAS.null,MAS.act,MAS.actsleep,MAS.actsleep.inter)
hist(residuals(MAS.actsleep));qqnorm(residuals(MAS.actsleep))

MAS.null.AG=lmer(mood ~ Age + Sex  + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.null.AG)
MAS.act.AG=lmer(mood ~ Age  + Sex  + TotalStepslog + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.act.AG)
MAS.sleep.AG=lmer(mood ~ Age + Sex + TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.sleep.AG)
MAS.actsleep.AG=lmer(mood ~ Age + Sex + TotalStepslog + TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.actsleep.AG)
MAS.actsleep.inter.AG=lmer(mood ~ Age + Sex + TotalStepslog*TotalhrAsleep + (1|userid),data=MoodActSleep,REML=FALSE);summary(MAS.actsleep.inter.AG)
anova(MAS.null.AG,MAS.act.AG,MAS.actsleep.AG,MAS.actsleep.inter.AG)
anova(MAS.act.AG,MAS.actsleep.AG)
anova(MAS.sleep.AG,MAS.actsleep.AG)
hist(residuals(MAS.actsleep));qqnorm(residuals(MAS.actsleep))

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
anova(MAS.null.rs,MAS.sleep.rs,MAS.actsleep.rs,MAS.actsleep.inter.rs)
hist(residuals(MAS.actsleep.rs))
qqnorm(residuals(MAS.actsleep.rs))

# Mood - Activity & Sleep - before/after
MoodActSleepIntern <- merge(MoodActSleep,StartDates,by.x="userid",by.y="USERID")
MoodActSleepIntern$StartDate <- as.Date(MoodActSleepIntern$StartDate,format='%m/%d/%Y')
MoodActSleepIntern$AfterIntern <- (MoodActSleepIntern$Date_mood >= MoodActSleepIntern$StartDate)
MASIbefore <- MoodActSleepIntern[which(MoodActSleepIntern$AfterIntern==FALSE),]
MASIafter <- MoodActSleepIntern[which(MoodActSleepIntern$AfterIntern==TRUE),]

MASIb.null <- lmer(mood ~ (1|userid),data=MASIbefore,REML=FALSE)
MASIb.sleep <- lmer(mood ~ TotalhrAsleep + (1|userid),data=MASIbefore, REML=FALSE );summary(MASIb.sleep)
MASIb.sleep.rs <- lmer(mood ~ TotalhrAsleep + (1+TotalhrAsleep|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleep.rs)
MASIb.act <- lmer(mood ~ TotalStepslog + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.act)
MASIb.act.rs <- lmer(mood ~ TotalStepslog + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.act.rs)
MASIb.sleepact <- lmer(mood ~ TotalStepslog + TotalhrAsleep + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact)
MASIb.sleepact.rs <- lmer(mood ~ TotalStepslog + TotalhrAsleep + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.rs)
MASIb.sleepact.inter <- lmer(mood ~ TotalStepslog * TotalhrAsleep + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.inter)
MASIb.sleepact.inter.rs <- lmer(mood ~ TotalStepslog * TotalhrAsleep + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.inter.rs)
anova(MASIb.sleep,MASIb.sleepact)
anova(MASIb.act,MASIb.sleepact)
anova(MASIb.null,MASIb.sleepact,MASIb.sleepact.inter)
anova(MASIb.sleep,MASIb.sleep.rs)
anova(MASIb.act,MASIb.act.rs)
anova(MASIb.null,MASIb.act.rs,MASIb.sleepact.rs,MASIb.sleepact.inter.rs)

MASIb.null.AG <- lmer(mood ~ Age+Sex+(1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.null.AG)
MASIb.sleep.AG <- lmer(mood ~ Age+Sex+TotalhrAsleep + (1|userid),data=MASIbefore, REML=FALSE );summary(MASIb.sleep.AG)
MASIb.sleep.rs.AG <- lmer(mood ~ Age+Sex+TotalhrAsleep + (1+TotalhrAsleep|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleep.rs.AG)
MASIb.act.AG <- lmer(mood ~ Age+Sex+TotalStepslog + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.act.AG)
MASIb.act.rs.AG <- lmer(mood ~ Age+Sex+TotalStepslog + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.act.rs.AG)
MASIb.sleepact.AG <- lmer(mood ~ Age+Sex+TotalStepslog + TotalhrAsleep + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.AG)
MASIb.sleepact.rs.AG <- lmer(mood ~ Age+Sex+TotalStepslog + TotalhrAsleep + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.rs.AG)
MASIb.sleepact.inter.AG <- lmer(mood ~ Age+Sex+TotalStepslog * TotalhrAsleep + (1|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.inter.AG)
MASIb.sleepact.inter.rs.AG <- lmer(mood ~ Age+Sex+TotalStepslog * TotalhrAsleep + (1+TotalStepslog|userid),data=MASIbefore,REML=FALSE);summary(MASIb.sleepact.inter.rs.AG)
anova(MASIb.sleep.AG,MASIb.sleepact.AG)
anova(MASIb.act.AG,MASIb.sleepact.AG)
anova(MASIb.null.AG,MASIb.sleepact.AG,MASIb.sleepact.inter.AG)

MASIa.null <- lmer(mood ~ (1|userid),data=MASIafter,REML=FALSE)
MASIa.sleep <- lmer(mood ~ TotalhrAsleep + (1|userid),data=MASIafter, REML=FALSE );summary(MASIa.sleep)
MASIa.sleep.rs <- lmer(mood ~ TotalhrAsleep + (1+TotalhrAsleep|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleep.rs)
MASIa.act <- lmer(mood ~ TotalStepslog + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.act)
MASIa.act.rs <- lmer(mood ~ TotalStepslog + (1+TotalStepslog|userid),data=MASIafter,REML=FALSE);summary(MASIa.act.rs)
MASIa.sleepact <- lmer(mood ~ TotalStepslog + TotalhrAsleep + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleepact)
MASIa.sleepact.inter <- lmer(mood ~ TotalStepslog * TotalhrAsleep + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleepact.inter)
anova(MASIa.sleep,MASIa.sleepact)
anova(MASIa.act,MASIa.sleepact)
anova(MASIa.null,MASIa.sleepact,MASIa.sleepact.inter)
anova(MASIa.sleep,MASIa.sleep.rs)
anova(MASIa.act,MASIa.act.rs)

MASIa.null.AG <- lmer(mood ~ Age+Sex+(1|userid),data=MASIafter,REML=FALSE);summary(MASIa.null.AG)
MASIa.sleep.AG <- lmer(mood ~ Age+Sex+TotalhrAsleep+ (1|userid),data=MASIafter, REML=FALSE );summary(MASIa.sleep.AG)
MASIa.sleep.rs.AG <- lmer(mood ~ Age+Sex+TotalhrAsleep + (1+TotalhrAsleep|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleep.rs.AG)
MASIa.act.AG <- lmer(mood ~ Age+Sex+TotalStepslog + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.act.AG)
MASIa.act.rs.AG <- lmer(mood ~ Age+Sex+TotalStepslog + (1+TotalStepslog|userid),data=MASIafter,REML=FALSE);summary(MASIa.act.rs.AG)
MASIa.sleepact.AG <- lmer(mood ~ Age+Sex+TotalStepslog + TotalhrAsleep + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleepact.AG)
MASIa.sleepact.inter.AG <- lmer(mood ~ Age+Sex+TotalStepslog * TotalhrAsleep + (1|userid),data=MASIafter,REML=FALSE);summary(MASIa.sleepact.inter.AG)
anova(MASIa.sleep.AG,MASIa.sleepact.AG)
anova(MASIa.act.AG,MASIa.sleepact.AG)
anova(MASIa.null.AG,MASIa.sleepact.AG,MASIa.sleepact.inter.AG)

MoodActSleepIntern <- MoodActSleepIntern[which(!is.na(MoodActSleepIntern$Sex)),]
MASI.ba.mood.null <- lmer(mood ~ (1|userid),data=MoodActSleepIntern,REML=FALSE);
MASI.ba.act.null <- lmer(TotalStepslog ~ (1|userid),data=MoodActSleepIntern,REML=FALSE);
MASI.ba.sleep.null <- lmer(TotalhrAsleep ~ (1|userid),data=MoodActSleepIntern,REML=FALSE);
MASI.ba.sleepeff.null <- lmer(Ratio ~ (1|userid),data=MoodActSleepIntern,REML=FALSE);
MASI.ba.mood <- lmer(mood ~ AfterIntern + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.ba.mood)
MASI.ba.act <- lmer(TotalStepslog ~ AfterIntern + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.ba.act)
MASI.ba.sleep <- lmer(TotalhrAsleep ~ AfterIntern + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.ba.sleep)
MASI.ba.sleepeff <- lmer(Ratio ~ AfterIntern + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.ba.sleepeff)
MASI.gender.mood <- lmer(mood ~ Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gender.mood)
MASI.gender.act <- lmer(TotalStepslog ~ Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gender.act)
MASI.gender.sleep <- lmer(TotalhrAsleep ~ Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gender.sleep)
MASI.gender.sleepeff <- lmer(Ratio ~ Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gender.sleepeff)
MASI.gba.mood <- lmer(mood ~ AfterIntern + Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gba.mood)
MASI.gba.act <- lmer(TotalStepslog ~ AfterIntern + Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gba.act)
MASI.gba.sleep <- lmer(TotalhrAsleep ~ AfterIntern + Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gba.sleep)
MASI.gba.sleepeff <- lmer(Ratio ~ AfterIntern + Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gba.sleepeff)
MASI.gbai.mood <- lmer(mood ~ AfterIntern * Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gbai.mood)
MASI.gbai.act <- lmer(TotalStepslog ~ AfterIntern * Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gbai.act)
MASI.gbai.sleep <- lmer(TotalhrAsleep ~ AfterIntern * Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gbai.sleep)
MASI.gbai.sleepeff <- lmer(Ratio ~ AfterIntern * Sex + (1|userid),data=MoodActSleepIntern,REML=FALSE);summary(MASI.gbai.sleepeff)
anova(MASI.ba.mood.null,MASI.ba.mood,MASI.gba.mood,MASI.gbai.mood)
anova(MASI.ba.act.null,MASI.ba.act,MASI.gba.act,MASI.gbai.act)
anova(MASI.ba.sleep.null,MASI.ba.sleep,MASI.gba.sleep,MASI.gbai.sleep)
anova(MASI.ba.sleepeff.null,MASI.ba.sleepeff,MASI.gba.sleepeff,MASI.gbai.sleepeff)
anova(MASI.ba.mood.null,MASI.gender.mood)
anova(MASI.ba.act.null,MASI.gender.act)
anova(MASI.ba.sleep.null,MASI.gender.sleep)
anova(MASI.ba.sleepeff.null,MASI.gender.sleepeff)
######## Multi Level ########
MoodActSleep$meanhrAsleep <- ave(MoodActSleep$TotalhrAsleep,list(MoodActSleep$userid))
MoodActSleep$centhrAsleep <- MoodActSleep$TotalhrAsleep - MoodActSleep$meanhrAsleep
MoodActSleep$thousandStep <- MoodActSleep$TotalSteps / 1000
MoodActSleep$meanthousandStep <- ave(MoodActSleep$thousandStep,list(MoodActSleep$userid))
MoodActSleep$centthousandStep <- MoodActSleep$thousandStep - MoodActSleep$meanthousandStep
MoodActSleep$meanEff <- ave(MoodActSleep$Ratio,list(MoodActSleep$userid))
MoodActSleep[,c("day")]<-scale(MoodActSleep[,c("day")])
MoodActSleep$daysquare <- MoodActSleep$day ^2
#### same day ####
model1 <-  lmer(mood ~ 1 + (1|userid),data=MoodActSleep,REML=FALSE);summary(model1)
model2 <-  lmer(mood ~ 1 + meanhrAsleep + meanthousandStep + meanEff + Age + Sex + (1|userid),data=MoodActSleep,REML=FALSE);summary(model2)
#model3 <- lmer(mood ~ 1 + meanhrAsleep + meanthousandStep + meanEff + Age + Sex +  TotalhrAsleep + thousandStep + Ratio + (1|userid),data=MoodActSleep,REML=FALSE);summary(model3)
#model4 <- lmer(mood ~ 1 + meanhrAsleep + meanthousandStep +  centhrAsleep + centthousandStep + (1|userid),data=MoodActSleep,REML=FALSE);summary(model4)
#model5 <- lmer(mood ~ 1 + meanhrAsleep * TotalhrAsleep + meanEff + meanthousandStep + thousandStep + (1|userid),data=MoodActSleep,REML=FALSE);summary(model5)
#model6 <- lmer(mood ~ 1 + meanhrAsleep * TotalhrAsleep + meanEff + meanthousandStep * thousandStep + (1|userid),data=MoodActSleep,REML=FALSE);summary(model6)
model10 <- lmer(mood ~ 1 + meanhrAsleep + meanthousandStep + meanEff + Age + Sex + thousandStep + TotalhrAsleep + Ratio  + day + daysquare + (1|userid),data=MoodActSleep,REML=FALSE);summary(model10)
model11 <- lmer(mood ~ 1 + meanhrAsleep + meanthousandStep + meanEff + Age + Sex + thousandStep + TotalhrAsleep + Ratio  + day + daysquare + (1+day|userid),data=MoodActSleep,REML=FALSE);summary(model11)
model12 <- lmer(mood ~ 1 + meanhrAsleep * TotalhrAsleep + meanEff + meanthousandStep *  thousandStep + Ratio + Age + Sex + day + daysquare + (1+day|userid),data=MoodActSleep,REML=FALSE);summary(model12)
model13 <- lmer(mood ~ 1 + meanhrAsleep + meanthousandStep + meanEff + Age + Sex + Ratio + TotalhrAsleep * day + thousandStep * day + daysquare + (1|userid),data=MoodActSleep,REML=FALSE);summary(model13)
modelfinal <- lmer(mood ~ 1 + TotalhrAsleep * day + thousandStep * day + daysquare + (1+day|userid),data=MoodActSleep,REML=FALSE);summary(modelfinal)
sjt.lmer(model1,model2,model10,model11,showHeaderStrings=TRUE,stringB="Estimate",
         stringDependentVariables="Response",labelDependentVariables=c("null","subject-level","both-levels","both-levels-random-slope"),
         labelPredictors=c("meanAsleepHr","meanSteps/1000","meanSleepEfficiency","Age","Sex","Steps/1000","AsleepHr","SleepEfficiency","day","daysquare","mean:everday sleep","mean:everyday step"),
         separateConfColumn=FALSE, showStdBeta=FALSE,pvaluesAsNumbers=FALSE,showAIC=TRUE,
         digits.p=3,digits.est=2,digits.ci=2)
#### same day updated ####
PHQtimes <- PHQ.BS[c("UserID","Year","surveyDate0","surveyDate1","surveyDate2")]
PHQtimes$surveyDate1[which(PHQtimes$Year=="2014" & is.na(PHQtimes$surveyDate1))]=
  mean(PHQtimes$surveyDate1[which(PHQtimes$Year=="2014")],na.rm=T)
PHQtimes$surveyDate1[which(PHQtimes$Year=="2015" & is.na(PHQtimes$surveyDate1))]=
  mean(PHQtimes$surveyDate1[which(PHQtimes$Year=="2015")],na.rm=T)
PHQtimes$surveyDate2[which(PHQtimes$Year=="2014" & is.na(PHQtimes$surveyDate2))]=
  mean(PHQtimes$surveyDate2[which(PHQtimes$Year=="2014")],na.rm=T)
PHQtimes$surveyDate2[which(PHQtimes$Year=="2015" & is.na(PHQtimes$surveyDate2))]=
  mean(PHQtimes$surveyDate2[which(PHQtimes$Year=="2015")],na.rm=T)
MoodActSleep$AfterIntern <- MoodActSleep$day>=0;table(MoodActSleep$userid,MoodActSleep$AfterIntern)
MASfull <- merge(MoodActSleep,PHQtimes,by.x="userid",by.y="UserID",all.x=TRUE)
MASpre <- MASfull[which(MASfull$day<0),]
MASpre$meanSleeppre <- ave(MASpre$TotalhrAsleep,list(MASpre$userid))
MASpre$meanSteppre <- ave(MASpre$thousandStep,list(MASpre$userid))
MASpre$meanMoodpre <- ave(MASpre$mood,list(MASpre$userid))
MASpre <- MASpre[c("userid","meanSleeppre","meanSteppre","meanMoodpre")]
MASpre <- MASpre[!duplicated(MASpre),]
#MASwodate <- MASfull[which(is.na(MASfull$surveyDate1)),]
#MASwdate <- MASfull[which(!is.na(MASfull$surveyDate1)),]
#MASq1 <- rbind(MASwdate[which((MASwdate$day>=0 & MASwdate$Date_mood<MASwdate$surveyDate1)),],
#               MASwodate[which(MASwodate$day>=0 & MASwodate$day<90),])
#MASq2 <- rbind(MASwdate[which(MASwdate$Date_mood>=MASwdate$surveyDate1 & MASwdate$Date_mood<MASwdate$surveyDate2),],
#               MASwodate[which(MASwodate$day>=90 & MASwodate$day<180),])
#MASq1$meanSleepq1 <- ave(MASq1$TotalhrAsleep,list(MASq1$userid))
#MASq1$meanStepq1 <- ave(MASq1$thousandStep,list(MASq1$userid))
#MASq2$meanSleepq2 <- ave(MASq2$TotalhrAsleep,list(MASq2$userid))
#MASq2$meanStepq2 <- ave(MASq2$thousandStep,list(MASq2$userid))
#MASq1 <- MASq1[c("userid","meanSleepq1","meanStepq1")]
#MASq1 <- MASq1[!duplicated(MASq1),]
#MASq2 <- MASq2[c("userid","meanSleepq2","meanStepq2")]
#MASq2 <- MASq2[!duplicated(MASq2),]
MASpost <- MoodActSleep[which(MoodActSleep$day>=0),]
MASpost <- merge(MASpost,MASpre,by="userid")
#MASpost <- merge(MASpost,MASq1,by="userid")
#MASpost <- merge(MASpost,MASq2,by="userid")
#MASpost$Ethnicity<-as.factor(MASpost$Ethnicity)
#MASpost$Marital<-as.factor(MASpost$Marital)
#MASpost$Child<-as.factor(MASpost$Child)
MASpost[,c("day")]<-scale(MASpost[,c("day")])
MASpost$daysquare <- MASpost$day^2
MASpost$SleepPre <- MASpost$meanSleeppre>=6
#MASpost$Sleepq1 <- MASpost$meanSleepq1>=6
#MASpost$Sleepq2 <- MASpost$meanSleepq2>=6
# the reason to use continuous baseline sleep instead of binary: most of the sleep time is over 6h
MASpost<-MASpost[which(!is.na(MASpost$Age) & !is.na(MASpost$Sex)),]
MASpost<-merge(MASpost,Baseline2010,by.x="userid",by.y="UserID")
stargazer(MASpost,type="text")
colnames(MASpost)[which(names(MASpost)=="meanMoodpre")] <- "BaselineMood"
colnames(MASpost)[which(names(MASpost)=="meanSleeppre")] <- "BaselineSleepTime"
colnames(MASpost)[which(names(MASpost)=="meanSteppre")] <- "BaselineSteps"
colnames(MASpost)[which(names(MASpost)=="Neu0")] <- "Neuroticism"
colnames(MASpost)[which(names(MASpost)=="depr0")] <- "PersonalHistoryofDepression"
colnames(MASpost)[which(names(MASpost)=="PHQtot0")] <- "BaselineDepressionSymptoms"
colnames(MASpost)[which(names(MASpost)=="EFE0")] <- "DifficultEarlyFamilyEnvironment"
MASpost$PersonalHistoryofDepression <-as.numeric(MASpost$PersonalHistoryofDepression)
MASpost$Sex <-as.numeric(MASpost$Sex)
MASpost2ndlevel <- MASpost[c("userid","Age","Sex","BaselineMood","BaselineSleepTime","BaselineSteps","Neuroticism",
                             "PersonalHistoryofDepression","BaselineDepressionSymptoms","DifficultEarlyFamilyEnvironment")]
MASpost2ndlevel <- MASpost2ndlevel[!duplicated(MASpost2ndlevel),]
stargazer(MASpost2ndlevel,type="text")

#### check collinearity ####
submat<-MASpost[c("Age","Sex","BaselineMood","BaselineSleepTime","BaselineSteps",
                  "Neuroticism","PersonalHistoryofDepression","BaselineDepressionSymptoms","DifficultEarlyFamilyEnvironment")]
#submat <- submat[!duplicated(submat),]
corstarsl <- function(x){ 
  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  # define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  # trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  # build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  # remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  # remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}
corresult=corstarsl(submat)
grid.table(corresult)
stargazer(corresult, type="text")
corvif(submat)
MASposttmp<-MASpost[c("mood","Age","Sex","BaselineSleepTime","BaselineSteps","BaselineMood",
                      "PersonalHistoryofDepression","DifficultEarlyFamilyEnvironment","Neuroticism","BaselineDepressionSymptoms")]
vif.sel<-VIF::vif(MASposttmp$mood,MASposttmp[c(-1)])
#### stepwise ####
fit.full<-lm(mood ~ 1 + Age + Sex + BaselineMood + BaselineSleepTime + BaselineSteps + 
               Neuroticism + PersonalHistoryofDepression + BaselineDepressionSymptoms + DifficultEarlyFamilyEnvironment,data=MASposttmp)
fit.null<-lm(mood ~ 1,data=MASposttmp)
stats::step(fit.null,scope = list(lower=fit.null,upper=fit.full),direction="forward")
stats::step(fit.full,data=MASposttmp,direction="backward")
stats::step(fit.null,scope=list(upper=fit.full),data=MASposttmp,direction="both")
attach(MASpost)
#### leap ####
leaps<-regsubsets(mood ~ 1 + Age + Sex + BaselineMood + BaselineSleepTime + BaselineSteps + 
                    Neuroticism + PersonalHistoryofDepression + BaselineDepressionSymptoms + DifficultEarlyFamilyEnvironment,data=MASpost,nbest=10)
plot(leaps,scale="adjr2")
plot(leaps,scale="bic")
plot(leaps,scale="Cp")
bestfit<-lm(mood ~ 1 + Age + Sex + meanMoodpre + Neu0 + EFE0 + meanSleeppre + meanSleepq2 + meanStepq2,data=MASpost)
summary(bestfit)
bestfit_2<-lm(mood ~ 1 + day + Age + Sex + meanMoodpre + Neu0 + EFE0 + meanSleeppre + meanSleepq2 + meanStepq2,data=MASpost)
summary(bestfit_2)

#### second-level variables ####
m1 <-lm(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism, data=MASpost);summary(m1)
stargazer(m1,type="text",star.cutoffs=c(0.05,0.01,0.001));
coefs=summary(m1)$coefficients;p.adjust(coefs[2:nrow(coefs),4],method="fdr")<0.001
m2 <-lm(mood ~ 1 + Age + BaselineMood + BaselineSleepTime, 
        data=MASpost);summary(m2)
stargazer(m2,type="text")
coefs=summary(m2)$coefficients;p.adjust(coefs[2:nrow(coefs),4],method="fdr")<0.05
#### add daily data ####
m3 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + (1|userid), data=MASpost);summary(m3)
coefs=summary(m3)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
m4 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + daysquare + (1|userid), data=MASpost);summary(m4)
coefs=summary(m4)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
m5 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + daysquare + TotalhrAsleep + (1|userid), data=MASpost);summary(m5)
coefs=summary(m5)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
m6 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + daysquare + TotalhrAsleep + thousandStep + (1|userid), data=MASpost);summary(m6)
coefs=summary(m6)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
m7 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + daysquare + TotalhrAsleep + thousandStep + (1+TotalhrAsleep|userid), data=MASpost);summary(m7)
coefs=summary(m7)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
m8 <-lmer(mood ~ 1 + Age + BaselineMood + BaselineSleepTime + PersonalHistoryofDepression + DifficultEarlyFamilyEnvironment + Neuroticism
          + day + daysquare + TotalhrAsleep + thousandStep + (1+TotalhrAsleep+thousandStep|userid), data=MASpost);summary(m8)
coefs=summary(m8)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")<0.001
mreduce1 <-lmer(mood ~ 1 + BaselineMood + DifficultEarlyFamilyEnvironment
                + day + daysquare + TotalhrAsleep + thousandStep + (1+TotalhrAsleep+thousandStep|userid), data=MASpost)
summary(mreduce1)
coefs=summary(mreduce1)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")

sjt.lmer(m3,m4,m5,m6,m7,m8,
         showHeaderStrings=TRUE,stringB="Estimate",
         stringDependentVariables="Response",
         labelDependentVariables=c("Day","Daysquare","Daily Sleep Hour","Daily Steps(*1000)","Random slope of daily sleep","random slope of daily steps"),
         labelPredictors=c("Age","Baseline Mood","Baseline Sleep Hour","Personal History of Depression","DifficultyEarlyFamilyEnviornment","Neuroticism",
                           "day","daysquare","Daily Sleep Hour","Daily Steps(*1000)"),
         separateConfColumn=FALSE, showStdBeta=FALSE,pvaluesAsNumbers=FALSE,showAIC=TRUE,
         digits.p=3,digits.est=2,digits.ci=2)
sjt.lmer(mreduce1,
         showHeaderStrings=TRUE,stringB="Estimate",
         stringDependentVariables="Response",
         labelDependentVariables=c("reducedModel"),
         labelPredictors=c("Baseline Mood","DifficultyEarlyFamilyEnviornment","day","daysquare","Daily Sleep Hour","Daily Steps(*1000)"),
         separateConfColumn=FALSE, showStdBeta=FALSE,pvaluesAsNumbers=FALSE,showAIC=TRUE,
         digits.p=3,digits.est=2,digits.ci=2)

# Table 1
mean(MASpost$TotalhrAsleep);sd(MASpost$TotalhrAsleep)
mean(MASpost$thousandStep);sd(MASpost$thousandStep)
# Table 2
rcorr(as.matrix(MASpost[,c("TotalhrAsleep","thousandStep","mood","Age","Sex")]))
######## Time lag association ########
Baselines <- MASpost[c("userid","BaselineMood","DifficultEarlyFamilyEnvironment","StartDate")]
Baselines <- Baselines[!duplicated(Baselines),]
Mood<-rbind(Mood2014,Mood2015)
Sleep$Yesterday<-Sleep$SleepDay-1;Sleep$Tomorrow<-Sleep$SleepDay+1;Sleep$tdby<-Sleep$SleepDay-2;Sleep$tdat<-Sleep$SleepDay+2
Activity$Yesterday<-Activity$ActivityDate-1;Activity$Tomorrow<-Activity$ActivityDate+1;Activity$tdby<-Activity$ActivityDate-2;Activity$tdat<-Activity$ActivityDate+2
#timestamp<-"tdby"  # mood@day x-2 ~ fitbit @ day x
#timestamp<-"Yesterday" # mood@day x-1 ~ fitbit @ day x
timestamp<-"Tomorrow" # mood@day x+1 ~ fitbit @ day x
#timestamp<-"tdat" # mood@day x+2 ~ fitbit @ day x
MSlag=merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id",timestamp))
MSlag=merge(MSlag,Baselines,by=c("userid","StartDate"))
MSlag$day<-MSlag$Date_mood - MSlag$StartDate;
MAlag=merge(Mood,Activity,by.x=c("userid","Date_mood"),by.y=c("Id",timestamp))
MAlag=merge(MAlag,Baselines,by=c("userid","StartDate"))
MAlag$day<-MAlag$Date_mood - MAlag$StartDate
mergecols=c("userid","StartDate","Date_mood","mood","day","BaselineMood","DifficultEarlyFamilyEnvironment")
MASlag=merge(MAlag,MSlag,by=mergecols,all=FALSE)
MASlag[,c("day")]<-scale(MASlag[,c("day")]);
MASlag$daysquare<-as.numeric(MASlag$day)^2;
MASlag$thousandStep<-MASlag$TotalSteps / 1000
MASlag$TotalhrAsleep<-MASlag$TotalHrAsleep
modelfinal <- lmer(mood ~ 1 + BaselineMood + DifficultEarlyFamilyEnvironment + day + daysquare + TotalhrAsleep + thousandStep + (1+TotalhrAsleep+thousandStep|userid),data=MASlag);
summary(modelfinal)
coefs=summary(modelfinal)$coefficients;p.adjust(coefs[2:nrow(coefs),5],method="fdr")

sjt.lmer(modelfinal,
         showHeaderStrings=TRUE,stringB="Estimate",
         stringDependentVariables="Response",
         labelDependentVariables=c("mood@dayX+2 ~ fitbit@dayX"),
         labelPredictors=c("Baseline Mood","DifficultEarlyFamilyEnvironment","day","daysquare","Daily Sleep Hour","Daily Steps(*1000)"),
         separateConfColumn=FALSE, showStdBeta=FALSE,pvaluesAsNumbers=FALSE,showAIC=TRUE,
         digits.p=3,digits.est=2,digits.ci=2)
         
#### Mood - sleep efficiency time lag correlation ####
MoodSleepfull=merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","SleepDay"),all.x=TRUE,all.y=TRUE,sort=TRUE)
MoodSleepfull$Ratio=MoodSleepfull$TotalMinutesAsleep/MoodSleepfull$TotalTimeInBed
ccf(MoodSleepfull$mood,MoodSleepfull$TotalTimeInBed,lag.max=7,na.action=na.pass,plot=TRUE)
ccf(MoodSleepfull$mood,MoodSleepfull$TotalMinutesAsleep,na.action=na.pass,lag.max=7,plot=TRUE)
ccf(MoodSleepfull$mood,MoodSleepfull$Ratio,lag.max=7,na.action=na.pass,plot=TRUE)
test=MoodSleepfull[which(MoodSleepfull$userid=="110000"),]
ccf(test$mood,test$TotalTimeInBed,lag.max=7,na.action=na.pass,plot=TRUE)
ccf(test$mood,test$TotalMinutesAsleep,na.action=na.pass,lag.max=7,plot=TRUE)
ccf(test$mood,test$Ratio,lag.max=7,na.action=na.pass,plot=TRUE)
#### time lag mixed models ####
MoodSleepProcess=function(x){
  x=x[which(x$TotalMinutesAsleep!=0 & x$TotalTimeInBed!=0),]
  x=x[which(!is.na(x$mood)),]
  x$userid=as.factor(x$userid)
  x$TotalhrAsleep=x$TotalMinutesAsleep/60
  x$Ratio=x$TotalMinutesAsleep / x$TotalTimeInBed
  return(x)
}
# Sleep @ day X ~ Mood @ day X-2
MoodSleeptdby = merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","tdby"),sort=TRUE);nrow(MoodSleeptdby)
MoodSleeptdby = MoodSleepProcess(MoodSleeptdby)
MoodSleeptdby.null=lmer(mood ~ (1|userid),data=MoodSleeptdby,REML=FALSE)
MoodSleeptdby.Mixed1=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleeptdby,REML=FALSE);summary(MoodSleeptdby.Mixed1)
MoodSleeptdby.Mixed2=lmer(mood ~ Ratio + (1|userid),data=MoodSleeptdby,REML=FALSE);summary(MoodSleeptdby.Mixed2)
MoodSleeptdby.Mixed3=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleeptdby,REML=FALSE);summary(MoodSleeptdby.Mixed3)
anova(MoodSleeptdby.null,MoodSleeptdby.Mixed1,MoodSleeptdby.Mixed3)
anova(MoodSleeptdby.null,MoodSleeptdby.Mixed2)
SleepMoodtdby.null1=lmer(TotalhrAsleep ~ (1|userid),data=MoodSleeptdby,REML=FALSE)
SleepMoodtdby.null2=lmer(Ratio ~ (1|userid),data=MoodSleeptdby,REML=FALSE)
SleepMoodtdby.Mixed1=lmer(TotalhrAsleep ~ mood + (1|userid),data=MoodSleeptdby,REML=FALSE);summary(SleepMoodtdby.Mixed1)
SleepMoodtdby.Mixed2=lmer(Ratio ~ mood + (1|userid),data=MoodSleeptdby,REML=FALSE);summary(SleepMoodtdby.Mixed2)
anova(SleepMoodtdby.null1,SleepMoodtdby.Mixed1)
anova(SleepMoodtdby.null2,SleepMoodtdby.Mixed2)
# Sleep @ day X ~ Mood @ day X-1
MoodSleepyd = merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","Yesterday"),sort=TRUE);nrow(MoodSleepyd)
MoodSleepyd = MoodSleepProcess(MoodSleepyd)
MoodSleepyd.null=lmer(mood ~ (1|userid),data=MoodSleepyd,REML=FALSE)
MoodSleepyd.Mixed1=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleepyd,REML=FALSE);summary(MoodSleepyd.Mixed1)
MoodSleepyd.Mixed2=lmer(mood ~ Ratio + (1|userid),data=MoodSleepyd,REML=FALSE);summary(MoodSleepyd.Mixed2)
MoodSleepyd.Mixed3=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleepyd,REML=FALSE);summary(MoodSleepyd.Mixed3)
anova(MoodSleepyd.null,MoodSleepyd.Mixed1,MoodSleepyd.Mixed3)
anova(MoodSleepyd.null,MoodSleepyd.Mixed2)
SleepMoodyd.null1=lmer(TotalhrAsleep ~ (1|userid),data=MoodSleepyd,REML=FALSE)
SleepMoodyd.null2=lmer(Ratio ~ (1|userid),data=MoodSleepyd,REML=FALSE)
SleepMoodyd.Mixed1=lmer(TotalhrAsleep ~ mood + (1|userid),data=MoodSleepyd,REML=FALSE);summary(SleepMoodyd.Mixed1)
SleepMoodyd.Mixed2=lmer(Ratio ~ mood + (1|userid),data=MoodSleepyd,REML=FALSE);summary(SleepMoodyd.Mixed2)
anova(SleepMoodyd.null1,SleepMoodyd.Mixed1)
anova(SleepMoodyd.null2,SleepMoodyd.Mixed2)
# Sleep @ day X ~ Mood @ day X+1
MoodSleeptm = merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","Tomorrow"),sort=TRUE);nrow(MoodSleeptm)
MoodSleeptm = MoodSleepProcess(MoodSleeptm)
MoodSleeptm.null=lmer(mood ~ (1|userid),data=MoodSleeptm,REML=FALSE)
MoodSleeptm.Mixed1=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleeptm,REML=FALSE);summary(MoodSleeptm.Mixed1)
MoodSleeptm.Mixed2=lmer(mood ~ Ratio + (1|userid),data=MoodSleeptm,REML=FALSE);summary(MoodSleeptm.Mixed2)
MoodSleeptm.Mixed3=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleeptm,REML=FALSE);summary(MoodSleeptm.Mixed3)
anova(MoodSleeptm.null,MoodSleeptm.Mixed1,MoodSleeptm.Mixed3)
anova(MoodSleeptm.null,MoodSleeptm.Mixed2)
# Sleep @ day X ~ Mood @ day X+2
MoodSleeptdat = merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","tdat"),sort=TRUE);nrow(MoodSleeptdat)
MoodSleeptdat = MoodSleepProcess(MoodSleeptdat)
MoodSleeptdat.null=lmer(mood ~ (1|userid),data=MoodSleeptdat,REML=FALSE)
MoodSleeptdat.Mixed1=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleeptdat,REML=FALSE);summary(MoodSleeptdat.Mixed1)
MoodSleeptdat.Mixed2=lmer(mood ~ Ratio + (1|userid),data=MoodSleeptdat,REML=FALSE);summary(MoodSleeptdat.Mixed2)
MoodSleeptdat.Mixed3=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleeptdat,REML=FALSE);summary(MoodSleeptdat.Mixed3)
anova(MoodSleeptdat.null,MoodSleeptdat.Mixed1,MoodSleeptdat.Mixed3)
anova(MoodSleeptdat.null,MoodSleeptdat.Mixed2)
# Sleep @ day X ~ Mood @ day X
MoodSleeptoday = merge(Mood,Sleep,by.x=c("userid","Date_mood"),by.y=c("Id","SleepDay"),sort=TRUE);nrow(MoodSleeptoday)
MoodSleeptoday = MoodSleepProcess(MoodSleeptoday)
MoodSleeptoday.null=lmer(mood ~ (1|userid),data=MoodSleeptoday,REML=FALSE)
MoodSleeptoday.Mixed1=lmer(mood ~ TotalhrAsleep + (1|userid),data=MoodSleeptoday,REML=FALSE);summary(MoodSleeptoday.Mixed1)
MoodSleeptoday.Mixed2=lmer(mood ~ Ratio + (1|userid),data=MoodSleeptoday,REML=FALSE);summary(MoodSleeptoday.Mixed2)
MoodSleeptoday.Mixed3=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MoodSleeptoday,REML=FALSE);summary(MoodSleeptoday.Mixed3)
anova(MoodSleeptoday.null,MoodSleeptoday.Mixed1,MoodSleeptoday.Mixed3)
anova(MoodSleeptoday.null,MoodSleeptoday.Mixed2)

# Compare different day lag models
keepcols=c("userid","Date_mood","mood","TotalhrAsleep","Ratio")
mergecols=c("userid","Date_mood","mood")
MStodayyd=merge(MoodSleeptoday[keepcols],MoodSleepyd[keepcols],by=mergecols)
MStodayydtm=merge(MStodayyd,MoodSleeptm[keepcols],by=mergecols)
names(MStodayydtm)[8:9]=c("TotalhrAsleep.z","ratio.z")
MStdydtmtdby=merge(MStodayydtm,MoodSleeptdby[keepcols],by=mergecols)
names(MStdydtmtdby)[10:11]=c("TotalhrAsleep.a","ratio.a")
MStdydtmtdbytdat=merge(MStdydtmtdby,MoodSleeptdat[keepcols],by=mergecols)

mtoday=lmer(mood ~ TotalhrAsleep.x + Ratio.x + (1|userid),data=MStdydtmtdbytdat,REML=FALSE);summary(mtoday)
myd=lmer(mood ~ TotalhrAsleep.y + Ratio.y + (1|userid),data=MStdydtmtdbytdat,REML=FALSE);summary(myd)
mtm=lmer(mood ~ TotalhrAsleep.z + ratio.z + (1|userid),data=MStdydtmtdbytdat,REML=FALSE);summary(mtm)
mtdby=lmer(mood ~ TotalhrAsleep.a + ratio.a + (1|userid),data=MStdydtmtdbytdat,REML=FALSE);summary(mtdby)
mtdat=lmer(mood ~ TotalhrAsleep + Ratio + (1|userid),data=MStdydtmtdbytdat,REML=FALSE);summary(mtdat)
####################################### Mood/Activity/Sleep summary ####################################### 
Summary.MSA = merge(Summary.activity,Summary.sleep,by="id",all.x=TRUE)
Summary.MSA = merge(Summary.MSA,Summary.mood,by="id",all.x=TRUE)
MSA.corr=rcorr(as.matrix(Summary.MSA[c("meanSteps","meancalories","meandistance","meanveryactmin","meanfairactmin","meanlightactmin","meansedmin","meanMinAsleep","meanMinInbed","meanAsleepInbedratio","meanMood")]))
write.table(data.frame(MSA.corr$P),"work/Fitbit/FBscripts/tmp.csv",sep=",")
####################################### PHQ #######################################
PHQ <- read.csv("Z:././././Data Analysis/Yu Fang/data/data14all_15BLq1q2_PHQ_Sleep_02162016.csv")
Baseline2010 <- PHQ[c("UserID","depr0","EFE0","Neu0","PHQtot0")]  # significant factor in 2010 paper, female sex included somewhere else

PHQ.BS1.2014=read.csv('z:/Data Analysis/Yu Fang/data/2014BioShort1.csv')
PHQ.BS2.2014=read.csv('z:/Data Analysis/Yu Fang/data/2014BioShort2.csv')
colnames(PHQ.BS1.2014)=paste(colnames(PHQ.BS1.2014),"BS1",sep="_")
colnames(PHQ.BS2.2014)=paste(colnames(PHQ.BS2.2014),"BS2",sep="_")
PHQ.BS.2014=merge(PHQ,PHQ.BS1.2014,by.x="UserID",by.y="USERID_BS1",all.y=TRUE)
PHQ.BS.2014=merge(PHQ.BS.2014,PHQ.BS2.2014,by.x="UserID",by.y="USERID_BS2",all.y=TRUE)

PHQ.BS1.2015=read.csv('z:/Data Analysis/Yu Fang/data/2015BioShort1.csv')
PHQ.BS2.2015=read.csv('z:/Data Analysis/Yu Fang/data/2015BioShort2.csv')
colnames(PHQ.BS1.2015)=paste(colnames(PHQ.BS1.2015),"BS1",sep="_")
colnames(PHQ.BS2.2015)=paste(colnames(PHQ.BS2.2015),"BS2",sep="_")
PHQ.BS.2015=merge(PHQ,PHQ.BS1.2015,by.x="UserID",by.y="USERID_BS1",all.y=TRUE)
PHQ.BS.2015=merge(PHQ.BS.2015,PHQ.BS2.2015,by.x="UserID",by.y="USERID_BS2",all.y=TRUE)

PHQ.BS=rbind(PHQ.BS.2014,PHQ.BS.2015)

PHQ.datecol1=c("surveyDate0","surveyDate1","surveyDate2","surveyDate3","surveyDate4")
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
####################################### Self reported Sleep #######################################
PHQSleep<-PHQ[which(PHQ$UserID %in% Sleep$Id),]
PHQSleep$surveyDate1<-as.Date(as.character(PHQSleep$surveyDate1),"%d%b%y")
PHQSleep$surveyDate2<-as.Date(as.character(PHQSleep$surveyDate2),"%d%b%y")
#### sleep on PHQ date 1 ####
SleepPHQ1=merge(PHQSleep,Sleep,by.x=c("UserID","surveyDate1"),by.y=c("Id","SleepDay"))
SleepPHQ1=SleepPHQ1[c("UserID","surveyDate1","Year","Age","Sex","sleep24h1","sleepAve1","interest1","down1","asleep1","tired1","appetite1","failure1","concentr1","activity1","suic1","PHQtot1",
                      "TotalSleepRecords","TotalMinutesAsleep","TotalTimeInBed")]
SPfactorcols=c("interest1","down1","asleep1","tired1","appetite1","failure1","concentr1","activity1","suic1")
# way one: factorize the scores (0,1,2,3)
SleepPHQ1[SPfactorcols]<-lapply(SleepPHQ1[SPfactorcols],as.factor)
# way two: binarize the scores (0,1)
SleepPHQ1[SPfactorcols]=ifelse(SleepPHQ1[SPfactorcols]>0,1,0)
# Add hr and ratio
SleepPHQ1$TotalHrAsleep=SleepPHQ1$TotalMinutesAsleep/60
SleepPHQ1$SRvFB=SleepPHQ1$sleep24h1/SleepPHQ1$TotalHrAsleep
SleepPHQ1$SRvFBbin=SleepPHQ1$SRvFB>1
SleepPHQ1$AvI=SleepPHQ1$TotalMinutesAsleep/SleepPHQ1$TotalTimeInBed
#### plot SRvFB vs PHQtot
plot(SleepPHQ1$SRvFB,SleepPHQ1$PHQtot1,type="p",xlab="selfreport/fitbit sleep",ylab="PHQtotal",main="PHQ date1",col="blue",
     pch=20,cex=1.5,cex.lab=1.5,cex.main=2)
#### predict variables: SRvFB, SRvFBbin, TotalHrAsleep, AVI ####
m1=glm(interest1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m1)
m2=glm(down1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m2)
m3=glm(asleep1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m3)
m4=glm(tired1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m4)
m5=glm(appetite1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m5)
m6=glm(failure1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m6)
m7=glm(concentr1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m7)
m8=glm(activity1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m8)
m9=glm(suic1 ~ AvI, family=binomial(link='logit'),data=SleepPHQ1);summary(m9)
m10=lm(PHQtot1 ~ SRvFB, data=SleepPHQ1);summary(m10)

lapply(SleepPHQ1[,c("down1","SRvFB")],table)
ftable(xtabs(~ down1 + SRvFB,data=SleepPHQ1))
m1<-polr(interest1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m1));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m1);print(p);print(ci)
m2<-polr(down1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m2));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m2);print(p);print(ci)
m3<-polr(asleep1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m3));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m3);print(p);print(ci)
m4<-polr(tired1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m4));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m4);print(p);print(ci)
m5<-polr(appetite1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m5));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m5);print(p);print(ci)
m6<-polr(failure1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m6));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m6);print(p);print(ci) 
m7<-polr(concentr1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m7));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m7);print(p);print(ci)
m8<-polr(activity1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m8));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m8);print(p);print(ci)
m9<-polr(suic1 ~ AvI, data=SleepPHQ1, Hess=TRUE);ctable=coef(summary(m9));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;ci=confint(m9);print(p);print(ci)

# t-test on PHQ tot
pt1=SleepPHQ1$PHQtot1[which(SleepPHQ1$SRvFB>1)];pt2=SleepPHQ1$PHQtot1[which(SleepPHQ1$SRvFB<1)]
t.test(pt1,pt2,paired = F)
# plot 
plot(SleepPHQ1$TotalHrAsleep,SleepPHQ1$sleep24h1,xlab="fitbit sleep(hr)",ylab="self report sleep(hr)")
abline(0,1)
# assumption test
sf <- function(y){
  c('Y>=0' = qlogis(mean(y==0)),
    'Y>=1' = qlogis(mean(y==1)),
    'Y>=2' = qlogis(mean(y==2)))
}
s<-with(SleepPHQ1,summary(as.numeric(as.character(failure1)) ~ SRvFBbin, fun=sf))
print(s)
#### Gender difference on Selfreport/Fitbit Ratio ####
SRvFB_female=SleepPHQ1$SRvFB[which(SleepPHQ1$Sex==2)];mean(SRvFB_female)
SRvFB_male=SleepPHQ1$SRvFB[which(SleepPHQ1$Sex==1)];mean(SRvFB_male)
SRvFB_genderdiff.ttest=t.test(SRvFB_female,SRvFB_male,paired=FALSE)
boxplot(SleepPHQ1$SRvFB ~ SleepPHQ1$Sex,xlab="Gender",ylab="Selfreport/Fitbit Sleep")
#### Average sleep on the week before PHQ date 1 ####
Sleepave=merge(Sleep,PHQSleep,by.x="Id",by.y="UserID")
Sleepave1=Sleepave[which((Sleepave$surveyDate1 - Sleepave$SleepDay >=0) & (Sleepave$surveyDate1-Sleepave$SleepDay<7)),]
Sleepave2=Sleepave[which((Sleepave$surveyDate2 - Sleepave$SleepDay >=0) & (Sleepave$surveyDate2-Sleepave$SleepDay<7)),]
subs=unique(Sleepave1$Id)
nsub=length(subs)
for (isub in 1:nsub){
  tmp=Sleepave1[which(Sleepave1$Id==subs[isub]),]
  newtmp=data.frame(id=subs[isub],sleepsr=tmp$sleepAve1[1],Age=tmp$Age[1],Sex=tmp$Sex[1],
                    sleepfb=mean(tmp$TotalMinutesAsleep/60),sleepsrvfb=tmp$sleepAve1[1]/mean(tmp$TotalMinutesAsleep/60),
                    AvI=mean(tmp$TotalMinutesAsleep/tmp$TotalTimeInBed),
                    PHQtot1=tmp$PHQtot1[1],interest1=tmp$interest1[1],down1=tmp$down1[1],asleep1=tmp$asleep1[1],tired1=tmp$tired1[1],
                    appetite1=tmp$appetite1[1],failure1=tmp$failure1[1],concentr1=tmp$concentr1[1],activity1=tmp$activity1[1],suic1=tmp$suic1[1])
  if (isub==1){out=newtmp}
  else {out=rbind(out,newtmp)}
}
Sleepave1=out
plot(Sleepave1$sleepsrvfb,Sleepave1$PHQtot1,type="p",xlab="selfreport/fitbit sleep",ylab="PHQtotal",main="PHQ date1 past week average",col="blue",
     pch=20,cex=1.5,cex.lab=1.5,cex.main=2)
subs=unique(Sleepave2$Id)
nsub=length(subs)
for (isub in 1:nsub){
  tmp=Sleepave2[which(Sleepave2$Id==subs[isub]),]
  newtmp=data.frame(id=subs[isub],sleepsr=tmp$sleepAve2[1],
                    sleepfb=mean(tmp$TotalMinutesAsleep/60),sleepsrvfb=tmp$sleepAve2[1]/mean(tmp$TotalMinutesAsleep/60),
                    PHQtot2=tmp$PHQtot2[1],interest2=tmp$interest2[1],down2=tmp$down2[1],asleep2=tmp$asleep2[1],tired2=tmp$tired2[1],
                    appetite2=tmp$appetite2[1],failure2=tmp$failure2[1],concentr2=tmp$concentr2[1],activity2=tmp$activity2[1],suic2=tmp$suic2[1])
  if (isub==1){out2=newtmp}
  else {out2=rbind(out2,newtmp)}
}
Sleepave2=out2
Sleepave1$SRvFBbin=Sleepave1$sleepsrvfb>1
# way one: factorize the scores (0,1,2,3)
Sleepave1[SPfactorcols]<-lapply(Sleepave1[SPfactorcols],as.factor)
# way two: binarize the scores (0,1)
Sleepave1[SPfactorcols]=ifelse(Sleepave1[SPfactorcols]>0,1,0)
#### predict variables: sleepsrvfb, SRvFBbin, sleepfb, AvI ####
m1=glm(interest1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m1)
m2=glm(down1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m2);
m3=glm(asleep1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m3)
m4=glm(tired1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m4)
m5=glm(appetite1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m5)
m6=glm(failure1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m6)
m7=glm(concentr1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m7)
m8=glm(activity1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m8)
m9=glm(suic1 ~ AvI, family=binomial(link='logit'),data=Sleepave1);summary(m9)  
m10=lm(PHQtot1 ~ AvI, data=Sleepave1);summary(m10)
m1<-polr(interest1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m1));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m1);print(ci);
m2<-polr(down1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m2));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m2);print(ci);
m3<-polr(asleep1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m3));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m3);print(ci)
m4<-polr(tired1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m4));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m4);print(ci)
m5<-polr(appetite1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m5));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m5);print(ci)
m6<-polr(failure1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m6));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m6);print(ci) 
m7<-polr(concentr1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m7));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m7);print(ci)
m8<-polr(activity1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m8));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m8);print(ci)
m9<-polr(suic1 ~ AvI, data=Sleepave1, Hess=TRUE);ctable=coef(summary(m9));p=pnorm(abs(ctable[,"t value"]),lower.tail=FALSE)*2;print(p);ci=confint(m9);print(ci)
# t-test on PHQ tot
pt1=Sleepave1$PHQtot1[which(Sleepave1$sleepsrvfb>1)];pt2=Sleepave1$PHQtot1[which(Sleepave1$sleepsrvfb<1)]
t.test(pt1,pt2,paired = F)
# plot
plot(Sleepave1$sleepfb,Sleepave1$sleepsr,xlab="fitbit sleep average (hr)",ylab="self report sleep average (hr)")
abline(0,1)
#### Gender difference on Selfreport/Fitbit Ratio ####
SRvFB_female=Sleepave1$sleepsrvfb[which(Sleepave1$Sex==2)];mean(SRvFB_female)
SRvFB_male=Sleepave1$sleepsrvfb[which(Sleepave1$Sex==1)];mean(SRvFB_male)
t.test(SRvFB_female,SRvFB_male,paired=FALSE)
boxplot(Sleepave1$sleepsrvfb ~ Sleepave1$Sex,xlab="Gender",ylab="Selfreport/Fitbit Sleep")
#### Sleep on PHQ date 2 ####
SleepPHQ2=merge(PHQSleep,Sleep,by.x=c("UserID","surveyDate2"),by.y=c("Id","SleepDay"))
SleepPHQ2=SleepPHQ2[c("UserID","surveyDate2","Year","sleep24h2","sleepAve2","interest2","down2","asleep2","tired2","appetite2","failure2","concentr2","activity2","suic2","PHQtot2",
                      "TotalSleepRecords","TotalMinutesAsleep","TotalTimeInBed","TotalHrAsleep")]
SleepPHQ.generalname=c("surveyDate","sleep24h","sleepAve","interest","down","asleep","tired","appetite","failure","concentr","activity","suic","PHQtot")
colnames(SleepPHQ1)[c(2,4:15)]=SleepPHQ.generalname
colnames(SleepPHQ2)[c(2,4:15)]=SleepPHQ.generalname
SleepPHQ12=rbind(SleepPHQ1,SleepPHQ2)
SleepPHQ12$TotalHrAsleep=SleepPHQ12$TotalMinutesAsleep/60
SleepPHQ12$SRvFB=SleepPHQ12$sleep24h/SleepPHQ12$TotalHrAsleep
SleepPHQ12$SRvFBbin=SleepPHQ12$SRvFB>1
SPfactorcols=c("interest","down","asleep","tired","appetite","failure","concentr","activity","suic")
# way one: factorize the scores (0,1,2,3)
SleepPHQ12[SPfactorcols]<-lapply(SleepPHQ12[SPfactorcols],as.factor)
# way two: binarize the scores (0,1)
SleepPHQ12[SPfactorcols]=ifelse(SleepPHQ12[SPfactorcols]>0,1,0)
m1<-glmer(interest ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m1)
m2<-glmer(down ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m2)
m3<-glmer(asleep ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m3)
m4<-glmer(tired ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m4)
m5<-glmer(appetite ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m5)
m6<-glmer(failure ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m6)
m7<-glmer(concentr ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m7)
m8<-glmer(activity ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m8)
m9<-glmer(suic ~ SRvFBbin + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m9)                                                                           

m1<-glmer(interest ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m1)
m2<-glmer(down ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m2)
m3<-glmer(asleep ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m3)
m4<-glmer(tired ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m4)
m5<-glmer(appetite ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m5)
m6<-glmer(failure ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m6)
m7<-glmer(concentr ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m7)
m8<-glmer(activity ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m8)
m9<-glmer(suic ~ SRvFB + (1|UserID),data=SleepPHQ12,family=binomial,nAGQ=0);summary(m9) 

# Average Sleep on the week before PHQ date 2
######## Sleep change over time, compare self-report pattern and fitbit record pattern ########
SRsleep<-PHQSleep[c("UserID","sleep24h0","sleepAve0","sleep24h1","sleepAve1","sleep24h2","sleepAve2")]
PHQtimes<-PHQSleep[c("UserID","Year","surveyDate1","surveyDate2")]
PHQtimes$surveyDate1[which(PHQtimes$Year=="2014" & is.na(PHQtimes$surveyDate1))]=
  mean(PHQtimes$surveyDate1[which(PHQtimes$Year=="2014")],na.rm=T)
PHQtimes$surveyDate1[which(PHQtimes$Year=="2015" & is.na(PHQtimes$surveyDate1))]=
  mean(PHQtimes$surveyDate1[which(PHQtimes$Year=="2015")],na.rm=T)
PHQtimes$surveyDate2[which(PHQtimes$Year=="2014" & is.na(PHQtimes$surveyDate2))]=
  mean(PHQtimes$surveyDate2[which(PHQtimes$Year=="2014")],na.rm=T)
PHQtimes$surveyDate2[which(PHQtimes$Year=="2015" & is.na(PHQtimes$surveyDate2))]=
  mean(PHQtimes$surveyDate2[which(PHQtimes$Year=="2015")],na.rm=T)
tmpSleep<-merge(Sleep,PHQtimes,by.x="Id",by.y="UserID")
nsub<-nrow(PHQtimes)
FBsleep<-data.frame(UserID=integer(nsub),baseasleep=numeric(nsub),q1asleep=numeric(nsub),q2asleep=numeric(nsub),
                    baseinbed=numeric(nsub),q1inbed=numeric(nsub),q2inbed=numeric(nsub))
for (i in 1:nsub){
  print(i)
  FBsleep$UserID[i]<-PHQtimes$UserID[i]
  subSleep=tmpSleep[which(tmpSleep$Id==FBsleep$UserID[i]),]
  FBsleep$baseasleep[i]<-mean(subSleep$TotalHrAsleep[which(subSleep$SleepDay<=subSleep$StartDate)])
  FBsleep$q1asleep[i]  <-mean(subSleep$TotalHrAsleep[which(subSleep$SleepDay>subSleep$StartDate & subSleep$SleepDay<=subSleep$surveyDate1)])
  FBsleep$q2asleep[i]  <-mean(subSleep$TotalHrAsleep[which(subSleep$SleepDay>subSleep$surveyDate1 & subSleep$SleepDay<=subSleep$surveyDate2)])
  FBsleep$baseinbed[i] <-mean(subSleep$TotalHrInBed[which(subSleep$SleepDay<=subSleep$StartDate)])
  FBsleep$q1inbed[i]   <-mean(subSleep$TotalHrInBed[which(subSleep$SleepDay>subSleep$StartDate & subSleep$SleepDay<=subSleep$surveyDate1)])
  FBsleep$q2inbed[i]   <-mean(subSleep$TotalHrInBed[which(subSleep$SleepDay>subSleep$surveyDate1 & subSleep$SleepDay<=subSleep$surveyDate2)])
}
SRFBsleep=merge(SRsleep,FBsleep,by="UserID")
SRFBmean=colMeans(SRFBsleep,na.rm=T)
SRFBsd=colSds(as.matrix(SRFBsleep),na.rm=T)
ylimits=range(c(SRFBmean[c(2:13)]-SRFBsd[c(2:13)],SRFBmean[c(2:13)]+SRFBsd[c(2:13)]))
ylimits=c(4,10)
x=c(0:2)
plot(x,SRFBmean[c(2,4,6)],type="b",col="red",ylim=ylimits,
     xlab="time\n0=baseline,1=quarter1,2=quarter2",ylab="Hour")
arrows(x, SRFBmean[c(2,4,6)]-SRFBsd[c(2,4,6)], x, SRFBmean[c(2,4,6)]+SRFBsd[c(2,4,6)], length=0.05, angle=90, code=3,col="red")
lines(x,SRFBmean[c(3,5,7)],type="b",col="orange",ylim=ylimits)
arrows(x, SRFBmean[c(3,5,7)]-SRFBsd[c(3,5,7)], x, SRFBmean[c(3,5,7)]+SRFBsd[c(3,5,7)], length=0.05, angle=90, code=3,col="orange")
lines(x,SRFBmean[c(8:10)],type="b",col="blue",ylim=ylimits)
arrows(x, SRFBmean[c(8:10)]-SRFBsd[c(8:10)], x, SRFBmean[c(8:10)]+SRFBsd[c(8:10)], length=0.05, angle=90, code=3,col="blue")
lines(x,SRFBmean[c(11:13)],type="b",col="green",ylim=ylimits)
arrows(x, SRFBmean[c(11:13)]-SRFBsd[c(11:13)], x, SRFBmean[c(11:13)]+SRFBsd[c(11:13)], length=0.05, angle=90, code=3,col="green")
legend(0.1,6,c("selfreport_day","selfreport_week","fitbit_asleep","fitbit_inbed"),lty=c(1,1,1),lwd=c(2,2,2),col=c("red","orange","blue","green"))

wilcox.test(SRFBsleep$sleep24h0,SRFBsleep$baseasleep,paired = T)
wilcox.test(SRFBsleep$sleep24h0,SRFBsleep$baseinbed,paired = T)
wilcox.test(SRFBsleep$sleepAve0,SRFBsleep$baseasleep,paired = T)
wilcox.test(SRFBsleep$sleepAve0,SRFBsleep$baseinbed,paired=T)

wilcox.test(SRFBsleep$sleep24h1,SRFBsleep$q1asleep,paired = T)
wilcox.test(SRFBsleep$sleep24h1,SRFBsleep$q1inbed,paired = T)
t.test(SleepPHQ1$sleep24h1,SleepPHQ1$TotalHrAsleep,paired=T)
wilcox.test(SRFBsleep$sleepAve1,SRFBsleep$q1asleep,paired = T)
t.test(SRFBsleep$sleepAve1,SRFBsleep$q1inbed,paired = T)
wilcox.test(Sleepave1$sleepsr,Sleepave1$sleepfb,paired=T)

t.test(SleepPHQ2$sleep24h2,SleepPHQ2$TotalHrAsleep,paired=T)
wilcox.test(SRFBsleep$sleep24h2,SRFBsleep$q2inbed,paired = T)
wilcox.test(SRFBsleep$sleep24h2,SRFBsleep$q2asleep,paired = T)
wilcox.test(SRFBsleep$sleepAve2,SRFBsleep$q2asleep,paired = T)
wilcox.test(Sleepave2$sleepsr,Sleepave2$sleepfb,paired=T)
wilcox.test(SRFBsleep$sleepAve2,SRFBsleep$q2inbed,paired=T)
############ PHQ & Activity ############
PHQActivity <- PHQ[which(PHQ$UserID %in% Activity$Id),]
PHQActivity$surveyDate1<-as.Date(as.character(PHQActivity$surveyDate1),"%d%b%y")
PHQActivity$surveyDate2<-as.Date(as.character(PHQActivity$surveyDate2),"%d%b%y")
ActivityPHQ1=merge(PHQActivity,Activity,by.x=c("UserID","surveyDate1"),by.y=c("Id","ActivityDate"))
ActivityPHQ1=ActivityPHQ1[c("UserID","surveyDate1","Year","sleep24h1","sleepAve1","interest1","down1","asleep1","tired1","appetite1","failure1","concentr1","activity1","suic1","PHQtot1",
                            "TotalSteps","TotalDistance","TrackerDistance","LoggedActivitiesDistance","VeryActiveDistance","ModeratelyActiveDistance","LightActiveDistance",
                            "SedentaryActiveDistance","VeryActiveMinutes","FairlyActiveMinutes","LightlyActiveMinutes","SedentaryMinutes","Calories")]
# way one: factorize the scores (0,1,2,3)
ActivityPHQ1[SPfactorcols]<-lapply(ActivityPHQ1[SPfactorcols],as.factor)
# way two: binarize the scores (0,1)
ActivityPHQ1[SPfactorcols]=ifelse(ActivityPHQ1[SPfactorcols]>0,1,0)
ActivityPHQ1$TotalStepslog=log10(ActivityPHQ1$TotalSteps)
##### predict variables: TotalStepslog Calories SedentaryMinutes ####
m1=glm(interest1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m1)
m2=glm(down1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m2)
m3=glm(asleep1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m3)
m4=glm(tired1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m4)
m5=glm(appetite1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m5)
m6=glm(failure1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m6)
m7=glm(concentr1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m7)
m8=glm(activity1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m8)
m9=glm(suic1 ~ SedentaryMinutes, family=binomial(link='logit'),data=ActivityPHQ1);summary(m9)
m10=lm(PHQtot1 ~ SedentaryMinutes, data=ActivityPHQ1);summary(m10)
#### average past week ####
Activityave=merge(Activity,PHQActivity,by.x="Id",by.y="UserID")
Activityave1=Activityave[which((Activityave$surveyDate1 - Activityave$ActivityDate >=0) & (Activityave$surveyDate1-Activityave$ActivityDate<7)),]
subs=unique(Activityave1$Id)
nsub=length(subs)
for (isub in 1:nsub){
  tmp=Activityave1[which(Activityave1$Id==subs[isub]),]
  newtmp=data.frame(id=subs[isub],TotalStepslog=mean(log10(tmp$TotalSteps)),Calories=mean(tmp$Calories),
                    SedentaryMinutes=mean(tmp$SedentaryMinutes),
                    PHQtot1=tmp$PHQtot1[1],interest1=tmp$interest1[1],down1=tmp$down1[1],asleep1=tmp$asleep1[1],tired1=tmp$tired1[1],
                    appetite1=tmp$appetite1[1],failure1=tmp$failure1[1],concentr1=tmp$concentr1[1],activity1=tmp$activity1[1],suic1=tmp$suic1[1])
  if (isub==1){out=newtmp}
  else {out=rbind(out,newtmp)}
}
Activityave1=out
# way one: factorize the scores (0,1,2,3)
Activityave1[SPfactorcols]<-lapply(Activityave1[SPfactorcols],as.factor)
# way two: binarize the scores (0,1)
Activityave1[SPfactorcols]=ifelse(Activityave1[SPfactorcols]>0,1,0)
##### predict variables: TotalStepslog Calories SedentaryMinutes ####
m1=glm(interest1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m1)
m2=glm(down1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m2)
m3=glm(asleep1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m3)
m4=glm(tired1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m4)  # sig: sedentarymin
m5=glm(appetite1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m5)
m6=glm(failure1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m6)
m7=glm(concentr1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m7)
m8=glm(activity1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m8)
m9=glm(suic1 ~ Calories, family=binomial(link='logit'),data=Activityave1);summary(m9)
m10=lm(PHQtot1 ~ Calories, data=Activityave1);summary(m10)
############ PHQ & Mood ############
PHQMood <- merge(PHQ, Summary.mood, by.x="UserID",by.y="id")
m1<-lm(RR ~ PHQtot0, data=PHQMood);summary(m1)

############ PHQ change & Activity and sleep ############
PHQ.BS=PHQ.BS[c("UserID","Year","Age","Sex","Ethnicity","Marital","Child","surveyDate0","surveyDate1","surveyDate2","PHQtot0","PHQtot1","PHQtot2",
                "PHQdate_BS1","PHQtot_BS1","PHQdate_BS2","PHQtot_BS2")]
PHQ.BS=PHQ.BS[which(!is.na(PHQ.BS$PHQtot0)),]
PHQ.BS$PHQchange<-rowMeans(subset(PHQ.BS,select=c("PHQtot1","PHQtot2","PHQtot_BS1","PHQtot_BS2")),na.rm=TRUE)-PHQ.BS$PHQtot0
######## Compare mean and variance ########
PHQ.BS.phq=PHQ.BS[c("PHQtot0","PHQtot_BS1","PHQtot1","PHQtot_BS2","PHQtot2")]
PHQ.BS.phqmean=colMeans(PHQ.BS.phq,na.rm = T)
PHQ.BS.phqsd=colSds(as.matrix(PHQ.BS.phq),na.rm=T)
par(mfrow=c(2,1))
plot(c(1:5),as.vector(PHQ.BS.phqmean),xlab="",ylab="mean",main="PHQ",xaxt="n",ylim=c(3,7))
axis(1,labels=FALSE)
labels=c("baseline","BS1","Q1","BS2","Q2")
text(1:5,1.8, srt = 0, adj = 0.5,labels = labels, xpd = TRUE)
mtext(1, text = "Date", line = 3)
plot(c(1:5),as.vector(PHQ.BS.phqsd),xlab="Date",ylab="sd",xaxt="n",ylim=c(2,5))
axis(1,labels=FALSE)
text(1:5,1, srt = 0, adj = 0.5,labels = labels, xpd = TRUE)
#mtext(1, text = "Date", line = 3)
######## penalized functional regression ########
PHQoutcome=PHQ.BS[c("UserID","Year","Age","Sex","Ethnicity","Marital","Child","PHQchange")]
#PHQoutcome=PHQoutcome[which(!is.na(PHQoutcome$Age) & !is.na(PHQoutcome$Sex)),]
dPS=merge(Sleep,PHQoutcome,by.x="Id",by.y="UserID")
dPA=merge(Activity,PHQoutcome,by.x="Id",by.y="UserID")
dPM=merge(Mood,PHQoutcome,by.x="userid",by.y="UserID")
dPM$userid=as.integer(dPM$userid)
dPS.sleeptime.model<-funreg(id=dPS$Id,response=dPS$PHQchange,time=dPS$day,x=dPS$TotalHrAsleep,
                            deg=2,family="gaussian");currModel=dPS.sleeptime.model
dPS.sleepeff.model<-funreg(id=dPS$Id,response=dPS$PHQchange,time=dPS$day,x=dPS$Efficiency,
                           deg=2,family="gaussian");currModel=dPS.sleepeff.model  # have to have enough points in one bin, adjusted by num.bins
dPM.mood.model<-funreg(id=dPM$userid,response=dPM$PHQchange,time=dPM$day,x=dPM$mood,
                       deg=2,family="gaussian");currModel=dPM.mood.model
dPA.step.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$TotalSteps,
                       deg=2,family="gaussian");currModel=dPA.step.model 
dPA.sed.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$SedentaryMinutes,
                      deg=2,family="gaussian");currModel=dPA.sed.model 
dPA.cal.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$Calories,
                      deg=2,family="gaussian");currModel=dPA.cal.model
dPA.dist.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$TotalDistance,
                      deg=2,family="gaussian");currModel=dPA.dist.model
dPA.very.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$VeryActiveMinutes,
                       deg=2,family="gaussian");currModel=dPA.very.model
dPA.fair.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$FairlyActiveMinutes,
                       deg=2,family="gaussian");currModel=dPA.fair.model
dPA.light.model<-funreg(id=dPA$Id,response=dPA$PHQchange,time=dPA$day,x=dPA$LightlyActiveMinutes,
                       deg=2,family="gaussian");currModel=dPA.light.model

dPScov=dPS[which(!is.na(dPS$Age) & !is.na(dPS$Sex)),]
dPMcov=dPM[which(!is.na(dPM$Age.x) & !is.na(dPM$Sex.x)),]
dPScov.sleepeff.model<-funreg(id=dPScov$Id,response=dPScov$PHQchange,time=dPScov$day,x=dPScov$Efficiency,
                              other.covariates=dPScov[,c("Sex","Age","Ethnicity","Marital","Child")],deg=2,family="gaussian",
                           num.bins=20);currModel=dPScov.sleepeff.model  # have to have enough points in one bin, adjusted by num.bins
dPMcov.mood.model<-funreg(id=dPMcov$userid,response=dPMcov$PHQchange,time=dPMcov$day,x=dPMcov$mood,
                          other.covariates=dPMcov[,c("Sex.x","Age.x","Ethnicity.x","Marital.x","Child.x")],deg=2,family="gaussian",
                          num.bins=20);currModel=dPMcov.mood.model
#### Presentation of the model ####
par(mfrow=c(2,2))
plot(x=currModel$model.for.x[[1]]$bin.midpoints,y=currModel$model.for.x[[1]]$mu.x.by.bin,xlab="Day",ylab="X(t)",
     main="Smoothed mean x values")
plot(currModel,type="correlations",xlab="Day")
plot(currModel,type="coefficients",xlab="Day")
plot(currModel$subject.info$response,currModel$subject.info$fitted,
     main="Predictive Performance",xlab="True dPHQ",ylab="Fitted dPHQ")
abline(0,1)
summarymodel=summary(currModel)
modelperm=funreg.permutation(currModel)
############ compare personal average Mood/Act/Sleep ~ Internship & Gender ############
basicpheno=PHQ[c("UserID","Age","Sex")]
library(reshape2);
#### Mood ####
Summary.mood.wgender=merge(Summary.mood,basicpheno,by.x="id",by.y="UserID")
Summary.mood.wgender=Summary.mood.wgender[c("id","meanMood","meanMood.pre","meanMood.post","Age","Sex","dmeanmood")]
nrow(Summary.mood);nrow(Summary.mood.wgender)
##boxplots
boxplot(meanMood~Sex,Summary.mood.wgender,xlab="Gender",ylab="meanMood")
boxplot(dmeanmood~Sex,Summary.mood.wgender,xlab="Gender",ylab="dmeanMood")
Summary.mood.wgender.reshape=melt(Summary.mood.wgender[c("meanMood.pre","meanMood.post","Sex")],id.var="Sex")
Summary.mood.wgender.reshapeplot=Summary.mood.wgender.reshape[which(!is.na(Summary.mood.wgender.reshape$Sex)),]
Summary.mood.wgender.reshapeplot$value=as.numeric(Summary.mood.wgender.reshapeplot$value)
boxplot(value ~ Sex*variable,Summary.mood.wgender.reshapeplot)
##ttest
meanmood.f=Summary.mood.wgender$meanMood[which(Summary.mood.wgender$Sex==2)]
meanmood.m=Summary.mood.wgender$meanMood[which(Summary.mood.wgender$Sex==1)]
t.test(meanmood.f,meanmood.m,paired=F)
meanmood.pre.f=Summary.mood.wgender$meanMood.pre[which(Summary.mood.wgender$Sex==2)]
meanmood.pre.m=Summary.mood.wgender$meanMood.pre[which(Summary.mood.wgender$Sex==1)]
t.test(meanmood.pre.f,meanmood.pre.m,paired=F)
meanmood.post.f=Summary.mood.wgender$meanMood.post[which(Summary.mood.wgender$Sex==2)]
meanmood.post.m=Summary.mood.wgender$meanMood.post[which(Summary.mood.wgender$Sex==1)]
t.test(meanmood.post.f,meanmood.post.m,paired=F)
dmeanmood.f=Summary.mood.wgender$dmeanmood[which(Summary.mood.wgender$Sex==2)]
dmeanmood.m=Summary.mood.wgender$dmeanmood[which(Summary.mood.wgender$Sex==1)]
t.test(dmeanmood.f,dmeanmood.m,paired=F)
#### Activity ####
Summary.activity.gender=merge(Summary.activity,basicpheno,by.x="id",by.y="UserID")
Summary.activity.gender=Summary.activity.wgender[c("id","meanSteps","meanSteps.pre","meanSteps.post","Age","Sex")]
nrow(Summary.activity);nrow(Summary.activity.gender)
Summary.activity.gender$dmeanSteps=Summary.activity.gender$meanSteps.post-Summary.activity.gender$meanSteps.pre
##boxplots
boxplot(meanSteps~Sex,Summary.activity.gender,xlab="Gender",ylab="meanSteps")
boxplot(dmeanSteps~Sex,Summary.activity.gender,xlab="Gender",ylab="dmeanSteps")
Summary.activity.gender.reshape=melt(Summary.activity.gender[c("meanSteps.pre","meanSteps.post","Sex")],id.var="Sex")
Summary.activity.gender.reshapeplot=Summary.activity.gender.reshape[which(!is.na(Summary.activity.gender.reshape$Sex)),]
Summary.activity.gender.reshapeplot$value=as.numeric(Summary.activity.gender.reshapeplot$value)
boxplot(value ~ Sex*variable,Summary.activity.gender.reshapeplot)
##ttest
Summary.activity.gender<-Summary.activity.gender[which(Summary.activity.gender$id!="93033"),]
meanSteps.f=Summary.activity.gender$meanSteps[which(Summary.activity.gender$Sex==2)]
meanSteps.m=Summary.activity.gender$meanSteps[which(Summary.activity.gender$Sex==1)]
t.test(meanSteps.f,meanSteps.m,paired=F)
meanSteps.pre.f=Summary.activity.gender$meanSteps.pre[which(Summary.activity.gender$Sex==2)]
meanSteps.pre.m=Summary.activity.gender$meanSteps.pre[which(Summary.activity.gender$Sex==1)]
t.test(meanSteps.pre.f,meanSteps.pre.m,paired=F)
meanSteps.post.f=Summary.activity.gender$meanSteps.post[which(Summary.activity.gender$Sex==2)]
meanSteps.post.m=Summary.activity.gender$meanSteps.post[which(Summary.activity.gender$Sex==1)]
t.test(meanSteps.post.f,meanSteps.post.m,paired=F)
dmeanSteps.f=Summary.activity.gender$dmeanSteps[which(Summary.activity.gender$Sex==2)]
dmeanSteps.m=Summary.activity.gender$dmeanSteps[which(Summary.activity.gender$Sex==1)]
t.test(dmeanSteps.f,dmeanSteps.m,paired=F)
#### Sleep ####
Summary.sleep.gender=merge(Summary.sleep,basicpheno,by.x="id",by.y="UserID")
Summary.sleep.gender=Summary.sleep.gender[c("id","meanMinAsleep","meanMinAsleep.pre","meanMinAsleep.post",
                                              "meanAsleepInbedratio","meanAsleepInbedratio.pre","meanAsleepInbedratio.post","Age","Sex")]
nrow(Summary.sleep);nrow(Summary.sleep.gender)
Summary.sleep.gender$dmeanMinAsleep=Summary.sleep.gender$meanMinAsleep.post-Summary.sleep.gender$meanMinAsleep.pre
Summary.sleep.gender$dEff=Summary.sleep.gender$meanAsleepInbedratio.post-Summary.sleep.gender$meanAsleepInbedratio.pre
##boxplots
boxplot(meanMinAsleep~Sex,Summary.sleep.gender,xlab="Gender",ylab="meanMinAsleep")
boxplot(dmeanMinAsleep~Sex,Summary.sleep.gender,xlab="Gender",ylab="dmeanMinAsleep")
boxplot(meanAsleepInbedratio~Sex,Summary.sleep.gender,xlab="Gender",ylab="meanAsleepInbedratio")
boxplot(dEff~Sex,Summary.sleep.gender,xlab="Gender",ylab="dmeanAsleepInbedratio")
Summary.sleep.gender.reshape=melt(Summary.sleep.gender[c("meanMinAsleep.pre","meanMinAsleep.post","Sex")],id.var="Sex")
Summary.sleep.gender.reshapeplot=Summary.sleep.gender.reshape[which(!is.na(Summary.sleep.gender.reshape$Sex)),]
Summary.sleep.gender.reshapeplot$value=as.numeric(Summary.sleep.gender.reshapeplot$value)
boxplot(value ~ Sex*variable,Summary.sleep.gender.reshapeplot)
Summary.sleep.gender.reshape=melt(Summary.sleep.gender[c("meanAsleepInbedratio.pre","meanAsleepInbedratio.post","Sex")],id.var="Sex")
Summary.sleep.gender.reshapeplot=Summary.sleep.gender.reshape[which(!is.na(Summary.sleep.gender.reshape$Sex)),]
Summary.sleep.gender.reshapeplot$value=as.numeric(Summary.sleep.gender.reshapeplot$value)
boxplot(value ~ Sex*variable,Summary.sleep.gender.reshapeplot)
##ttest
#asleepmin
meanMinAsleep.f=Summary.sleep.gender$meanMinAsleep[which(Summary.sleep.gender$Sex==2)]
meanMinAsleep.m=Summary.sleep.gender$meanMinAsleep[which(Summary.sleep.gender$Sex==1)]
t.test(meanMinAsleep.f,meanMinAsleep.m,paired=F)
meanMinAsleep.pre.f=Summary.sleep.gender$meanMinAsleep.pre[which(Summary.sleep.gender$Sex==2)]
meanMinAsleep.pre.m=Summary.sleep.gender$meanMinAsleep.pre[which(Summary.sleep.gender$Sex==1)]
t.test(meanMinAsleep.pre.f,meanMinAsleep.pre.m,paired=F)
meanMinAsleep.post.f=Summary.sleep.gender$meanMinAsleep.post[which(Summary.sleep.gender$Sex==2)]
meanMinAsleep.post.m=Summary.sleep.gender$meanMinAsleep.post[which(Summary.sleep.gender$Sex==1)]
t.test(meanMinAsleep.post.f,meanMinAsleep.post.m,paired=F)
dmeanMinAsleep.f=Summary.sleep.gender$dmeanMinAsleep[which(Summary.sleep.gender$Sex==2)]
dmeanMinAsleep.m=Summary.sleep.gender$dmeanMinAsleep[which(Summary.sleep.gender$Sex==1)]
t.test(dmeanMinAsleep.f,dmeanMinAsleep.m,paired=F)
#efficiency
meanAsleepInbedratio.f=Summary.sleep.gender$meanAsleepInbedratio[which(Summary.sleep.gender$Sex==2)]
meanAsleepInbedratio.m=Summary.sleep.gender$meanAsleepInbedratio[which(Summary.sleep.gender$Sex==1)]
t.test(meanAsleepInbedratio.f,meanAsleepInbedratio.m,paired=F)
meanAsleepInbedratio.pre.f=Summary.sleep.gender$meanAsleepInbedratio.pre[which(Summary.sleep.gender$Sex==2)]
meanAsleepInbedratio.pre.m=Summary.sleep.gender$meanAsleepInbedratio.pre[which(Summary.sleep.gender$Sex==1)]
t.test(meanAsleepInbedratio.pre.f,meanAsleepInbedratio.pre.m,paired=F)
meanAsleepInbedratio.post.f=Summary.sleep.gender$meanAsleepInbedratio.post[which(Summary.sleep.gender$Sex==2)]
meanAsleepInbedratio.post.m=Summary.sleep.gender$meanAsleepInbedratio.post[which(Summary.sleep.gender$Sex==1)]
t.test(meanAsleepInbedratio.post.f,meanAsleepInbedratio.post.m,paired=F)
dEff.f=Summary.sleep.gender$dEff[which(Summary.sleep.gender$Sex==2)]
dEff.m=Summary.sleep.gender$dEff[which(Summary.sleep.gender$Sex==1)]
t.test(dEff.f,dEff.m,paired=F)

