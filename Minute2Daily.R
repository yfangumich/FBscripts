setwd('Y:/Data Analysis/Yu Fang/IHS_data/fitbit/2017_Cohort/')
library(tidyr)
library(dplyr)
library(data.table)
writetable <- function(file,path){
  write.table(file,path,quote=F,row.names=F,col.names=T)
}

readtable <- function(path){
  return(read.table(path,header=T,stringsAsFactors = F))
}

remove_entry <- function(df){
  df$STUDY_METRIC_ID <- NULL
  df <- df[df$STUDY_ID==21 & df$STUDY_PRTCPT_ID>=300000,]  # study id = 1: 2016 cohort, <300000: test access code, not real subjects
  df <- df[!duplicated(df),]
  return(df)
}
separate_date <- function(df){
  df$START_DT <- df$STUDY_METRIC_MSR_START_DT
  df_sepdate <- separate(data=df,col=START_DT,into=c("MSR_START_Date","MSR_START_Time"),sep=" ")
  return(df_sepdate)
}
separate_hour <- function(df_sepdate){
  df_sephour <- separate(data=df_sepdate,col=MSR_START_Time,into=c("MSR_START_hh","MSR_START_mm","MSR_START_ss"),sep=":")
  return(df_sephour)
}

time_form <- function(timestring){
  timevalue <- as.POSIXct(timestring,format="%Y-%m-%d %H:%M:%S",tz="EST")
  return(timevalue)
}


# Step
step <- readtable('july_step_data.tsv')
step <- remove_entry(step)
step_sepdate <- separate_date(step)
step_sephour <- separate_hour(step_sepdate)
step_daily <- aggregate(STUDY_METRIC_MSR_VAL~STUDY_PRTCPT_ID+MSR_START_Date,step_sepdate,sum)
step_hourly <- aggregate(STUDY_METRIC_MSR_VAL~STUDY_PRTCPT_ID+MSR_START_Date+MSR_START_hh,step_sephour,sum)


# Sleep
sleep <- readtable('july_sleep_data.tsv')
sleep <- remove_entry(sleep)
sleep_sepdate <- separate_date(sleep)
sleep_sepdate <- sleep_sepdate[!duplicated(sleep_sepdate[c("STUDY_METRIC_MSR_END_DT","STUDY_PRTCPT_ID")]),] # temporary method
sleep_sepdate <- sleep_sepdate[order(sleep_sepdate$STUDY_PRTCPT_ID,sleep_sepdate$MSR_START_Date,sleep_sepdate$MSR_START_Time),]
sleep_sepdate$STUDY_METRIC_MSR_END_DT_lag1 <- dplyr::lag(sleep_sepdate$STUDY_METRIC_MSR_END_DT)
sleep_sepdate$STUDY_METRIC_MSR_END_DT_interval <- 
  difftime(time_form(sleep_sepdate$STUDY_METRIC_MSR_END_DT),
           time_form(sleep_sepdate$STUDY_METRIC_MSR_END_DT_lag1),units = c("mins"))
sleep_sepdate$SLEEP_PERIOD_START <- 0
sleep_sepdate$SLEEP_PERIOD_START[1] <- 1
sleep_sepdate$SLEEP_PERIOD_START[sleep_sepdate$STUDY_METRIC_MSR_END_DT_interval!=1] <- 1
sleep_sepdate$SLEEP_PERIOD_END <- 0
sleep_sepdate$SLEEP_PERIOD_END[nrow(sleep_sepdate)] <- 1
sleep_sepdate$SLEEP_PERIOD_END[dplyr::lead(sleep_sepdate$SLEEP_PERIOD_START)==1] <- 1
sleep_sepdate$STUDY_METRIC_MSR_INBED <- 1  # original VAL is 1/2/3 to indicated different stages, here is only to indicate if in bed, used for counting length later. Same as in the hourly data.
sleep_sepdate$STUDY_METRIC_MSR_ASLEEP <- 0;sleep_sepdate$STUDY_METRIC_MSR_ASLEEP[sleep_sepdate$STUDY_METRIC_MSR_VAL==1] <- 1
sleep_sepdate$STUDY_METRIC_MSR_RESTLESS <- 0;sleep_sepdate$STUDY_METRIC_MSR_RESTLESS[sleep_sepdate$STUDY_METRIC_MSR_VAL==2] <- 1
sleep_sepdate$STUDY_METRIC_MSR_AWAKE <- 0;sleep_sepdate$STUDY_METRIC_MSR_AWAKE[sleep_sepdate$STUDY_METRIC_MSR_VAL==3] <- 1
sleep_sepdate_count <- transform(sleep_sepdate,SLEEP_PERIOD=ave(SLEEP_PERIOD_START,FUN=cumsum))
sleep_period_start <- sleep_sepdate_count[sleep_sepdate_count$SLEEP_PERIOD_START==1,c("STUDY_METRIC_MSR_START_DT","SLEEP_PERIOD","STUDY_PRTCPT_ID")]
sleep_period_date <- sleep_sepdate_count[sleep_sepdate_count$SLEEP_PERIOD_END==1,c("MSR_START_Date","SLEEP_PERIOD","STUDY_PRTCPT_ID","STUDY_METRIC_MSR_END_DT")]# double check if start date and end date is different by: View(sleep_sepdate_count[sleep_sepdate_count$SLEEP_PERIOD_END==1,]), the ones ends at 23:59:00/30 are the ones at the last day
sleep_period_time <- merge(sleep_period_date,sleep_period_start,by=c("STUDY_PRTCPT_ID","SLEEP_PERIOD"),sort=F)
sleep_period_length <- aggregate(cbind(STUDY_METRIC_MSR_INBED,STUDY_METRIC_MSR_ASLEEP,STUDY_METRIC_MSR_RESTLESS,STUDY_METRIC_MSR_AWAKE)
                                 ~STUDY_PRTCPT_ID+SLEEP_PERIOD,sleep_sepdate_count,sum)
sleep_periods <- merge(sleep_period_length,sleep_period_time,by=c("STUDY_PRTCPT_ID","SLEEP_PERIOD"),sort=F) # main intermediate outcome
names(sleep_periods)[names(sleep_periods)=="MSR_START_Date"]="SLEEP_PERIOD_DATE" # eliminate the confusion caused by 'START'
sleep_daily <- aggregate(cbind(STUDY_METRIC_MSR_INBED,STUDY_METRIC_MSR_ASLEEP,STUDY_METRIC_MSR_RESTLESS,STUDY_METRIC_MSR_AWAKE)~SLEEP_PERIOD_DATE+STUDY_PRTCPT_ID,sleep_periods,sum)

sleep_sephour <- separate_hour(sleep_sepdate)
sleep_sephour$STUDY_METRIC_MSR_INBED <- 1  
sleep_sephour <- sleep_sephour[!duplicated(sleep_sephour[c("STUDY_METRIC_MSR_END_DT","STUDY_PRTCPT_ID")]),] # temporary method
sleep_hourly <- aggregate(STUDY_METRIC_MSR_inbed~STUDY_PRTCPT_ID+MSR_START_Date+MSR_START_hh,sleep_sephour,sum)
sleep_hourly <- sleep_hourly[order(sleep_hourly$STUDY_PRTCPT_ID,sleep_hourly$MSR_START_Date,sleep_hourly$MSR_START_hh),]
