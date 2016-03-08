library("numDeriv")
library("RCurl") ## to source() from Github
library("ggplot2"); theme_set(theme_bw())
library("reshape2")
library("plyr")
library("RColorBrewer")
library("RLRsim")
df=MoodActSleep[c("userid","mood","TotalStepslog","TotalhrAsleep","day")]
m1=lmer(mood ~ TotalStepslog + TotalhrAsleep + day + (1+day|userid),data=df,REML=FALSE)
length(getME(m1,"theta"))
length(fixef(m1))

dfs<-df
dfs[,c("day")]<-scale(dfs[,c("day")])
dfs$daysquare=dfs$day^2
m1_daysc<-lmer(mood ~ TotalStepslog + TotalhrAsleep + day + daysquare + (1+day|userid),data=dfs,REML=FALSE)
summary(m1_daysc)
attributes(summary(m1_daysc))

m1_daysc$pp


m=lmer(mood ~ TotalStepslog + TotalhrAsleep + day + daysquare + (1|userid),data=dfs,REML=FALSE)
m0=lm(mood ~ TotalStepslog + TotalhrAsleep + day + daysquare ,data=dfs)

anova(m1_daysc,m,m0)

