library(RColorBrewer)
library(classInt)

# Data for study
df = read.csv("data.csv",header=T,colClasses="character")

# Create variables
df$TOTALENROLLMENT = as.numeric(df$TOTALENROLLMENT)

df$ECEENROLLMENT = as.numeric(df$ECEENROLLMENT)
df$ECEPCT = df$ECEENROLLMENT/df$TOTALENROLLMENT

df$SPEDENROLLMENT = as.numeric(df$SPEDENROLLMENT)
df$SPEDPCT = df$SPEDENROLLMENT/df$TOTALENROLLMENT

df$ELLENROLLMENT = as.numeric(df$ELLENROLLMENT)
df$ELLPCT = df$ELLENROLLMENT/df$TOTALENROLLMENT

df$ATRISKENROLLMENT = as.numeric(df$ATRISKENROLLMENT)
df$ATRISKPCT = as.numeric(df$ATRISKPCT)

df$AMT_ENROLLMENT = as.numeric(df$AMT_ENROLLMENT)
df$AMT_ATRISK = as.numeric(df$AMT_ATRISK)

df = subset(df,df$LEVEL!="other")

### Look at change in at-risk funding and the change in enrollment funds
t1 = subset(df,YEAR=="2015",select=c("SCHOOLCODE","SCHOOLNAME","AMT_ENROLLMENT","TOTALENROLLMENT","ATRISKENROLLMENT","AMT_ATRISK"))
t2 = subset(df,YEAR=="2016",select=c("SCHOOLCODE","AMT_ENROLLMENT","TOTALENROLLMENT","ATRISKENROLLMENT","AMT_ATRISK"))
temp = merge(t1,t2,by="SCHOOLCODE")
chg.amt_atrisk = (temp$AMT_ATRISK.y - temp$AMT_ATRISK.x)
chg.amt_enroll = (temp$AMT_ENROLLMENT.y - temp$AMT_ENROLLMENT.x)
chg.enroll     = (temp$TOTALENROLLMENT.y - temp$TOTALENROLLMENT.x)/temp$TOTALENROLLMENT.x
plot(chg.amt_atrisk[idx],chg.amt_enroll[idx],col=(abs(chg.enroll[idx])<.01)+1,xlim=c(-2e6,2e6),ylim=c(-2e6,2e6))
abline(0,-1,col="red")
abline(h=0,col="gray")
abline(v=0,col="gray")


# Estimate model for 2015
res.csm = lm(log(AMT_ENROLLMENT)~
                log(TOTALENROLLMENT) + 
                ECEPCT + 
                LEVEL +
                FORTYFORTY,
            data=df,subset=YEAR=="2015")


