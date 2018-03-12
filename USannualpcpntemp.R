
dir()

#1) Can we see climate warming for ourselves by analyzing these data?
#2) In what parts of the country are temperatures getting warmer? Are there any parts that actually got colder over 1950-2010?
#3) Can we say, based on these data, What change has occurred in precipitation over the period?

#packages---------
#***********************************************
library(maptools)
data(wrld_simpl)
library(colorRamps)
library(dplyr)


#upload dataframes---------
#***********************************************

precip<-readRDS("USAAnnualPcpn1950_2008.rds")
temp<-readRDS("USAAnnualTemp1950_2008.rds")
ClmtRgns<-read.csv("ClimateRegionsUSA.csv")

#Clean data
#***********************************************
WthrStnClean<-function(df){
  df<-na.exclude(df)
  df["latlon"]<-paste(df$lat,df$lon)
  df$latlon<-as.factor(df$latlon)
  df["Count"]<-paste(1)
  df$Count<-as.numeric(df$Count)
  Countdf<-aggregate(df$Count,by=list(df$latlon),sum)

  Countdf["Include"]<-NA
  TimePeriod<-39

  for(i in 1:NROW(Countdf)){
    Countdf$Include[i]<-if(Countdf$x[i]<TimePeriod){0}else{1}
  }

  names(Countdf)[1]<-paste("latlon") 
  names(Countdf)[2]<-paste("NoYearData") 
  df<-left_join(df,Countdf,by="latlon")
  df<-subset(df,Include==1)
  }

temp<-WthrStnClean(temp)
precip<-WthrStnClean(precip)

#Objective 1---------
#***********************************************

MAT<-aggregate(temp$data,by=list(temp$year), mean)
names(MAT)[2]<-paste("data") 
names(MAT)[1]<-paste("year") 
MAT["ClimPrd"]<-NA
for(i in 1:NROW(MAT)){
  MAT$ClimPrd[i]<-if(MAT$year[i]<=1980){0}else{1}
}

par(mfrow=c(1,2))

d1950<-density(MAT$data[which(MAT$ClimPrd==0)])
y1950<-approxfun(d1950$x, d1950$y)
ymax1950<-y1950(mean(MAT$data[which(MAT$ClimPrd==0)]))

d1981<-density(MAT$data[which(MAT$ClimPrd==1)])
y1981<-approxfun(d1981$x, d1981$y)
ymax1981<-y1981(mean(MAT$data[which(MAT$ClimPrd==1)]))

plot(density(MAT$data[which(MAT$ClimPrd==0)])
     ,xlim=c(min(MAT$data),max(MAT$data))
     ,las=1,xlab="Temperature (F)", ylab="Density"
     ,main="",lty=3,lwd=2,col="grey27")
segments(mean(MAT$data[which(MAT$ClimPrd==0)]),0,
         mean(MAT$data[which(MAT$ClimPrd==0)]), 
         ymax1950,lwd=2,lty=3)
lines(density(MAT$data[which(MAT$ClimPrd==1)]), col="red", xlim=c(min(MAT$data),max(MAT$data)), lwd=2)
segments(mean(MAT$data[which(MAT$ClimPrd==1)]),0,
         mean(MAT$data[which(MAT$ClimPrd==1)]), 
         ymax1981,lwd=2,col="red")

ClmTempDiff<-mean(MAT$data[which(MAT$ClimPrd==1)])-mean(MAT$data[which(MAT$ClimPrd==0)])

boxplot(data~ClimPrd, data=MAT, col=c("grey27","red"), las=1
        ,xlab="Climate Period", xaxt="n", ylab="Temperature (F)")
axis(1,c(1,2),c("1950-80","1981-2008"))


#Objective 2---------
#***********************************************

temp<-left_join(temp,ClmtRgns,by="state")
MAT.region<-aggregate(temp$data,by=list(temp$year,temp$region), mean)
names(MAT.region)[3]<-paste("data") 
names(MAT.region)[1]<-paste("year") 
names(MAT.region)[2]<-paste("region") 

MAT.region["ClimPrd"]<-NA
for(i in 1:NROW(MAT.region)){
  MAT.region$ClimPrd[i]<-if(MAT.region$year[i]<=1980){0}else{1}
}


summary(lmMAT.region<-lm(data~ClimPrd*region, data=MAT.region))
anova(lmMAT.region)
plot(lmMAT.region, which=c(1,2))
shapiro.test(resid(lmMAT.region))

NumStates<-NROW(unique(ClmtRgns$state))
NumRegions<-NROW(unique(ClmtRgns$region))
StateColors<-primary.colors(NumStates)
RegionColors<-rainbow(NumRegions)

interaction.plot(MAT.region$ClimPrd,MAT.region$region,MAT.region$data, col=RegionColors, lwd=3, las=1, ylab="Temperature (F)",xlab="Climate Period", xaxt="n")
axis(1,c(1,2),c("1950-80","1981-2008"))

#Objective 3---------
#***********************************************
plot(data~year, data=precip)
