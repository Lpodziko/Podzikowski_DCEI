
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

temp<-na.exclude(temp)
temp["latlon"]<-paste(temp$lat,temp$lon)
temp$latlon<-as.factor(temp$latlon)
temp["Count"]<-paste(1)
temp$Count<-as.numeric(temp$Count)
CountTemp<-aggregate(temp$Count,by=list(temp$latlon),sum)

CountTemp["Include"]<-NA
TimePeriod<-39

for(i in 1:NROW(CountTemp)){
  CountTemp$Include[i]<-if(CountTemp$x[i]<TimePeriod){0}else{1}
}

names(CountTemp)[1]<-paste("latlon") 
names(CountTemp)[2]<-paste("NoYearData") 
temp<-left_join(temp,CountTemp,by="latlon")
temp<-subset(temp,Include==1)


precip<-na.exclude(precip)
precip["latlon"]<-paste(precip$lat,precip$lon)
precip$latlon<-as.factor(precip$latlon)
precip["Count"]<-paste(1)
precip$Count<-as.numeric(precip$Count)
Countprecip<-aggregate(precip$Count,by=list(precip$latlon),sum)

Countprecip["Include"]<-NA
TimePeriod<-39

for(i in 1:NROW(Countprecip)){
  Countprecip$Include[i]<-if(Countprecip$x[i]<TimePeriod){0}else{1}
}

names(Countprecip)[1]<-paste("latlon") 
names(Countprecip)[2]<-paste("NoYearData") 
precip<-left_join(precip,Countprecip,by="latlon")
precip<-subset(precip,Include==1)

#Objective 1---------
#***********************************************

MAT<-aggregate(temp$data,by=list(temp$year), mean)
names(MAT)[2]<-paste("data") 
names(MAT)[1]<-paste("year") 
plot(data~year, data=MAT, type="l")

MAT["ClimPrd"]<-NA
for(i in 1:NROW(MAT)){
  MAT$ClimPrd[i]<-if(MAT$year[i]<=1980){0}else{1}
}

print(meanMAT<-tapply(MAT$data,MAT$ClimPrd,mean))
print(sdMAT<-tapply(MAT$data,MAT$ClimPrd,sd))

summary(MAT$data[which(MAT$ClimPrd==0)])
summary(MAT$data[which(MAT$ClimPrd==1)])

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
MAT.region$ClimPrd<-as.numeric(MAT.region$ClimPrd)

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
