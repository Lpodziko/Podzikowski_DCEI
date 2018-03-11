
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
dim(temp)
precip<-na.exclude(precip)
dim(precip)

str(precip)
summary(precip)
str(temp)
summary(temp)

#Clean data
#***********************************************

NumStates<-NROW(unique(ClmtRgns$state))
NumRegions<-NROW(unique(ClmtRgns$region))
StateColors<-primary.colors(NumStates)
RegionColors<-rainbow(NumRegions)

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

print(tapply(MAT$data,MAT$ClimPrd,mean))
print(tapply(MAT$data,MAT$ClimPrd,sd))

quartz()
par(mfrow=c(2,1))
plot(density(MAT$data[which(MAT$ClimPrd==0)]), xlim=c(min(MAT$data),max(MAT$data)))
abline(v=mean((MAT$data[which(MAT$ClimPrd==0)])), col="red")
plot(density(MAT$data[which(MAT$ClimPrd==1)]), xlim=c(min(MAT$data),max(MAT$data)))
abline(v=mean((MAT$data[which(MAT$ClimPrd==1)])), col="red")

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

interaction.plot(MAT.region$ClimPrd,MAT.region$region,MAT.region$data, col=RegionColors, lwd=3, las=1)
