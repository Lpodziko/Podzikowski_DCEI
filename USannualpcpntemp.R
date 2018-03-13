
dir()

#1) Can we see climate warming for ourselves by analyzing these data?
#2) In what parts of the country are temperatures getting warmer? Are there any parts that actually got colder over 1950-2010?
#3) Can we say, based on these data, What change has occurred in precipitation over the period?

#packages---------
#***********************************************
library(maptools)
#data(wrld_simpl)
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
  TimePeriod<-40

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

meanUSweather<-function(df){
  MAT<-aggregate(df$data,by=list(df$year), mean)
  names(MAT)[2]<-paste("data") 
  names(MAT)[1]<-paste("year") 
  MAT["ClimPrd"]<-NA
  for(i in 1:NROW(MAT)){
    MAT$ClimPrd[i]<-if(MAT$year[i]<=1980){0}else{1}
  }
  return(MAT)
}

MAT.temp<-meanUSweather(temp)

Xaxislab.plota<-"Climate Period"
Xaxiscat.plota<-c("1950-80","1981-2008")
Yaxislab.plota<-"Mean Annual Temp. (F)"
Xaxislab.plotb<-"Mean Annual Temp.(F)"
Yaxislab.plotb<-"Density"
ClmtPrd1color<-"grey27"
ClmtPrd2color<-"red"
plotname<-"Objective1.png"
plotcolors<-c(ClmtPrd1color,ClmtPrd2color)

ClmtPrd1<-c(MAT.temp$data[which(MAT.temp$ClimPrd==0)])
ClmtPrd2<-c(MAT.temp$data[which(MAT.temp$ClimPrd==1)])


plotfnct<-function(df){
  d1950<-density(df$data[which(df$ClimPrd==0)])
  y1950<-approxfun(d1950$x, d1950$y)
  ymax1950<-y1950(mean(df$data[which(df$ClimPrd==0)]))
  d1981<-density(df$data[which(df$ClimPrd==1)])
  y1981<-approxfun(d1981$x, d1981$y)
  ymax1981<-y1981(mean(df$data[which(df$ClimPrd==1)]))

  png(plotname, width = 7, height = 4, units = 'in', res = 800)
  layout(matrix(1:2, ncol = 2), widths = 1, heights = 1, respect = FALSE)
  par(mar = c(4.3, 2.1, 1.4, 0), oma=c(3,3,3,3))
  boxplot(data~ClimPrd, data=df, col=plotcolors, las=1
          ,xlab=Xaxislab.plota, xaxt="n", ylab="", yaxt="n")
  axis(1,c(1,2),Xaxiscat.plota)
  axis(2, las=1)
  mtext(side=2,Yaxislab.plota, line=2.85)
  par(mar = c(4.3, 0, 1.4, 2.1))
  plot(density(ClmtPrd1)
       ,xlim=c(min(df$data),max(df$data))
       ,las=1,xlab=Xaxislab.plotb, ylab="",yaxt="n"
       ,main="",lty=3,lwd=2,col=ClmtPrd1color)
  segments(mean(ClmtPrd1),0, mean(ClmtPrd1), 
           ymax1950,lwd=2,lty=3,col=ClmtPrd1color)
  lines(density(ClmtPrd2), col=ClmtPrd2color, 
        xlim=c(min(df$data),max(df$data)), lwd=2)
  segments(mean(ClmtPrd2),0,mean(ClmtPrd2), 
           ymax1981,lwd=2,col=ClmtPrd2color)
  axis(side=4,las=1)
  mtext(side=4,Yaxislab.plotb,line=2.75)
  dev.off()
}

plotfnct(MAT.temp)

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

MAT.rgnprd<-aggregate(MAT.region$data,by=list(MAT.region$ClimPrd,MAT.region$region), mean)
names(MAT.rgnprd)[3]<-paste("data") 
names(MAT.rgnprd)[1]<-paste("ClimPrd") 
names(MAT.rgnprd)[2]<-paste("region") 
ClmtPrd0<-(MAT.rgnprd$data[which(MAT.rgnprd$ClimPrd==0)])
ClmtPrd1<-(MAT.rgnprd$data[which(MAT.rgnprd$ClimPrd==1)])

MAT.diff.rgnprd<-aggregate(MAT.rgnprd$data,by=list(MAT.rgnprd$region),diff)
names(MAT.diff.rgnprd)[1]<-paste("region") 
names(MAT.diff.rgnprd)[2]<-paste("changetemp")
MAT.diff.rgnprd<-MAT.diff.rgnprd[rev(order(MAT.diff.rgnprd$changetemp)),]

#plots

layout(matrix(1:2, ncol = 2), widths = 1, heights = 1, respect = FALSE)
par(mar = c(4.3, 2.1, 1.4, 0), oma=c(3,3,3,3))
NumRegions<-NROW(unique(ClmtRgns$region))
RegionColors<-heat.colors(NumRegions)
interaction.plot(MAT.region$ClimPrd,MAT.region$region,MAT.region$data, col=RegionColors, lwd=3, las=1, ylab="",xlab="", xaxt="n", legend=T, yaxt="n")
axis(side=2, las=1)
mtext("Mean Annual Temp. (F)", side=2, line=2.25)
mtext("Climate Period", side=1, line=2)
xvalclmtprd0<-rep(1,NROW(ClmtPrd0))
xvalclmtprd1<-rep(2,NROW(ClmtPrd1))
#points(xvalclmtprd0,ClmtPrd0, pch=22)
#points(xvalclmtprd1,ClmtPrd1, pch=22)
axis(1,c(1,2),c("1950-80","1981-2008"))
par(mar = c(4.3, 0, 1.4, 2.1))
barplotvector<-MAT.diff.rgnprd$changetemp
barplotlabels<-MAT.diff.rgnprd$region
barplot(barplotvector, col=RegionColors, las=1, 
        ylim=c(0,max(MAT.diff.rgnprd$changetemp)+0.25),
        yaxt="n")
xlabarplot<-c(0.7,1.93,3.17,4.33,5.48,6.8,7.88,9.11,10.34,11.58,12.81)
text(cex=1, x=xlabarplot, y=-0.05, barplotlabels, xpd=TRUE, srt=90, adj=1)
mtext(side=4,"Change Mean Annual Temp. (F)", line=2.5)
axis(side=4, las=1)
box()


#Objective 3---------
#***********************************************
MAT.precip<-meanUSweather(precip)

Xaxislab.plota<-"Climate Period"
Xaxiscat.plota<-c("1950-80","1981-2008")
Yaxislab.plota<-"Mean Annual Precip. (in)"
Xaxislab.plotb<-"Mean Annual Precip.(in)"
Yaxislab.plotb<-"Density"
ClmtPrd1color<-"grey27"
ClmtPrd2color<-"red"
plotname<-"Objective1.png"
plotcolors<-c(ClmtPrd1color,ClmtPrd2color)

ClmtPrd1<-c(MAT.precip$data[which(MAT.precip$ClimPrd==0)])
ClmtPrd2<-c(MAT.precip$data[which(MAT.precip$ClimPrd==1)])

plotfnct(MAT.precip)

