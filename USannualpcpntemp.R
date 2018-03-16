
dir()

#2) In what parts of the country are temperatures getting warmer? Are there any parts that actually got colder over 1950-2010?
#3) Can we say, based on these data, What change has occurred in precipitation over the period?

#packages---------
#***********************************************
library(maptools)
#data(wrld_simpl)
library(colorRamps)
library(dplyr)
library(RColorBrewer)


#upload dataframes---------
#***********************************************

precip<-readRDS("USAAnnualPcpn1950_2008.rds")
temp<-readRDS("USAAnnualTemp1950_2008.rds")
ClmtRgns<-read.csv("ClimateRegionsUSA.csv")

#Clean data--------
#***********************************************

#Function to clean temperature and precipitation data:
#Step 1: remove NA from the dataframe
#Step 2: remove weather stations with less than two years data
#return: a cleaned weather matrix

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

#Can we see climate warming for ourselves by analyzing these data?
#Step 1: calculate mean annual tempearture [aggregate]
#Step 2: create a time period category [1950-1980, 1981-2010]
#Step 3: boxplot and density functions of MAT by climate period 


MAweather<-function(df){
  MAT<-aggregate(df$data,by=list(df$year), mean)
  names(MAT)[2]<-paste("data") 
  names(MAT)[1]<-paste("year") 
  MAT$ClimPrd<-cut(MAT$year, c((min(MAT$year)-1),1980,(max(MAT$year))+1), labels=c(0,1))
  return(MAT)
}

MAT.temp<-MAweather(temp)

#setting the many plot parameters
Xaxislab.plota<-"Climate Period"
Xaxiscat.plota<-c("1950-80","1981-2008")
Yaxislab.plota<-"MAT (F)"
Xaxislab.plotb<-"MAT (F)"
Yaxislab.plotb<-"Density"
ClmtPrd1color<-"grey27"
ClmtPrd2color<-"red"
plotname<-"Objective1.png"
plotcolors<-c(ClmtPrd1color,ClmtPrd2color)

ClmtPrd1<-c(MAT.temp$data[which(MAT.temp$ClimPrd==0)])
ClmtPrd2<-c(MAT.temp$data[which(MAT.temp$ClimPrd==1)])


plotfnct<-function(df){
  #Creating the density functions
  d1950<-density(df$data[which(df$ClimPrd==0)])
  y1950<-approxfun(d1950$x, d1950$y)
  ymax1950<-y1950(mean(df$data[which(df$ClimPrd==0)]))
  d1981<-density(df$data[which(df$ClimPrd==1)])
  y1981<-approxfun(d1981$x, d1981$y)
  ymax1981<-y1981(mean(df$data[which(df$ClimPrd==1)]))

  #Plotting
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

MAweather.RGN<-function(df){
  df<-left_join(df,ClmtRgns,by="state")
  df$ClimPrd<-cut(df$year, c((min(df$year)-1),1980,(max(df$year))+1), labels=c(0,1))
  MAT.rgnprd<-aggregate(df$data,by=list(df$ClimPrd,df$region), mean)
  names(MAT.rgnprd)[3]<-paste("data") 
  names(MAT.rgnprd)[1]<-paste("ClimPrd") 
  names(MAT.rgnprd)[2]<-paste("region")
  return(MAT.rgnprd)
}

MAweather.RGNdiff<-function(df){
  MAT.diff.rgnprd<-aggregate(df$data,by=list(df$region),diff)
  names(MAT.diff.rgnprd)[1]<-paste("region") 
  names(MAT.diff.rgnprd)[2]<-paste("changetemp")
  MAT.diff.rgnprd<-MAT.diff.rgnprd[rev(order(MAT.diff.rgnprd$changetemp)),]
  return(MAT.diff.rgnprd)
}

MAT.rgnprd<-MAweather.RGN(temp)
MAT.diff.rgnprd<-MAweather.RGNdiff(MAT.rgnprd)

ClmtPrd0<-(MAT.rgnprd$data[which(MAT.rgnprd$ClimPrd==0)])
ClmtPrd1<-(MAT.rgnprd$data[which(MAT.rgnprd$ClimPrd==1)])
ClmtPrd1labels<-(MAT.rgnprd$region[which(MAT.rgnprd$ClimPrd==1)])
clmtPrddf<-data.frame(ClmtPrd1, ClmtPrd1labels)
clmtPrddf<-clmtPrddf[rev(order(clmtPrddf$ClmtPrd1)),]
clmtPrddf["clmtPrddfabv"]<-c("HI","SE","S","W","C","SW","NW","NE","NEC","NWC","AK")
leftside<-c("","","S","","" ,"SW","","NE","","NWC","")
rightside<-c("HI","SE","","W","C","", "NW","","NEC","","AK")  
lablocations<-clmtPrddf$ClmtPrd1

NumRegions<-NROW(unique(ClmtRgns$region))
RegionColors<-heat.colors(NumRegions)
Rlabelsinteractionplot<-2.015
Llabelsinteractionplot<-0.985

PlotAyaxislabel<-"MAT (F)"
PlotAxaxislabel<-"Climate Period"
PlotAxaxis<-c("1950-80","1981-2008")

barplotvector<-MAT.diff.rgnprd$changetemp
barplotlabels<-MAT.diff.rgnprd$region
PlotBYaxislim<-c(0,max(MAT.diff.rgnprd$changetemp)+0.25)
xlab.barplot<-c(0.7,1.93,3.17,4.33,5.48,6.8,7.88,9.11,10.34,11.58,12.81)
PlotBaxislabel<-"Change MAT (F)"
plotname2<-"Objective2.png"

#plots
plotfnct2<-function(df){
  png(plotname2, width = 7, height = 4, units = 'in', res = 800)
  layout(matrix(1:2, ncol = 2), widths = 1, heights = 1, respect = FALSE)
  par(mar = c(4.3, 2.1, 1.4, 0), oma=c(3,3,3,3))
  with(df,interaction.plot(ClimPrd,region,data, lwd=3, las=1, ylab="",xlab="", xaxt="n", legend=F, yaxt="n"))
  text(Rlabelsinteractionplot,lablocations,rightside, adj=0, cex=0.6)
  text(Llabelsinteractionplot,lablocations,leftside, adj=1, cex=0.6)
  axis(side=2, las=1)
  mtext(PlotAyaxislabel, side=2, line=2.25)
  mtext(PlotAxaxislabel, side=1, line=2)
  xvalclmtprd0<-rep(1,NROW(ClmtPrd0))
  xvalclmtprd1<-rep(2,NROW(ClmtPrd1))
  axis(1,c(1,2),PlotAxaxis)
  par(mar = c(4.3, 0, 1.4, 2.1))
  barplot(barplotvector, col=RegionColors, las=1, 
          ylim=PlotBYaxislim,
          yaxt="n")
  text(cex=1, x=xlab.barplot, y=-0.05, barplotlabels, xpd=TRUE, srt=90, adj=1)
  mtext(side=4,PlotBaxislabel, line=2.5)
  axis(side=4, las=1)
  box()
  dev.off()
}

plotfnct2(MAT.rgnprd)

#Objective 3---------
#***********************************************
MAP<-MAweather(precip)

Xaxislab.plota<-"Climate Period"
Xaxiscat.plota<-c("1950-80","1981-2008")
Yaxislab.plota<-"MAP (in)"
Xaxislab.plotb<-"MAP (in)"
Yaxislab.plotb<-"Density"
ClmtPrd1color<-"grey27"
ClmtPrd2color<-"blue"
plotname<-"Objective3.png"
plotcolors<-c(ClmtPrd1color,ClmtPrd2color)

ClmtPrd1<-c(MAP$data[which(MAP$ClimPrd==0)])
ClmtPrd2<-c(MAP$data[which(MAP$ClimPrd==1)])

plotfnct(MAP)

MAweather.ST<-function(df){
  df<-left_join(df,ClmtRgns,by="state")
  df$ClimPrd<-cut(df$year, c((min(df$year)-1),1980,(max(df$year))+1), labels=c(0,1))
  MAT.rgnprd<-aggregate(df$data,by=list(df$ClimPrd,df$state), mean)
  names(MAT.rgnprd)[3]<-paste("data") 
  names(MAT.rgnprd)[1]<-paste("ClimPrd") 
  names(MAT.rgnprd)[2]<-paste("region")
  return(MAT.rgnprd)
}

MAP.rgnprd<-MAweather.ST(precip)
MAP.diff.rgnprd<-MAweather.RGNdiff(MAP.rgnprd)

ClmtPrd0<-(MAP.rgnprd$data[which(MAP.rgnprd$ClimPrd==0)])
ClmtPrd1<-(MAP.rgnprd$data[which(MAP.rgnprd$ClimPrd==1)])
ClmtPrd1labels<-(MAP.rgnprd$region[which(MAP.rgnprd$ClimPrd==1)])
clmtPrddf<-data.frame(ClmtPrd1, ClmtPrd1labels)
clmtPrddf<-clmtPrddf[rev(order(clmtPrddf$ClmtPrd1)),]
rightside<-c("AL","TN","OH")
leftside<-c("","","")
lablocations<-clmtPrddf$ClmtPrd1

NumRegions<-NROW(unique(ClmtRgns$region))
RegionColors<-rev(colorRampPalette(brewer.pal(9,'Blues'))(3))
Rlabelsinteractionplot<-2.015

PlotAyaxislabel<-"MAP (in)"
PlotAxaxislabel<-"Climate Period"
PlotAxaxis<-c("1950-80","1981-2008")

barplotvector<-MAP.diff.rgnprd$changetemp
barplotlabels<-MAP.diff.rgnprd$region
PlotBYaxislim<-c(0,max(MAP.diff.rgnprd$changetemp)+0.25)
xlab.barplot<-c(0.7,1.93,3.17)
PlotBaxislabel<-"Change MAP (F)"
plotname2<-"Objective3b.png"

plotfnct2(MAP.rgnprd)
