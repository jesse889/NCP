# NCP
#Some script to calculate NCP based on MIMS data

Data<-read.csv("O2ArSept2018.csv",header=T,na.strings="NA")
ls(Data)

###################################################################
#Prep stuff to get 5 minute averages and data all paired up

Data$Day <- as.POSIXct(Data$Date, format="%m/%d/%Y %H:%M")

Data1<-Data[complete.cases(Data$Oxygen),]

#To see what cut does:
cut(Data1$Day, breaks="5 min")

#Applying cut:
Oxygenmeans <- aggregate(Data1["Oxygen"], 
                   list(fiveMin=cut(Data1$Day, "5 mins")),
                   mean)

Argonmeans <- aggregate(Data1["Argon"], 
                         list(fiveMin=cut(Data1$Day, "5 mins")),
                         mean)


Methanemeans <- aggregate(Data1["Methane"],
                          list(fiveMin=cut(Data1$Day, "5 mins")),
                          mean)


NitrousOxidemeans <- aggregate(Data1["Nitrous.Oxide"],
                          list(fiveMin=cut(Data1$Day, "5 mins")),
                          mean)


NitricOxidemeans <- aggregate(Data1["Nitric.oxide"],
                               list(fiveMin=cut(Data1$Day, "5 mins")),
                               mean)


TempMIMSmeans <- aggregate(Data1["Temperature.Corrected"], 
                        list(fiveMin=cut(Data1$Day, "5 mins")),
                        mean)


Watermeans <- aggregate(Data1["Water"],
                        list(fiveMin=cut(Data1$Day, "5 mins")),
                        mean)

#These are both in ppm

#Import temp and salnity (note: have to have minute of first time match the minute of the MIMs file so that 5 minute match up can work)
#HAVE TO PUT SCCOOS DATE INTO PST

SCCOOS<-read.csv("Temp_Salinity_Sept.csv",header=T,na.strings="NA")

UTCtime<-as.POSIXct(SCCOOS$timeUTC, tz="UTC")
SCCOOS$timePST<-format(UTCtime, tz="America/Los_Angeles", usetz=TRUE)

SCCOOS$timePST <- as.POSIXct(SCCOOS$timePST, format="%Y-%m-%d %H:%M")

SCCOOS2 <- data.frame("timePST"=c(SCCOOS$timePST),"pressure"=c(SCCOOS$pressure),"temperature"=c(SCCOOS$temperature),"chlorophyll"=c(SCCOOS$chlorophyll),"salinity"=c(SCCOOS$salinity))

library("zoo", lib.loc="~/R/win-library/3.0")
SCCOOS.zoo <- zoo(SCCOOS2[,-1],SCCOOS2[,1])

SCCOOS2 <- merge(SCCOOS.zoo, zoo(,seq(start(SCCOOS.zoo), end(SCCOOS.zoo), by="min")), all=TRUE)

SCCOOS2 <- as.data.frame(SCCOOS2)

SCCOOS2$pressure <- na.spline(SCCOOS2$pressure)
SCCOOS2$temperature <- na.spline(SCCOOS2$temperature)
SCCOOS2$chlorophyll <- na.spline(SCCOOS2$chlorophyll)
SCCOOS2$salinity <- na.spline(SCCOOS2$salinity)
SCCOOS2$timePST <- rownames(SCCOOS2)
SCCOOS2$timePST <- as.POSIXct(SCCOOS2$timePST, format="%Y-%m-%d %H:%M")

SCCOOS2 <- SCCOOS2[2:51441,1:5]

#Import wind

Wind <- read.csv("Wind_Sept.csv",header=T,na.strings="NA")

GMTtime <- as.POSIXct(Wind$TimeGMT, tz="GMT")
Wind$timePST<-format(GMTtime, tz="America/Los_Angeles", usetz=TRUE)

Wind$timePST <- as.POSIXct(Wind$timePST, format="%Y-%m-%d %H:%M")

Wind2 <- data.frame("TimePST"=c(Wind$timePST),"WSPD"=c(Wind$WSPD),"AIRTMP"=c(Wind$ATMP),"WTRTMP"=c(Wind$WTMP),"WDIR"=c(Wind$WDIR),"PRESS"=c(Wind$PRESS))

Wind.zoo <- zoo(Wind2[,-1],Wind2[,1])

Wind2 <- merge(Wind.zoo, zoo(,seq(start(Wind.zoo), end(Wind.zoo), by="min")), all=TRUE)

Wind2 <- as.data.frame(Wind2)

Wind2$AIRTMP <- na.spline(Wind2$AIRTMP)
Wind2$PRESS <- na.spline(Wind2$PRESS)
Wind2$WDIR <- na.spline(Wind2$WDIR)
Wind2$WSPD <- na.spline(Wind2$WSPD)
Wind2$WTRTMP <- na.spline(Wind2$WTRTMP)
Wind2$timePST <- rownames(Wind2)
Wind2$timePST <- as.POSIXct(Wind2$timePST, format="%Y-%m-%d %H:%M")

Wind2 <- Wind2[3:66013,1:6]

#Average of 5 mins

Tempmeans <- aggregate(SCCOOS2["temperature"], 
                         list(fiveMin=cut(SCCOOS2$timePST, "5 mins")),
                         mean)

Salinitymeans <- aggregate(SCCOOS2["salinity"], 
                        list(fiveMin=cut(SCCOOS2$timePST, "5 mins")),
                        mean)

Chlorophyllmeans <- aggregate(SCCOOS2["chlorophyll"],
                        list(fiveMin=cut(SCCOOS2$timePST, "5 mins")),
                        mean)

Windspeedmeans <- aggregate(Wind2["WSPD"],
                        list(fiveMin=cut(Wind2$timePST, "5 mins")),
                        mean)

AirPressmeans <- aggregate(Wind2["PRESS"],
                           list(fiveMin=cut(Wind2$timePST, "5 mins")),
                           mean)

AirTempmeans <- aggregate(Wind2["AIRTMP"],
                           list(fiveMin=cut(Wind2$timePST, "5 mins")),
                           mean)



#Merge SCCOOS with Data data.frame

Everything<-merge(Salinitymeans,Tempmeans,by="fiveMin",all=TRUE)
Everything2<-merge(Everything,Argonmeans,by="fiveMin",all=TRUE)
Everything3<-merge(Everything2,Oxygenmeans,by="fiveMin",all=TRUE)
Everything4<-merge(Everything3,TempMIMSmeans,by="fiveMin",all=TRUE)
Everything5<-merge(Everything4,Watermeans,by="fiveMin",all=TRUE)
Everything6<-merge(Everything5,Windspeedmeans,by="fiveMin",all=TRUE)
Everything7<-merge(Everything6,Chlorophyllmeans,by="fiveMin",all=TRUE)

Everything7 <- Everything7[368:13203,1:9]

Everything7$TS <- log((298.15-Everything7$Temperature.Corrected)/(273.15+Everything7$Temperature.Corrected))

AverageWater <- mean(na.omit(Everything7$Water))
Everything7$WaterCorrection <- AverageWater/Everything7$Water

Everything7$ArgonCorr <- Everything7$Argon/Everything7$WaterCorrection

Everything7$OxygenCorr <- Everything7$Oxygen/Everything7$WaterCorrection

ls(Everything7)

#Calculate Argon and Oxygen at Saturation (These saturations come from Hamme and Emerson 2004 and Garcia and Gordon 1992):

ArSat <- 2.71828^(2.79150 + 3.17609*(Everything7$TS) + 4.13116*(Everything7$TS^2) + 4.90379*(Everything7$TS^3) + Everything7$salinity*(-0.00696233 + -0.00766670*(Everything7$TS) - 0.0116888*(Everything7$TS^2)))

O2Sat <- 2.71828^(5.80871 + 3.20291*(Everything7$TS) + 4.17887*(Everything7$TS^2) + 5.10006*(Everything7$TS^2) + 5.10006*(Everything7$TS^3) -0.0986643*(Everything7$TS^4) + 3.80369*(Everything7$TS^5) + Everything7$salinity*(-0.00701577 - 0.00770028*(Everything7$TS) - 0.0113864*(Everything7$TS^2) - 0.00951519*(Everything7$TS^3)) - 0.000000275915*(Everything7$salinity^2))
#Units for ArSat and O2Sat are in umol/kg


#Converting measured oxygen and argon into umol/kg (based on calibrations (100% and 0%) using oxygen and argon saturated water)

Everything7$OxygenConc <- 0.0288*Everything7$OxygenCorr - 371.24

Everything7$ArgonConc <- 0.027*Everything7$ArgonCorr - 16.507



#Convert Wind speed (m/s) to 10m above sea surface using Donelan 1990

ActualHeightToSurface=8.2 #This will change as I made it up--may want to use actual tidal height from some database

U10<-Everything7$WSPD*(1+(((1.3*(10^-3))^0.5)/0.4)*log(10/(ActualHeightToSurface)))



#Have to calculate gas transfer velocity (aka gas transfer coefficient or piston velocity) for CO2 (k660)--There are many different equations for this but used McGillis et al. (2001) as it covered a wide range of windspeeds and does not take k to zero at low wind speeds (See Wanninkhof et al. 2009)
#K units are cm/hr and U10 units are in m/s

K660<-3.3+0.026*((U10)^3)

#Calculate Schmidt number (the kiematic viscosity of water divided by the diffusion coefficient of the gas--a dimensionless number) for O2 in seawater (Wanninkhof 1992)
#T=Temp in degress Celsius

ScO2<-1953.4-128*Everything7$temperature+3.9918*(Everything7$temperature^2)-0.050091*(Everything7$temperature^3)

#Convert K660 to KO2 using ScO2 and 660 (the Schmitt number for CO2 in seawter)

FUNK660<-function(U10){if(U10<1.9999999999){2/3}else{1/2}}

number<-sapply(U10, FUNK660)

KO2<-((ScO2/660)^(-1*number))*K660 #Units are cm/hr so will have to change to m/5min probably

KO2mper5min<-KO2/(12*100)


#Calcuate Biological Oxygen Saturation

DeltaO2Ar <- ((Everything7$OxygenConc/Everything7$ArgonConc)/(O2Sat/ArSat)-1)

#NCP

NCP5min <- KO2mper5min*O2Sat*DeltaO2Ar

NCP5min <- NCP5min[2141:7652] #This reflects time when the MIMS was working well

NCP5min.splined <- na.spline(NCP5min)

#Summing NCP for 24 hours (from midnight to midnight)
cutpoints<-seq(1,length(NCP5min.splined),by=288)

categories<-findInterval(1:length(NCP5min),cutpoints)

NCP24<-tapply(NCP5min.splined,categories,sum)



#Making some files

fivemin_means_and_NCP <- cbind(Everything7[2141:7652,],NCP5min)

write.csv(fivemin_means_and_NCP, "fivemin_means_and_NCP.csv")
