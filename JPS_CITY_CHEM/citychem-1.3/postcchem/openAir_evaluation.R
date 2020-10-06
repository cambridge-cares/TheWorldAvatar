library(ncdf4)
library(openair)

## path of statmonihour to evaluate 
statmoni_dir <- "../OUTPUT/"

###Measurements###
obs<-read.csv(file = "./obsdat/HH_NO2_monit_2013.csv", header = F, sep = ";", na.strings = "-99")[,1:18]
colnames(obs) <- c("date", "13ST", "17SM", "20VE", "21BI", "24FL", "27TA", "51BF", "52NG", "54BL", "61WB", "64KS", "68HB", "70MB", "72FI", "73FW", "74BT", "80KT")
obs$date <- as.POSIXct(obs$date, format =  "%d.%m.%Y %H:%M", tz = "GMT")

###Model###
##NO2##
pollutant <- "NO2"
statmoni <- nc_open(paste(statmoni_dir,"statmonihour.nc", sep =""))
startdate <- as.POSIXct(statmoni$dim$time$vals[1], origin="1970-01-01 00:00:00", tz = "GMT")
enddate <- as.POSIXct(statmoni$dim$time$vals[statmoni$dim$time$len], origin="1970-01-01 00:00:00", tz = "GMT")
date <- seq.POSIXt(from = startdate+3600, to = enddate+3600, by = "hour")
mod <- data.frame(date)
for (i in 1:20)
{
  mod <- cbind(mod, ncvar_get(statmoni, pollutant)[i,i,])
  mod[,i+1] <- as.numeric(format(round((mod[,i+1]), 2), nsmall = 1))  
}
names(mod) <- c("date", "80KT", "21BI", "74BT", "54BL", "51BF", "73FW", "72FI", "24FL", "78GW", "68HB", "77HF", "64KS", "70MB", "52NG", "79SU", "13ST", "17SM", "27TA", "20VE", "61WB")

for (i in 2:length(names(obs)))
{
id <- grep(names(obs)[i], names(mod))

temp <- mod$date
temp <- as.data.frame(temp)
names(temp) <- "date"

temp <- merge(temp,obs[,c(1,i)])
temp[,3] <- mod[,id]
temp[,4] <- names(obs)[i]
temp[,5] <- pollutant
temp[,6] <- "base"

names(temp) <- c("date", "obs", "mod", "site", "pol", "scenario")

if (exists("myData")==F)
{
  myData <- temp
} 
else
{
  myData <- rbind(myData,temp)
}
}

###Plots
setwd("../GRAFICS/")
png(filename = "openAir_timeplot_hourly.png", width = 800, height = 600, units = "px", pointsize = 24, res = 100)
timePlot(myData, type = "site", pollutant = c("obs","mod"), layout = c(4,5),main="NO2 07/2013 HH",date.format = "%d",ylab="[ug*m-3]",xlab="hour")
invisible(dev.off())

png(filename = "openAir_timeplot_daily.png", width = 1480, height = 1000, units = "px", pointsize = 24, res = 150)
timePlot(myData, type = "site", pollutant = c("obs","mod"), layout = c(4,5), avg.time = "day",main="NO2 07/2013 HH",date.format = "%d",ylab="[ug*m-3]",xlab="day")
invisible(dev.off())

png(filename = "openAir_timeVariation_13ST.png", width = 1480, height = 1000, units = "px", pointsize = 24, res = 150)
timeVariation(selectByDate(subset(myData, site == "13ST"),month="jul"),pollutant = c("obs","mod"),ylab="[ug*m-3]",main="NO2 07/2013 HH",date.format = c("%I","%I","%b","%a"))
invisible(dev.off())

openAir_modStats <- modStats(myData, type = "site")
write.csv(openAir_modStats, "openAir_modStats.csv", row.names = F, quote = F)

png(filename = "openAir_TaylorDiagram.png", width = 1480, height = 1000, units = "px", pointsize = 24, res = 150)
TaylorDiagram(myData, group ="site",main="NO2 07/2013 HH")
invisible(dev.off())

png(filename = "openAir_ScatterPlot_monthly.png", width = 1480, height = 1000, units = "px", pointsize = 24, res = 150)
scatterPlot(selectByDate(myData,month="jul"), x = "obs", y = "mod", group = "site", avg.time = "month", mod.line = T,main="NO2 07/2013 HH")
invisible(dev.off())

png(filename = "openAir_ScatterPlot_daily.png", width = 1480, height = 1000, units = "px", pointsize = 24, res = 150)
scatterPlot(selectByDate(myData,month="jul"), x = "obs", y = "mod", type = "site", avg.time = "day", statistic = "mean", linear = T, mod.line = T, layout = c(5, 4),main="NO2 07/2013 HH")
invisible(dev.off())
