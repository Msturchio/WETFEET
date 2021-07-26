library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(stringr)
library(plantecophys)
rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

work<-"F:/WETFEED Analysis/Environmental data"# work
raw<-"F:/WETFEED Analysis/Environmental data/Model Outputs/raw"
hist<-"F:/WETFEED Analysis/Environmental data/Model Outputs/hist"
lins<-"F:/WETFEED Analysis/Environmental data/Model Outputs/lins"
deets<-"F:/WETFEED Analysis/Environmental data/Model Outputs/deets"
anly<-"F:/WETFEED Analysis/Environmental data/Model Outputs/Analysis"
clean<-"F:/WETFEED Analysis/Environmental Data/Cleaned"
EnvAnly<-"F:/WETFEED Analysis/Environmental Data/EnvAnly"
#############################################################################
#############################################################################

### DATA CLEANING SECTION

setwd(clean)

env<-read.csv("GTMNERR_Soil and Air TemperatureOK5.csv")
env$Date<-as.Date(env$Date, format = "%m/%d/%Y")
env$SVP_kPa<-(610.78*(2.71828^(env$AirTemp_C/(env$AirTemp_C+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH_perc/100))


test<-subset(env, Code == "BM2MW")

env$combo<-interaction(env$Date, env$Code)

air<-env[!is.na(env$AirTemp_C),]
soil1<-env[!is.na(env$SoilTemp1_C),]
soil2<-env[!is.na(env$SoilTemp2_C),]
rm(env)

airtemp<-data.frame(AirTemp_C=as.numeric(),
                    Date=as.numeric(),
                    Code=as.numeric())
x<-0.01
reps<-unique(air$combo)
for (reps in unique(air$combo)){
  dum<-subset(air, combo == reps)
  qq<-quantile(air$AirTemp_C, probs = seq(0,1,0.001), na.rm = T)
  qq<-data.frame(qq)
  qq$percent<-rownames(qq)
  qq$percent<-str_remove_all(qq$percent, "%")
  qq$percent<-as.numeric(qq$percent)/100
  qq<-subset(qq, percent < as.numeric(1-x))
  mx<-max(qq, na.rm = T)
  qq<-subset(qq, percent > as.numeric(x))
  mn<-min(qq, na.rm = T)
  dum<-subset(dum, dum$AirTemp_C < mx)
  dum<-subset(dum, dum$AirTemp_C > mn)
  airtemp<-rbind(dum, airtemp)
  rm(dum); rm(qq); rm(mx)
}
# airtemp<-summaryBy(AirTemp_C ~ Code + Date, FUN = c(min, mean, max), na.rm = T, airtemp)
airtemp$Sensor<-"Air"
# names(airtemp)[3]<-"Min"
# names(airtemp)[4]<-"Mean"
# names(airtemp)[5]<-"Max"


soiltemp1<-data.frame(SoilTemp1_C=as.numeric(),
                      Date=as.numeric(),
                      Code=as.numeric())
x<-0.01
reps<-unique(soil1$combo)
for (reps in unique(soil1$combo)){
  dum<-subset(soil1, combo == reps)
  qq<-quantile(dum$SoilTemp1_C, probs = seq(0,1,0.001), na.rm = T)
  qq<-data.frame(qq)
  qq$percent<-rownames(qq)
  qq$percent<-str_remove_all(qq$percent, "%")
  qq$percent<-as.numeric(qq$percent)/100
  qq<-subset(qq, percent < as.numeric(1-x))
  mx<-max(qq, na.rm = T)
  qq<-subset(qq, percent > as.numeric(x))
  mn<-min(qq, na.rm = T)
  # dum<-subset(dum, dum$SoilTemp1_C < mx)
  dum<-subset(dum, dum$SoilTemp1_C > mn)
  soiltemp1<-rbind(dum, soiltemp1)
  rm(dum); rm(qq); rm(mx)
}
# soiltemp1<-summaryBy(SoilTemp1_C ~ Code + Date, FUN = c(min, mean, max), na.rm = T, soiltemp1)
soiltemp1$Sensor<-"Soil1"
# names(soiltemp1)[3]<-"Min"
# names(soiltemp1)[4]<-"Mean"
# names(soiltemp1)[5]<-"Max"

soiltemp2<-data.frame(SoilTemp2_C=as.numeric(),
                      Date=as.numeric(),
                      Code=as.numeric())
x<-0.01
reps<-unique(soil2$combo)
for (reps in unique(soil2$combo)){
  dum<-subset(soil2, combo == reps)
  qq<-quantile(dum$SoilTemp2_C, probs = seq(0,1,0.001), na.rm = T)
  qq<-data.frame(qq)
  qq$percent<-rownames(qq)
  qq$percent<-str_remove_all(qq$percent, "%")
  qq$percent<-as.numeric(qq$percent)/100
  qq<-subset(qq, percent < as.numeric(1-x))
  mx<-max(qq, na.rm = T)
  qq<-subset(qq, percent > as.numeric(x))
  mn<-min(qq, na.rm = T)
  # dum<-subset(dum, dum$SoilTemp2_C < mx)
  dum<-subset(dum, dum$SoilTemp2_C > mn)
  soiltemp2<-rbind(dum, soiltemp2)
  rm(dum); rm(qq); rm(mx)
}
# soiltemp2<-summaryBy(SoilTemp2_C ~ Code + Date, FUN = c(min, mean, max), na.rm = T, soiltemp2)
soiltemp2$Sensor<-"Soil2"
# names(soiltemp2)[3]<-"Min"
# names(soiltemp2)[4]<-"Mean"
# names(soiltemp2)[5]<-"Max"


env<-rbind(airtemp, soiltemp1); env<-rbind(env, soiltemp2)
rm(air, soil1, soil2, airtemp, soiltemp1, soiltemp2)

env$Site<-substr(env$Code, 1, 2)
env$Plot<-substr(env$Code, 3, 3)
env$Species<-substr(env$Code, 4, 4)
env$Trt<-substr(env$Code, 5, 5)

test<-subset(env, FileName == "BM2MW (W) 2020-06-14 05_41_27 -0400.csv")


#############################################################################
#######################################
#############################################################################

### In here you should subset out the data that you know might be shit.

levels(env$Code)

sub<-subset(env, Code == "NM1GC" | Code == "NM1MC")
env<-subset(env, Code != "NM1GC" | Code != "NM1MC")
summaryBy(AirTemp_C ~ Date, FUN = mean, na.rm = T, sub)
sub<-subset(sub, Date > "2020-01-29")
summaryBy(AirTemp_C ~ Date, FUN = mean, na.rm = T, sub)

env<-rbind(env, sub)

sub<-subset(env, Code == "NMXXC")
env<-subset(env, Code != "NMXXC")
sub<-subset(sub, Date < "2020-01-30")

env<-rbind(env, sub)
rm(sub)

#############################################################################
#######################################
#############################################################################

env$Site<-as.factor(env$Site)
env$Plot<-as.factor(env$Plot)
env$Species<-as.factor(env$Species)
env$Trt<-as.factor(env$Trt)


setwd(EnvAnly)
df<-subset(env, Sensor == "Air")
df<-summaryBy(AirTemp_C~ Site + Trt + Date, FUN = c(min, mean, max), na.rm = T, df)

### Airtemp by Site (panels) and treatment/species

tiff(file = "South Site (min,mean,max)_cleaned10.tiff", height = 6, width = 6, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,1), mar = c(1,1,1,1), omi = c(1,0.5,0.5,0.1))

plot(AirTemp_C ~ Date, env, pch = NA, ylim = c(0,40), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), max(df$Date, na.rm = T)))

rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()

dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$AirTemp_C.min ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$AirTemp_C.mean ~ dat$Date, type = "l", col = "black", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$AirTemp_C.max ~ dat$Date, type = "l", col = "firebrick", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$AirTemp_C.min ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$AirTemp_C.mean ~ dat$Date, type = "l", col = "black", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$AirTemp_C.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.0, outer= F)
legend("bottomleft", c("(a)"), bty = "n")


axis(2, at = seq(0,45,5), cex.axis = 1.2, las = 2)

#######################################


#######################################



######################################

##########################################

# mtext(side = 3, expression("North Site"), cex = 2, padj = -0.25, outer = T)

df<-subset(env, Sensor == "Air")
df<-summaryBy(RH_perc ~ Site + Trt + Date, FUN = c(min, mean, max), na.rm = T, df)



plot(RH_perc ~ Date, env, pch = NA, ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), max(df$Date, na.rm = T)))


rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()

dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$RH_perc.min ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$RH_perc.mean ~ dat$Date, type = "l", col = "black", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$RH_perc.max ~ dat$Date, type = "l", col = "firebrick", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$RH_perc.min ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$RH_perc.mean ~ dat$Date, type = "l", col = "black", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$RH_perc.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

# axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1)

mtext(side = 2, expression("Rh%"), cex = 1.5, padj = -2.0, outer= F)
legend("bottomleft", c("(b)"), bty = "n")

# mtext(side = 1, expression("Time"), cex = 2, padj = 2.5, outer = T)

axis(2, at = seq(0,100,10), cex.axis = 1.2, las = 2)

###########################################################


axis(2, at = seq(0,100,10), cex.axis = 1.2, las = 2)

# par(fig = c(0, 1, 0, 1), oma = c(28, 0, 0, 0), mar = c(0, 0, 0, 0), new = T)

mtext(side = 3, expression("South Site"), cex = 2, padj = -0.25, outer = T)

df<-subset(env, Sensor == "Air")
df<-summaryBy(VPD_kPa ~ Site + Trt + Date, FUN = c(min, mean, max), na.rm = T, df)



plot(VPD_kPa ~ Date, env, pch = NA, ylim = c(0,3), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), max(df$Date, na.rm = T)))


rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()

dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "C")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 2)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 1)
dat<-subset(df, Site == "NM" & Trt == "W")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1)

mtext(side = 2, expression("VPD_kPa"), cex = 1.25, padj = -1.65, outer= F)
legend("topleft", c("(c)"), bty = "n")

mtext(side = 1, expression("Time"), cex = 2, padj = 2.75, outer = T)

axis(2, at = seq(0,3,.2), cex.axis = 1.2, las = 2)


# legend("top", lty = c(2,1,NA,NA,NA), pch = c(NA,NA,15,15,15), bty = "n", xpd = T, inset = c(0,0), 
#        c("Ambient Treatment","Warmed Treatment","Min","Mean","Max"), lwd = 2, cex = 1,
#        col = c("grey60","grey60","blue","black","firebrick"), horiz = T)

dev.off()
