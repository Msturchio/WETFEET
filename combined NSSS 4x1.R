

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("D:/WETFEED Analysis/Environmental data/Stuff done by hand")
setwd("/Volumes/The Hive/WETFEED Analysis/Environmental data/Stuff done by hand")
# env<-read.csv("Cleanup_temp.csv")
# 
# env$Date<-substr(env$DateTime, 1, 7)
# env$Time<-substr(env$DateTime, 9, 13)



env<-read.csv("new8.csv")
env$SVP_kPa<-(610.78*(2.71828^(env$Temperature/(env$Temperature+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH/100))
env$Date<-as.Date(env$Date, format = "%m/%d/%y")





dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)

dum<-read.csv("temps need reorder.csv")
dum$Date<-as.Date(dum$Date, format = "%m/%d/%y")

warmed<- subset(dum, Treatment == "Warmed")

control<-subset(dum, Treatment == "Control")


BM<-merge(warmed, control, by = "Date")

BM$Diff<-BM$Temperature.mean.x-BM$Temperature.mean.y

tiff(file = "Combined NSSS 4x1.tiff", height = 8, width = 6, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(4,1), omi = c(0.8, 0.6, 0.1, 0.1), mar = c(2,2.5,0.2,0.5))

plot(Diff ~ Date, BM, pch = NA, ylim = c(-1,2.5), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), as.Date("2020-07-30")))

rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


points(BM$Diff ~ BM$Date, type = "l", col = "black", lty = 1)

rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-07-14"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

abline(h = mean(BM$Diff, na.rm = T), lty = 2, col = "green")
abline(h = 0)
# legend("topright", c(paste(mean(BM$Diff, na.rm = T))))

legend("topleft", c("(a)"), bty = "n")
legend("top", c("North Site", "South Site"), lty=c(1,2), bty = "n", cex = 1.3)
mtext(side = 2, expression(Difference~"in"~mean~daily~italic(T)[air]~(degree*C)), cex = 0.6, padj = -4, outer= F)

axis(2, at = seq(-1,2.5,0.5), cex.axis = 1.2, las = 2)
axis.Date(1, BM$Date, at = seq(as.Date("2019-10-01", na.rm = T), as.Date("2020-08-01", na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
#######################################
#######################
######################


warmed<- subset(dum, Treatment == "Warmed")

control<-subset(dum, Treatment == "Control")


BM<-merge(warmed, control, by = "Date")

BM$Diff<-BM$Temperature.max.x-BM$Temperature.max.y

plot(Diff ~ Date, BM, pch = NA, ylim = c(-2,6), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), as.Date("2020-07-30")))

rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


points(BM$Diff ~ BM$Date, type = "l", col = "black", lty = 1)

rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-07-14"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

abline(h = mean(BM$Diff, na.rm = T), lty = 2, col = "green")
abline(h = 0)
# legend("topright", c(paste(mean(BM$Diff, na.rm = T))))

legend("topleft", c("(b)"), bty = "n")

mtext(side = 2, expression(Difference~"in"~max~daily~italic(T)[air]~(degree*C)), cex = 0.6, padj = -4, outer= F)
mtext(side = 1, expression("Date (Month/Year)"), cex = 1.8, padj = 2, outer = T)
axis(2, at = seq(-2,6,1), cex.axis = 1.2, las = 2)
axis.Date(1, BM$Date, at = seq(as.Date("2019-10-01", na.rm = T), as.Date("2020-08-01", na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)

#######################################




env<-read.csv("new8.csv")
env$SVP_kPa<-(610.78*(2.71828^(env$Temperature/(env$Temperature+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH/100))
env$Date<-as.Date(env$Date, format = "%m/%d/%y")





dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)



warmed<- subset(dum, Treatment == "Warmed")

control<-subset(dum, Treatment == "Control")


NM<-merge(warmed, control, by = "Date")

NM$Diff<-NM$Temperature.mean.x-NM$Temperature.mean.y


plot(Diff ~ Date, NM, pch = NA, ylim = c(-1,2.5), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), as.Date("2020-07-30")))

rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


points(NM$Diff ~ NM$Date, type = "l", col = "black", lty = 2)

rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

abline(h = mean(NM$Diff, na.rm = T), lty = 2, col = "green")
abline(h = 0)
# legend("topright", c(paste(mean(NM$Diff, na.rm = T))))

legend("topleft", c("(c)"), bty = "n")
mtext(side = 2, expression(Difference~"in"~mean~daily~italic(T)[air]~(degree*C)), cex = 0.6, padj = -4, outer= F)

axis(2, at = seq(-1,2.5,0.5), cex.axis = 1.2, las = 2)
axis.Date(1, NM$Date, at = seq(min(NM$Date, na.rm = T), max(NM$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
#######################################
#######################
######################


warmed<- subset(dum, Treatment == "Warmed")

control<-subset(dum, Treatment == "Control")


NM<-merge(warmed, control, by = "Date")

NM$Diff<-NM$Temperature.max.x-NM$Temperature.max.y

plot(Diff ~ Date, NM, pch = NA, ylim = c(-2,6), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-01"), as.Date("2020-07-30")))

rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


points(NM$Diff ~ NM$Date, type = "l", col = "black", lty = 2)

rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

abline(h = mean(NM$Diff, na.rm = T), lty = 2, col = "green")
abline(h = 0)
# legend("topright", c(paste(mean(NM$Diff, na.rm = T))))

legend("topleft", c("(d)"), bty = "n")
mtext(side = 2, expression(Difference~"in"~max~daily~italic(T)[air]~(degree*C)), cex = 0.6, padj = -4, outer= F)


axis(2, at = seq(-2,6,1), cex.axis = 1.2, las = 2)
axis.Date(1, NM$Date, at = seq(min(NM$Date, na.rm = T), max(NM$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = T)
#######################################
dev.off()
