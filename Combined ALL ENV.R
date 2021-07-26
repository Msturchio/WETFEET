library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Desktop")
setwd("/Volumes/The Hive/WETFEED Analysis/Environmental data/Stuff done by hand")
# env<-read.csv("Cleanup_temp.csv")
# 
# env$Date<-substr(env$DateTime, 1, 7)
# env$Time<-substr(env$DateTime, 9, 13)



env<-read.csv("new8.csv")
env$SVP_kPa<-(610.78*(2.71828^(env$Temperature/(env$Temperature+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH/100))
env$Date<-as.Date(env$Date, format = "%m/%d/%y")


warmed<- subset(env, Treatment == "Warmed")

control<-subset(env, Treatment == "Control")


dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)


tiff(file = "Combined ENV 6x1.tiff", height = 12, width = 6, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(6,1), omi = c(1, 0.6, 0.1, 0.1), mar = c(1,2.5,0.2,0.5))
dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)

plot(Temperature.mean ~ Date, dum, pch = NA, ylim = c(0,40), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), as.Date("2020-08-05")))


# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.min ~ dat$Date, type = "l", col = "blue", lty = 1)

rect(xleft = as.Date("2020-08-01"), xright = as.Date("2020-08-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-20"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

mtext(side = 2, expression(italic(T)[air]~(degree*C)), cex = 1.7, padj = -1.1, outer= F)
legend("topleft", c("(a)"), bty = "n")
legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 1.2)
legend("bottomright", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 1.2)
# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)[]

axis.Date(1, env$Date, at = seq(min(control$Date, na.rm = T), max(control$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,40,5), cex.axis = 1.2, las = 2)

#############################################
#############################################
hot<-subset(dum, Treatment == "Warmed")
cold<-subset(dum, Treatment == "Control")

dum<-summaryBy(RH ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)


plot(RH.mean ~ Date, dum, pch = NA, ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), as.Date("2020-08-05")))


# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


dat<-subset(dum, Treatment == "Control")
points(dat$RH.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$RH.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$RH.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.min ~ dat$Date, type = "l", col = "blue", lty = 1)

rect(xleft = as.Date("2020-08-01"), xright = as.Date("2020-08-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-20"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

mtext(side = 2, expression("RH (%)"), cex = 1.5, padj = -1.6, outer= F)
# legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)
# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)

axis.Date(1, env$Date, at = seq(min(control$Date, na.rm = T), max(control$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,100,10), cex.axis = 1.2, las = 2)
legend("bottomleft", c("(b)"), bty = "n")



##########################################
########################################

hot<-subset(dum, Treatment == "Warmed")
cold<-subset(dum, Treatment == "Control")

dum<-summaryBy(VPD_kPa ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)


plot(VPD_kPa.mean ~ Date, dum, pch = NA, ylim = c(0,4.5), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), as.Date("2020-08-05")))


# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()

dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 1)

rect(xleft = as.Date("2020-08-01"), xright = as.Date("2020-08-30"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-20"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

# legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)

mtext(side = 1, expression("Date (Month/Year)"), cex = 1.5, padj = 3, outer = T)
legend("topleft", c("(c)"), bty = "n")
mtext(side = 2, expression("VPD (kPa)"), cex = 1.5, padj = -1.6, outer= F)
axis.Date(1, env$Date, at = seq(min(control$Date, na.rm = T), max(control$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1, labels = F)
axis(2, at = seq(0,4.5,0.5), cex.axis = 1.2, las = 2)



env<-read.csv("BM2MW_Fert New.csv")

env$Date<-substr(env$DateTime, 1, 7)
env$Time<-substr(env$DateTime, 9, 13)

env<-read.csv("Best.csv")
env$SVP_kPa<-(610.78*(2.71828^(env$Temperature/(env$Temperature+238.3)*17.2694)))/1000
env$VPD_kPa<-env$SVP_kPa * (1- (env$RH/100))
env$Date<-as.Date(env$Date, format = "%m/%d/%y")


# write.csv(dum, "temps need reorder.csv")

dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)



dum<-summaryBy(Temperature ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)

dum<-read.csv("temps need reorder.csv")
dum$Date<-as.Date(dum$Date, format = "%m/%d/%y")

plot(Temperature.mean ~ Date, dum, pch = NA, ylim = c(0,40), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), max(dum$Date, na.rm = T)))

# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$Temperature.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$Temperature.min ~ dat$Date, type = "l", col = "blue", lty = 1)


# rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-02"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-20"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# rect(xleft = as.Date("2020-04-09"), xright = as.Date("2020-06-07"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# box()
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-07-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()

legend("topleft", c("(d)"), bty = "n")
mtext(side = 2, expression(italic(T)[air]~(degree*C)), cex = 1.7, padj = -1.1, outer= F)

# legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 1.2)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 1.2)
# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)

axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,40,5), cex.axis = 1.2, las = 2)

#############################################
#############################################


dum<-summaryBy(RH ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)
# write.csv(dum, "RH need reorder.csv")

dum<-read.csv("RH need reorder.csv")
dum$Date<-as.Date(dum$Date, format = "%m/%d/%y")

plot(RH.mean ~ Date, dum, pch = NA, ylim = c(0,100), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), max(dum$Date, na.rm = T)))


# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


dat<-subset(dum, Treatment == "Control")
points(dat$RH.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$RH.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$RH.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$RH.min ~ dat$Date, type = "l", col = "blue", lty = 1)

# rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-02"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# rect(xleft = as.Date("2020-04-09"), xright = as.Date("2020-06-07"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-07-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()
legend("bottomleft", c("(e)"), bty = "n")
mtext(side = 2, expression("RH (%)"), cex = 1.5, padj = -1.6, outer= F)
# legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)
# mtext(side = 2, expression(degree*C), cex = 1.5, padj = -2.5, outer= F)
# mtext(side = 1, expression("Time"), cex = 1.5, padj = 1.5, outer = T)

axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = F)
axis(2, at = seq(0,100,10), cex.axis = 1.2, las = 2)




##########################################
########################################



dum<-summaryBy(VPD_kPa ~ Date + Treatment , FUN = c(min, mean, max), na.rm = T, env)
# write.csv(dum, "VPD need reorder.csv")
hot<-subset(dum, Treatment == "Warmed")
cold<-subset(dum, Treatment == "Control")

dum<-read.csv("VPD need reorder.csv")
dum$Date<-as.Date(dum$Date, format = "%m/%d/%y")


plot(VPD_kPa.mean ~ Date, dum, pch = NA, ylim = c(0,4.5), xaxt="n",yaxt="n",xlab="",ylab="", 
     xlim = c(as.Date("2019-10-10"), max(dum$Date, na.rm = T)))


# plot(Temperature.mean ~ Date, dum, pch = 1, col= "blue", ylim = c(0, 40), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
rect(xleft = as.Date("2019-10-19"), xright = as.Date("2019-10-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2019-12-13"), xright = as.Date("2019-12-19"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-02-06"), xright = as.Date("2020-02-12"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-04-02"), xright = as.Date("2020-04-08"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-07"), xright = as.Date("2020-06-13"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-07-23"), xright = as.Date("2020-07-29"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()


dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.mean ~ dat$Date, type = "l", col = "black", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.max ~ dat$Date, type = "l", col = "firebrick", lty = 1)

dat<-subset(dum, Treatment == "Control")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 3)
dat<-subset(dum, Treatment == "Warmed")
points(dat$VPD_kPa.min ~ dat$Date, type = "l", col = "blue", lty = 1)

# rect(xleft = as.Date("2020-07-29"), xright = as.Date("2020-08-02"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# rect(xleft = as.Date("2020-04-09"), xright = as.Date("2020-06-07"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-06-20"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
# box()
rect(xleft = as.Date("2020-06-13"), xright = as.Date("2020-07-06"), ybottom = -999, ytop = 999, col = "white", bty = "n", border = F)
box()
# legend("bottomleft", c("Max", "Mean", "Min"), col=c("firebrick", "Black", "Blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 0.8)
# legend("bottom", c("Warmed", "Ambient"), lty = 1:2, horiz = F, bty='n', cex = 0.8)


legend("topleft", c("(f)"), bty = "n")
mtext(side = 2, expression("VPD (kPa)"), cex = 1.5, padj = -1.6, outer= F)
axis.Date(1, env$Date, at = seq(min(env$Date, na.rm = T), max(env$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 1, labels = T)
axis(2, at = seq(0,4.5,0.5), cex.axis = 1.2, las = 2)



dev.off()


