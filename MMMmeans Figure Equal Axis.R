library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

work<-"E:/WETFEED Analysis"# work
raw<-"E:/WETFEED Analysis/Model Outputs/raw"
hist<-"E:/WETFEED Analysis/Model Outputs/hist"
lins<-"E:/WETFEED Analysis/Model Outputs/lins"
deets<-"E:/WETFEED Analysis/Model Outputs/deets"
anly<-"E:/WETFEED Analysis/Model Outputs/Analysis"
clean<-"E:/WETFEED Analysis/Environmental data/Cleaned"

setwd(anly)
df<-read.csv("NaddedtoLPR.csv")
df$Date<-as.Date(df$Date, "%Y-%m-%d")
df$MoYr<-as.factor(floor_date(df$Date, "month"))
# setwd(work)
# tp<-read.csv("LDM Raw.csv")
# tp$Date<-as.Date(tp$Date, format = "%m/%d/%Y")
# tp<-summaryBy(Timepoint ~ Date + rDate, FUN = mean, na.rm = T, tp)
# names(tp)[3]<-"Timepoint"; tp$Timepoint<-as.factor(tp$Timepoint)
# tp$rDate<-as.Date(tp$rDate, format = "%m/%d/%Y")

# setwd(anly)
# df<-merge(df, tp, by = "Date")
# df$rDate<-as.Date(df$rDate, format = "%m/%d/%Y")


df$Site<-substr(df$UserIDs_in, 1,2); df$Site<-as.factor(df$Site)
df$Plot<-substr(df$UserIDs_in, 3,3); df$Plot<-as.factor(df$Plot)
df$Species<-substr(df$UserIDs_in,4,4); df$Species<-as.factor(df$Species)
df$Treatment<-substr(df$UserIDs_in,5,5);df$Treatment<-as.factor(df$Treatment)
# df$MoYr<-as.Date(df$Date, format = "%m-%Y")

summary(df)





#########################################################
#########################################################

work<-"E:/WETFEED Analysis"# work
raw<-"E:/WETFEED Analysis/Model Outputs/raw"
hist<-"E:/WETFEED Analysis/Model Outputs/hist"
lins<-"E:/WETFEED Analysis/Model Outputs/lins"
deets<-"E:/WETFEED Analysis/Model Outputs/deets"
anly<-"E:/WETFEED Analysis/Model Outputs/Analysis"
clean<-"E:/WETFEED Analysis/Environmental data/Cleaned"


setwd(anly)
source<-read.csv("NaddedtoLPR.csv")

setwd(clean)

env<-read.csv("Temperature_cleaned.csv")
env$Date<-as.Date(env$Date, "%m/%d/%Y")
env$sDate<-as.character(env$Date)
env<-subset(env, AirTemp_C > 0)
source$Date<-as.Date(source$Date, "%Y-%m-%d")
source$MoYr<-as.factor(floor_date(df$Date, "month"))

source$Narea<-source$Narea*1000

new_env<-subset(env, Code == "")
for (reps in unique(source$sDate)){
  dat<-subset(env, env$sDate == reps)
  mnd<-min(dat$Date, na.rm = T)
  
  dat1<-subset(env, env$Date == mnd-1)
  dat2<-subset(env, env$Date == mnd-2)
  dat3<-subset(env, env$Date == mnd-3)
  dat4<-subset(env, env$Date == mnd-4)
  dat5<-subset(env, env$Date == mnd-5)
  dat6<-subset(env, env$Date == mnd-6)
  dat7<-subset(env, env$Date == mnd-7)
  dat8<-subset(env, env$Date == mnd-8)
  dat9<-subset(env, env$Date == mnd-9)
  dat10<-subset(env, env$Date == mnd-10)
  dat11<-subset(env, env$Date == mnd-11)
  dat12<-subset(env, env$Date == mnd-12)
  dat13<-subset(env, env$Date == mnd-13)
  dat14<-subset(env, env$Date == mnd-14)
  dat15<-subset(env, env$Date == mnd-15)
  dat16<-subset(env, env$Date == mnd-16)
  dat17<-subset(env, env$Date == mnd-17)
  dat18<-subset(env, env$Date == mnd-18)
  dat19<-subset(env, env$Date == mnd-19)
  dat20<-subset(env, env$Date == mnd-20)
  dat21<-subset(env, env$Date == mnd-21)
  
  
  
  newdat<-rbind(dat1,dat2,dat3,dat4,dat5,dat6,dat7)
  newdat$Date<-max(newdat$Date, na.rm = T)+1
  
  ##,dat15,dat16,dat17,dat18,dat19,dat20,dat21,dat22
  
  new_env<-rbind(newdat, new_env)
  rm(dat1,dat2,dat3,dat4,dat5,dat6,dat7)
}
rm(newdat, env)


new_env$Site<-substr(new_env$Code, 1, 2)
new_env$Plot<-substr(new_env$Code, 3, 3)
new_env$Spp<-substr(new_env$Code, 4, 4)
new_env$Treatment<-substr(new_env$Code, 5, 5)
new_env$Site<-as.factor(new_env$Site)
new_env$Plot<-as.factor(new_env$Plot)
new_env$Spp<-as.factor(new_env$Spp)
new_env$Treatment<-as.factor(new_env$Treatment)

source$Site<-substr(source$UserIDs_in, 1, 2)
source$Plot<-substr(source$UserIDs_in, 3, 3)
source$Spp<-substr(source$UserIDs_in, 4, 4)
source$Treatment<-substr(source$UserIDs_in, 5, 5)
source$site<-as.factor(source$Site)
source$Treatment<-as.factor(source$Treatment)
source$Spp<-as.factor(source$Spp)
source$Plot<-as.factor(source$Treatment)

env<-summaryBy(AirTemp_C + SoilTemp1_C + SoilTemp2_C + RH_perc + DewPt_C
               ~ Site + Treatment + Date + MoYr, FUN = mean, na.rm = T, new_env)

rm(new_env)


#########################################################

setwd(anly)

df<-merge(source, env, by = c("Date", "Site", "Treatment"), all = T); rm(env)

#########################################################
#########################################################


dat<-subset(df, R_calc == "R.area")
dat<-summaryBy(rdref + AirTemp_C.mean + Narea + Nmass..g.N.kg.1. ~ Treatment + Spp + Date + MoYr + Site, FUN = c(mean, std.error), na.rm = T, dat)


source$MoYr<-floor_date(source$Date, "month") # Makes all sampling dates 1st of the month
# Otherwise they'll be offset and wonky when plotted
source$Site<-substr(source$UserIDs_in, 1, 2); source$Site<-as.factor(source$Site)
source$Plot<-substr(source$UserIDs_in, 3, 3); source$Plot<-as.factor(source$Plot)
source$Species<-substr(source$UserIDs_in, 4, 4); source$Species<-as.factor(source$Species)
source$Treatment<-substr(source$UserIDs_in, 5, 5); source$Treatment<-as.factor(source$Treatment)


sdf<-summaryBy(rdref + q10 + LMA_g.m2 + Narea + Nmass..g.N.kg.1.~ MyTemp + MoYr + R_calc * Species * Treatment * Site, FUN = c(mean,std.error), na.rm = T, source)



tiff(file = "Meansfigure equal axis.tiff", height = 12, width = 6, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(6,2), omi = c(1.5, 0.5, 0.3, 0.1), mar = c(0.6,1.5,1,0.25))

###############################

############################### 
# 
plotCI(sdf$MoYr, sdf$rdref.mean, sdf$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.6,2.2))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(0.6,2.2,.2),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates

# mtext(side = 2, expression(italic(R)[area]*25^degree*""^C~(mu*mol~m^-2~s^-1)), cex = 1, padj = -1.8, outer= F)
mtext(side = 3, expression(italic(Avicennia~germinans)), cex = 1, padj = -0.3, outer = F)
legend("top", c("North Site", "South Site"), col=c("Black", "black"), pch=1, lty = 1:2, horiz = F, bty='n', cex = 1)
legend("topleft", c("(a)"), bty = "n")
legend("topright", c("Ambient", "Warmed"), col=c("blue", "firebrick"), pch=1, lty = 1, horiz = F, bty='n', cex= 1)

mtext(side = 2, expression(italic(R)[area]*''^25~(mu*mol~m^-2~s^-1)), cex = 0.8, padj = -2, outer= F)

###############################
plotCI(sdf$MoYr, sdf$rdref.mean, sdf$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(0.6,2.2))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(0.6,2.2))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(0.6,2.2,.2),las = 2, cex.axis = 1.1, labels = F)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates

mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = -0.5, outer = F)
legend("topleft", c("(b)"), bty = "n")


############################### Line 1 left "R.area means x time"

###############################
############################### Line 1 Right "R.area site 2 raw"
# 
plotCI(sdf$MoYr, sdf$rdref.mean, sdf$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(2,16))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(2,16,2),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates

# legend("bottom", c("Warmed", "Control"), col=c("firebrick", "blue"), pch=1, lty = 1, horiz = F, bty='n')
# legend("top", c("North Site", "South Site"), col=c("Black", "black"), pch=1, lty = 1:2, horiz = F, bty='n')
mtext(side = 2, expression(italic(R)[mass]*''^25~(n*mol~g^-1~s^-1)), cex = 0.8, padj = -1.8, outer= F)
# mtext(side = 2, expression(italic(R)[mass]*25^degree*""^C~(n*mol~g^-1~s^-1)), cex = 1, padj = -1.8, outer= F)
legend("topleft", c("(c)"), bty = "n")

###############################
plotCI(sdf$MoYr, sdf$rdref.mean, sdf$rdref.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(2,16))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$rdref.mean, dum$rdref.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(2,16))
points(dum$rdref.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(2,16,2),las = 2, cex.axis = 1.1, labels = F)
axis.Date(1, sdf$MoYr, at = seq(min(sdf$MoYr), max(sdf$MoYr), "month"), las=2, labels = F) # Special axis command for dates

# legend("bottom", c("Warmed", "Control"), col=c("firebrick", "blue"), pch=1, lty = 1, horiz = F, bty='n')
# legend("top", c("North Site", "South Site"), col=c("Black", "black"), pch=1, lty = 1:2, horiz = F, bty='n')


legend("topleft", c("(d)"), bty = "n")
############################### Line 2 left "R.area means x time"

###############################
############################### Line 2 Right "R.area site 2 raw"
# 
plotCI(sdf$MoYr, sdf$q10.mean, sdf$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.4,2.6))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")



axis(2, at = seq(1.4,2.6,.2),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates
mtext(side = 2, expression(italic(Q)[10^~~25]), cex = 1.2, padj = -1.8, outer= F)
# mtext(side = 2, expression(Q[10]), cex = 1.5, padj = -2.0, outer= F)
legend("topleft", c("(e)"), bty = "n")

###############################
plotCI(sdf$MoYr, sdf$q10.mean, sdf$q10.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.4,2.6))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$q10.mean, dum$q10.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.4,2.6))
points(dum$q10.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")


axis(2, at = seq(1.4,2.6,.2),las = 2, cex.axis = 1.1, labels = F)
axis.Date(1, sdf$MoYr, at = seq(min(sdf$MoYr), max(sdf$MoYr), "month"),las=2, labels = F) # Special axis command for dates


legend("topleft", c("(f)"), bty = "n")
############################### Line 3 left "R.area means x time"

###############################
############################### Line 3 Right "R.area site 2 raw"
#
plotCI(sdf$MoYr, sdf$LMA_g.m2.mean, sdf$LMA_g.m2.std.error*0, sfrac = 0,
       yaxt="n", xaxt="n", xlab="",ylab="",pch=NA, ylim = c(70,320))


par(new = T) # Adds plot on top of the template

dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")



axis(2, at = seq(70,320,50),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, cex = 1.2, labels = F) # Special axis command for dates

# mtext(side = 2, expression(LMA~(g~m^-2)), cex = 1, padj = -2.00, outer= F)
# mtext(side = 1, expression(bold(Time~(2019-2020))), cex =0, padj=2.5, outer =F)
mtext(side = 2, expression(LMA~(g~m^-2)), cex = 1, padj = -1.8, outer= F)
legend("topleft", c("(g)"), bty = "n")

###################################
plotCI(sdf$MoYr, sdf$LMA_g.m2.mean, sdf$LMA_g.m2.std.error*0, sfrac = 0,
       yaxt="n",xaxt="n",xlab="",ylab="",pch=NA, ylim = c(70,320))
# This makes a template

par(new=T) # Adds plot on top of the template

yl<-0; yh<-300

dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$LMA_g.m2.mean, dum$LMA_g.m2.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(70,320))
points(dum$LMA_g.m2.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")


axis(2, at = seq(70,320,50),las = 2, cex.axis = 1.1, labels = F)

axis.Date(1, sdf$MoYr, at = seq(min(sdf$MoYr), max(sdf$MoYr), "month"), las = 2, cex = 1.2, labels =F) # Special axis command for dates



mtext(side = 1, expression(Date~(Month/Year)), cex =1.2, padj=3.5, outer =T)
legend("topleft", c("(h)"), bty = "n")

################################################################


################################################################ 
plotCI(sdf$MoYr, sdf$Narea.mean, sdf$Narea.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.5,5))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")
legend("topleft", c("(i)"), bty = "n")
axis(2, at = seq(1.5,5,0.5),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates
mtext(side = 2, expression(italic(N)[area]~(g~N~m^-2)), cex = 1, padj = -1.75, outer= F)
##########################################################################
plotCI(sdf$MoYr, sdf$Narea.mean, sdf$Narea.std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(1.5,5))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "red", lty = 1, type = "l")



par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$Narea.mean, dum$Narea.std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(1.5,5))
dum<-dum[complete.cases(dum$Narea.mean),]
points(dum$Narea.mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(1.5,5,0.5),las = 2, cex.axis = 1.1, labels = F)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), las=2, labels = F) # Special axis command for dates
legend("topleft", c("(j)"), bty = "n")


######################################


################################################################ 
plotCI(sdf$MoYr, sdf$Nmass..g.N.kg.1..mean, sdf$Nmass..g.N.kg.1..std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(12,22))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.area"  & Species == "G" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")
legend("topleft", c("(k)"), bty = "n")
axis(2, at = seq(12,22,2),las = 2, cex.axis = 1.1)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), format = "%b %Y", las=2) # Special axis command for dates
mtext(side = 2, expression(italic(N)[mass]~(g~N~kg^-1)), cex = 1, padj = -1.75, outer= F)


###############################
plotCI(sdf$MoYr, sdf$Nmass..g.N.kg.1..mean, sdf$Nmass..g.N.kg.1..std.error*0, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=NA, ylim = c(12,22))
# This makes a template

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "W" & Site == "BM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "red", lty = 1, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "C" & Site == "BM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "blue", lty = 1, type = "l")

par(new=T) # Adds plot on top of the template
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "W" & Site == "NM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "red", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "red", lty = 2, type = "l")

par(new=T)
dum<-subset(sdf, R_calc == "R.mass"  & Species == "M" & Treatment == "C" & Site == "NM")
plotCI(dum$MoYr, dum$Nmass..g.N.kg.1..mean, dum$Nmass..g.N.kg.1..std.error, sfrac = 0,
       xaxt="n",yaxt="n",xlab="",ylab="",pch=1, col = "blue", ylim = c(12,22))
dum<-dum[complete.cases(dum$Nmass..g.N.kg.1..mean),]
points(dum$Nmass..g.N.kg.1..mean ~ dum$MoYr, col = "blue", lty = 2, type = "l")

axis(2, at = seq(12,22,2),las = 2, cex.axis = 1.1, labels = F)
axis.Date(1, source$MoYr, at = seq(min(source$MoYr), max(source$MoYr), "month"), format = "%b %Y", las=2)
# axis.Date(1, source$Date, at = seq(min(NM$Date, na.rm = T), max(NM$Date, na.rm = T),"months"), las = 2, format = "%b %Y", cex.axis = 0.8, labels = T)# Special axis command for dates
legend("topleft", c("(l)"), bty = "n")


############################### Line 1 left "R.area means x time"

dev.off()

