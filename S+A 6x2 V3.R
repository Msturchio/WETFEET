############
###########

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix); library(MASS)

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

source$sDate<-as.character(source$Date)
reps<-unique(source$sDate)

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

#############
#############
############

tiff(file = "S+A 6x2 V3.tiff", height = 12, width = 5, res = 1200, units = "in", compression = "zip+p")
par(mfrow = c(6,2), omi = c(0.5, 0.5, 0.25, 0.1), mar = c(0.5,1,1,0.5))



dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(0.5, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")

legend("topright", c("Ambient", "Warmed"), col=c("lightskyblue1", "lightpink1"), pch=1, lty = 1, horiz = F, bty='n', cex = 0.85)
legend("bottomright", c("North Site", "South Site"), pch=c(2,6), lty =c (1,3), horiz = F, bty='n', cex= 0.85)



dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(rdref~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)


m1<-lm(rdref~AirTemp_C.mean+Treatment,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Treatment"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "C")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "blue" )
dat<-subset(new, group == "W")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "firebrick" )
m1<-lm(rdref~AirTemp_C.mean*Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )


# i.nought=1.0969799
# I1=i.nought + 0
# I1sub = I1 + + 0.0390441
# I2=i.nought + -0.9339020
# I2sub= I2 + + 0.0390441
# B=-0.0007277
# B2= B+ 0.0390441
# 
# xx<-subset(dum, Site == "BM" & Treatment == "C" )
# ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "blue")
# xx<-subset(dum, Site == "BM" & Treatment == "W" )
# ablineclip(I1sub, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "firebrick3")
# 
# xx<-subset(dum, Site == "NM" & Treatment == "C" )
# ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B2,lty=3, lwd=2.15, col = "blue")
# xx<-subset(dum, Site == "NM" & Treatment == "W" )
# ablineclip(I2sub, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B2,lty=3, lwd=2.15, col = "firebrick3")


###legend("topright", c("Air","Trt","A x T"), bty = "n")

mtext(side = 2, expression(italic(R)[area^25]*""~(mu*mol~m^-2~s^-1)), cex = 0.75, padj = -2.0, outer= F)
mtext(side = 3, expression(italic(Avicennia~germinans)), cex = 0.8, padj = 0, outer = FALSE)

axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(0.5,3,.5), las = 2, cex.axis = 1)
legend("topleft", c("(a)"), bty = "n", cex =1)

# plot.new()
###############################
############################### Line 1 Middle "R.area site 1 raw"


dum<-subset(df,  Site == "BM" & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(0.5, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df,  Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df,  Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df,  Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")


mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 0.8, padj = 0, outer = FALSE)

dum<-subset(df,  Spp == "M" & R_calc == "R.area" )
m1<-lm(rdref~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

###legend("topright", c("Air","Trt","A x T"), bty = "n")


# mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)

axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(0.5,3,.5), las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(b)"), bty = "n", cex = 1)


##################
################

dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(rdref~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(0, 20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
#legend("topright", c("Air","Trt","A x T"), bty = "n")


dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(rdref~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

m1<-lm(rdref~AirTemp_C.mean+Treatment,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Treatment"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "C")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "blue" )
dat<-subset(new, group == "W")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "firebrick" )
m1<-lm(rdref~AirTemp_C.mean*Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

# i.nought=3.93092
# I1=i.nought + 0
# I1sub= I1 + 0.35298
# I2=i.nought + -7.80571
# I2sub=I2 + 0.35298
# B= 0.02087
# B2= B + 0.35298
# 
# xx<-subset(dum, Site == "BM" & Treatment == "C" )
# ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "blue")
# xx<-subset(dum, Site == "BM" & Treatment == "W" )
# ablineclip(I1sub, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "firebrick3")
# 
# xx<-subset(dum, Site == "NM" & Treatment == "C" )
# ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B2,lty=3, lwd=2.15, col = "blue")
# xx<-subset(dum, Site == "NM" & Treatment == "W" )
# ablineclip(I2sub, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B2,lty=3, lwd=2.15, col = "firebrick3")






mtext(side = 2, expression(italic(R)[mass^25]*""~(n*mol~g^-1~s^-1)), cex = 0.75, padj = -2.0, outer= F) 
axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(0,20,4),las = 2, cex.axis = 1)

legend("topleft", c("(c)"), bty = "n", cex =1)
# plot.new()
###############################
############################### Line 2 Middle "R.area site 1 raw"


dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(rdref~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(0, 20), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(rdref ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")



dum<-subset(df, Spp == "M" & R_calc == "R.mass")
m1<-lm(rdref~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

i.nought=21.34033
I1=i.nought + 0
I2=i.nought + -1.22228
B=-0.44474

xx<-subset(dum, Site == "BM")
ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2, col = "black")
xx<-subset(dum, Site == "NM")
ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=3, lwd=2, col = "black")

axis(1, at = seq(15,29,2), cex.axis = 1.2, labels =F)
axis(2, at = seq(0,20,4),las = 2, cex.axis = 1, labels = F)

legend("topleft", c("(d)"), bty = "n", cex = 1)


#######################
#######################


dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(q10~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(1, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(q10 ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
points(q10 ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(q10 ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")


dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(q10~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=2)
i.nought= 2.507551
I1=i.nought + 0
I2= I1 + 0.250620
B= -0.022527
B2= B + -0.018812

xx<-subset(dum, Site == "BM" )
ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "black")

xx<-subset(dum, Site == "NM" )
ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=3, lwd=2.15, col = "black")


mtext(side = 2, expression(italic(Q)[10^~~25]), cex = 1, padj = -2, outer= F)
axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(1,3,0.4), las = 2, cex.axis = 1)

legend("topleft", c("(e)"), bty = "n", cex =1)
# plot.new()
#######
############3
###################



dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(q10~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(1,3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(q10 ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
points(q10 ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(q10 ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
#legend("topright", c("Air","Trt","A x T"), bty = "n")

# legend("bottom", c("p=0.06"), bty='n', cex = 1.5)

dum<-subset(df, Spp == "M" & R_calc == "R.mass" )
m1<-lm(q10~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=2, col = "grey60")



axis(1, at = seq(15,29,2), cex.axis = 1, labels = F)
axis(2, at = seq(1,3,0.4), las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(f)"), bty = "n", cex = 1)

###############
#############


dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(LMA_g.m2~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(65, 315), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "G")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")


dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(LMA_g.m2~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

i.nought= 279.037
I1=i.nought + 0
I2= I1 + 124.197 - 40
B= -1.315
B2= B -1.617
B3 = -6.081

# dum<-subset(dum, Site == "BM" )
# ablineclip(I1, x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2, col = "black")

dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(LMA_g.m2~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

i.nought= 279.037
I1=i.nought + 0
I2= I1 + 124.197 - 40
B= -1.315
B2= B -1.617
B3 = -6.081

# dum<-subset(dum, Site == "NM" )
# ablineclip(I2, x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), B3,lty=3, lwd=2, col = "black")

# m1<-lm(LMA_g.m2~AirTemp_C.mean+Treatment,dum)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Treatment"))
# new<-as.data.frame(p1$data)
# dat<-subset(new, group == "C")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "blue" )
# dat<-subset(new, group == "W")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "firebrick" )
m1<-lm(LMA_g.m2~AirTemp_C.mean*Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )


mtext(side = 2, expression(LMA~(g~m^-2)), cex = 0.8, padj = -2, outer= F)
axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(65,315,50),las = 2, cex.axis = 1)

legend("topright", c("(g)"), bty = "n", cex =1)
# plot.new()
####################
####################



dum<-subset(df, Site == "BM"  & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(LMA_g.m2~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(65, 315), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM"  & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM"  & R_calc == "R.area"  & Treatment == "C" & Spp == "M")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM"  & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(LMA_g.m2 ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
#legend("topright", c("Air","Trt","A x T"), bty = "n")



dum<-subset(df,  Spp == "M" & R_calc == "R.area" )
m1<-lm(LMA_g.m2~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=2, col = "grey60")


axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(65,315,50),las = 2, cex.axis = 1, labels = F)
mtext(side = 1, expression(Mean~Daily~italic(T)[air]~(degree*C)), cex = 0, padj = 1.5, outer=T)
legend("topleft", c("(h)"), bty = "n", cex = 1)

###########
##########


#################
#################


dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(Narea ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(1,6), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(Narea ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "G")
points(Narea ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(Narea ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")


dum<-subset(df,  Spp == "G" & R_calc == "R.area" )
m1<-lm(Narea~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)


legend("topleft", c("(i)"), bty = "n", cex = 1)
mtext(side = 2, expression(italic(N)[area]~(g~N~m^-2)), cex = 0.7, padj = -2.1, outer= F)
axis(1, at = seq(15,29,2), cex.axis = 0.9, labels = F)
axis(2, at = seq(1,6,1),las = 2, cex.axis = 1, labels = T)


###########3
###########


dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(Narea ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(1,6), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(Narea ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "M")
points(Narea ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(Narea ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")

dum<-subset(df, Spp == "M" & R_calc == "R.area" )
m1<-lm(Narea~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

axis(1, at = seq(15,29,2), cex.axis = 0.9, labels = F)
axis(2, at = seq(1,6,1),las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(j)"), bty = "n", cex = 1)

#################################################
################################################




dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(10, 26), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")



dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(Nmass..g.N.kg.1.~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

i.nought= 9.49382
I1=i.nought + 0
I2= I1 + 124.197 - 40
B= 0.28811
B2= B -1.617
B3 = -6.081

dum<-subset(dum, Nmass..g.N.kg.1. != "NA")
ablineclip(I1, x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2, col = "grey60")


axis(1, at = seq(15,29,2), cex.axis = 0.9, labels = T)
axis(2, at = seq(10,26,4),las = 2, cex.axis = 1)
legend("topleft", c("(k)"), bty = "n", cex = 1)
mtext(side = 2, expression(italic(N)[mass]~(g~N~kg^-1)), cex = 0.7, padj = -2.1, outer= F)
# plot.new()
########################
########################


dum<-subset(df, Site == "BM"&  R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(10, 26), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM"&  R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM"&  R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM"&  R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(Nmass..g.N.kg.1. ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")

dum<-subset(df, Spp == "M" & R_calc == "R.mass" )
m1<-lm(Nmass..g.N.kg.1.~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=2)

i.nought=32.3628
I1=i.nought + 0
I2=i.nought + -13.0488
B1=-0.7500
B2= B1 + 0.5703
# i.nought=0.51
# I1=i.nought + 0
# I2=i.nought + (exp(coef(m1)["TreatmentW"])-1)
# B=(exp(coef(m1)["AirTemp_C.mean"])-1)

xx<-subset(dum, Treatment == "C" & Nmass..g.N.kg.1. != "NA")
ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B1,lty=1, lwd=2, col = "blue")
xx<-subset(dum, Treatment == "W" & Nmass..g.N.kg.1. != "NA")
ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B2,lty=1, lwd=2, col = "firebrick3")


axis(1, at = seq(15,29,2), cex.axis = 0.9, labels = T)
axis(2, at = seq(10,26,4),las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(l)"), bty = "n", cex = 1)
###################################################
################################################


dev.off()
