############
###########

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix); library(MASS)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

work<-"/Volumes/The Hive/WETFEED Analysis"# work
raw<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/raw"
hist<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/hist"
lins<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/lins"
deets<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/deets"
anly<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/Analysis"
clean<-"/Volumes/The Hive/WETFEED Analysis/Environmental data/Cleaned"

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

work<-"/Volumes/The Hive/WETFEED Analysis"# work
raw<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/raw"
hist<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/hist"
lins<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/lins"
deets<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/deets"
anly<-"/Volumes/The Hive/WETFEED Analysis/Model Outputs/Analysis"
clean<-"/Volumes/The Hive/WETFEED Analysis/Environmental data/Cleaned"


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


df$Rinsitu<-exp(df$a + (df$b * df$AirTemp_C.mean) + (df$c * df$AirTemp_C.mean^2))
df$Qinsitu<-exp(10*(df$b+(2*df$c*df$AirTemp_C.mean)))

# dum<-subset(df, Spp == "M" & R_calc == "R.area" )
# m1<-lm(Qinsitu~AirTemp_C.mean*Treatment*Site, dum)
# anova(m1)
# summary(m1)
# plot(dum$Qinsitu~dum$AirTemp_C.mean)

# new<-summaryBy(AirTemp_C.mean~UserIDs_in, FUN = min, na.rm=T, df)
# newer<-merge(new, df, by = "UserIDs_in")

# nu<-summaryBy(Rinsitu+AirTemp_C.mean~R_calc+Treatment+Spp+site+MoYr, FUN=mean, na.rm=T, df)
#  write.csv(nu, "everything22.csv", row.names = F)
##################################
tiff(file = "Longterm Q10 Insitu.tiff", height = 6, width = 6, res = 1200, units = "in", compression = "zip+p")
par(mfrow = c(2,2), omi = c(0.5, 0.5, 0.25, 0.1), mar = c(1,2.5,1,0.5))



dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(0, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")

legend("top", c("Warmed", "Ambient"), col=c("lightpink1", "lightskyblue1"), pch=1, lty = 1, horiz = F, bty='n', cex = 1)
legend("bottomright", c("North Site", "South Site"), pch=c(2,6), lty =c (1,3), horiz = F, bty='n', cex= 1)



dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(Rinsitu~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(Rinsitu~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)


#South site
K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.1551 * (3.1212) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2, lty = 3, col = "black")

#North Site
K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.09196 * (2.0133) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2,  col = "black")


# m1<-lm(Rinsitu~AirTemp_C.mean+Treatment,dum)
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
# m1<-lm(Rinsitu~AirTemp_C.mean*Site,dum)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
# new<-as.data.frame(p1$data)
# dat<-subset(new, group == "NM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dur<-subset(dum, Site == "NM")
# ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
# dat<-subset(new, group == "BM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dum<-subset(dum, Site == "BM")
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

# text(23, 2, expression(italic(r)^2~'= 0.78'),cex=1)


# legend("left", c(expression(italic(T)[air]*'***',"Treatment*", italic(T)[air]*' x Site***' )), bty = "n", cex = 0.8)

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

mtext(side = 2, expression(italic("in situ")~~italic(R)[area]~(mu*mol~m^-2~s^-1)), cex = 0.8, padj = -2.5, outer= F)
mtext(side = 3, expression(italic(Avicennia~germinans)), cex = 1, padj = 0, outer = FALSE)

axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(0,3,.5), las = 2, cex.axis = 1)
legend("topleft", c("(a)"), bty = "n", cex =1)

# plot.new()
###############################
############################### Line 1 Middle "R.area site 1 raw"


dum<-subset(df,  Site == "BM" & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(0, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df,  Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df,  Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df,  Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")


mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)

dum<-subset(df,  Spp == "M" & R_calc == "R.area" )
m1<-lm(Rinsitu~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)

K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.37284 * (1.79215) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2, col = "black")


# text(23, 0.4, expression(italic(r)^2~'= 0.50'),cex=1)
# legend("bottomleft", c(expression(italic(T)[air]*'***')), bty = "n", cex = 0.8)

# m1<-lm(Rinsitu~AirTemp_C.mean,dum)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
# new<-as.data.frame(p1$data)
# m2<-lm(predicted~x, new)
# coef(lm(predicted~x, new))
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "grey69" )
###legend("topright", c("Air","Trt","A x T"), bty = "n")


# mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)

axis(1, at = seq(15,29,2), cex.axis = 1.2, labels = F)
axis(2, at = seq(0,3,0.5),las = 2, cex.axis = 1)
legend("topleft", c("(b)"), bty = "n", cex = 1)

############################################################

dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(Rinsitu~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(0, 16), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
#legend("topright", c("Air","Trt","A x T"), bty = "n")


dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(Rinsitu~AirTemp_C.mean*Treatment*Site, dum)
anova(m1)
summary(m1)
# text(23, 12, expression(italic(r)^2~'= 0.67'),cex=1)


#North Site
K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {4.5113 * (2.2519) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2, col = "black")


#South Site
K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {5.7164 * (5.1481) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2, lty = 3, col = "black")

# m1<-lm(Rinsitu~AirTemp_C.mean*Site,dum)
# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
# new<-as.data.frame(p1$data)
# dat<-subset(new, group == "BM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "blue" )
# dat<-subset(new, group == "W")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, col = "firebrick" )
# m1<-lm(Rinsitu~AirTemp_C.mean*Site,dum)

# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
# new<-as.data.frame(p1$data)
# dat<-subset(new, group == "NM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dur<-subset(dum, Site == "NM")
# ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
# dat<-subset(new, group == "BM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dum<-subset(dum, Site == "BM")
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

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




# legend("left", c(expression(italic(T)[air]*'***',"Site*", italic(T)[air]*' x Site***' )), bty = "n", cex = 0.8)

mtext(side = 2, expression(italic("in situ")~italic(R)[mass]~(n*mol~g^-1~s^-1)), cex = 0.8, padj = -2.3, outer= F) 
axis(1, at = seq(15,29,2), cex.axis = 1, labels = T)
axis(2, at = seq(0,16,4),las = 2, cex.axis = 1)

legend("topleft", c("(c)"), bty = "n", cex =1)
# plot.new()
###############################
############################### Line 2 Middle "R.area site 1 raw"


dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(Rinsitu~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(0, 16), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(Rinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")



dum<-subset(df, Spp == "M" & R_calc == "R.mass")
m1<-lm(Rinsitu~AirTemp_C.mean+Site, dum)
anova(m1)
summary(m1)

# legend("bottomleft", c(expression(italic(T)[air]*'***',"Site*")), bty = "n", cex = 0.8)
# 
# text(23, 3, expression(italic(r)^2~'= 0.40'),cex=1)

K<-273.15; df$alpha<-df$AirTemp_C.mean+K; df$beta<-K+25
q10 <- function(x) {9.40122 * (1.34446) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,28.5,1)) ~ seq(16,28.5,1), type = "l", lwd = 2, col = "black")

# p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean", "Site"))
# new<-as.data.frame(p1$data)
# dat<-subset(new, group == "NM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dur<-subset(dum, Site == "NM")
# ablineclip(m2,x1=min(dur$AirTemp_C.mean,na.rm = TRUE),x2=max(dur$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
# dat<-subset(new, group == "BM")
# m2<-lm(predicted~x, dat)
# coef(lm(predicted~x, dat))
# dum<-subset(dum, Site == "BM")
# ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

axis(1, at = seq(15,29,2), cex.axis = 1, labels =T)
axis(2, at = seq(0,16,4),las = 2, cex.axis = 1)

legend("topleft", c("(d)"), bty = "n", cex = 1)


#######################
#######################

# 
# dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
# plot(Qinsitu~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1",  xlim = c(15,29), ylim = c(1.2, 2.8), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
# dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
# dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
# 
# 
# dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
# m1<-lm(Qinsitu~AirTemp_C.mean*Treatment*Site, dum)
# anova(m1)
# summary(m1)
# # p1<-plot_model(m1, type= c("pred"), terms= c("AirTemp_C.mean"))
# # new<-as.data.frame(p1$data)
# # m2<-lm(predicted~x, new)
# # ablineclip(m2,x1=min(dum$AirTemp_C.mean,na.rm = TRUE),x2=max(dum$AirTemp_C.mean,na.rm = TRUE), lwd=2)
# # i.nought= 2.507551
# # I1=i.nought + 0
# # I2= I1 + 0.250620
# # B= -0.022527
# # B2= B + -0.018812
# # 
# # xx<-subset(dum, Site == "BM" )
# # ablineclip(I1, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=1, lwd=2.15, col = "black")
# # 
# # xx<-subset(dum, Site == "NM" )
# # ablineclip(I2, x1=min(xx$AirTemp_C.mean,na.rm = TRUE),x2=max(xx$AirTemp_C.mean,na.rm = TRUE), B,lty=3, lwd=2.15, col = "black")
# 
# 
# mtext(side = 2, expression(italic(Q)[10]), cex = 1.1, padj = -2, outer= F)
# axis(1, at = seq(15,29,2), cex.axis = 1.1, labels = T)
# axis(2, at = seq(1.2,2.8,.2), las = 2, cex.axis = 1)
# 
# legend("topleft", c("(e)"), bty = "n", cex =1)
# 
# ####################################################
# 
# 
# dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
# plot(Qinsitu~ AirTemp_C.mean, dum, pch = 2, col= "lightskyblue1", xlim = c(15,29), ylim = c(1.2,2.6), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
# dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 2, col= "lightpink1")
# dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightskyblue1")
# dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
# points(Qinsitu ~ AirTemp_C.mean, dum, pch = 6, col= "lightpink1")
# #legend("topright", c("Air","Trt","A x T"), bty = "n")
# 
# # legend("bottom", c("p=0.06"), bty='n', cex = 1.5)
# 
# dum<-subset(df, Spp == "M" & R_calc == "R.mass" )
# m1<-lm(Qinsitu~AirTemp_C.mean*Treatment*Site, dum)
# anova(m1)
# summary(m1)
# 
# m1<-lm(Qinsitu~AirTemp_C.mean*Treatment,dum)
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
# 


# axis(1, at = seq(15,29,2), cex.axis = 1.1, labels = T)
# axis(2, at = seq(1.2,2.6,.2), las = 2, cex.axis = 1)
# legend("topleft", c("(f)"), bty = "n", cex = 1)
mtext(side = 1, expression(Mean~Daily~italic(T)[air]~(degree*C)), cex = 0, padj = 1.5, outer=T)

dev.off()
