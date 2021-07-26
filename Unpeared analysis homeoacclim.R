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

df<-read.csv("everything.csv")

tiff(file = "UNpeared analysis homeoacclim.tiff", height = 6, width = 6, res = 1200, units = "in", compression = "zip+p")
par(mfrow = c(2,2), omi = c(0.5, 1, 0.25, 0.1), mar = c(1,1,1,0.5))



dum<-subset(df,  site == "BM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightskyblue1",  xlim = c(0,14), ylim = c(0,2), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "G" & Treatment == "C")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightskyblue1")

dum<-subset(df, site == "BM" & R_calc == "R.area"   & Spp == "G" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightpink1")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "G" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightpink1")

mtext(side = 2, expression(italic(Acclim)[homeo]), cex = 1.5, padj = -2.2, outer= T) 
mtext(side = 3, expression(italic(Avicennia~germinans)), cex = 1, padj = 0, outer = FALSE)
mtext(side = 1, expression(Seasonal~temperature~change~(degree*C)), cex = 1.2, padj = 1.35, outer= T) 
legend("bottomleft", c("North Site", "South Site"), pch=c(2,6), lty =c (1,3), horiz = F, bty='n', cex= 1)
legend("bottomright", c("Ambient", "Warmed"), col=c("lightskyblue1", "lightpink1"), pch=c(1,1), lty =c (1,1), cex =c (0.7,0.7), horiz = F, bty='n')

dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(homeo_aclim~temp_diff*site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("temp_diff", "site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, site == "NM")
ablineclip(m2,x1=min(dur$temp_diff,na.rm = TRUE),x2=max(dur$temp_diff,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, site == "BM")
ablineclip(m2,x1=min(dum$temp_diff,na.rm = TRUE),x2=max(dum$temp_diff,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

legend("topright", c(expression(Δ~italic(T)[air]~'***',"Site***", Δ~italic(T)[air]*' x S***' )), bty = "n", cex = 0.9)

text(6, 1.4, expression(italic(r)^2~'= 0.74'),cex=1)
mtext(side = 2, expression(Area~basis), cex = 0.8, padj = -5.5, outer= F) # legend("topright", c("Tdiff***","Site***","Td x S***"), bty = "n")

abline(h=1, lty=2, col = "grey69")
axis(1, at = seq(0,14,4), cex.axis = 1.2, labels = F)
axis(2, at = seq(0,2,.5), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("(a)"), bty = "n", cex =1)
#####################################

dum<-subset(df,  site == "BM" & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightskyblue1",  xlim = c(0,14), ylim = c(0,2), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "M" & Treatment == "C")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightskyblue1")

dum<-subset(df, site == "BM" & R_calc == "R.area"   & Spp == "M" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightpink1")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "M" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightpink1")


mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)

dum<-subset(df, Spp == "M" & R_calc == "R.area" )
m1<-lm(homeo_aclim~temp_diff+site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("temp_diff", "site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, site == "NM")
ablineclip(m2,x1=min(dur$temp_diff,na.rm = TRUE),x2=max(dur$temp_diff,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, site == "BM")
ablineclip(m2,x1=min(dum$temp_diff,na.rm = TRUE),x2=max(dum$temp_diff,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

text(7.5, 1.4, expression(italic(r)^2~'= 0.37'),cex=1)

legend("topright", c(expression(Δ~italic(T)[air]~'***',"Site*")), bty = "n", cex = 0.9)

abline(h=1, lty=2, col = "grey69")
axis(1, at = seq(0,14,4), cex.axis = 1.2, labels = F)
axis(2, at = seq(0,2,.5), las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(b)"), bty = "n", cex =1)
####################################

dum<-subset(df,  site == "BM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightskyblue1",  xlim = c(0,14), ylim = c(0,2), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "G" & Treatment == "C")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightskyblue1")

dum<-subset(df, site == "BM" & R_calc == "R.area"   & Spp == "G" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightpink1")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "G" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightpink1")


dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(homeo_aclim~temp_diff*site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("temp_diff", "site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, site == "NM")
ablineclip(m2,x1=min(dur$temp_diff,na.rm = TRUE),x2=max(dur$temp_diff,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, site == "BM")
ablineclip(m2,x1=min(dum$temp_diff,na.rm = TRUE),x2=max(dum$temp_diff,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )



text(6, 1.4, expression(italic(r)^2~'= 0.75'),cex=1)
mtext(side = 2, expression(Mass~basis), cex = 0.8, padj = -5.5, outer= F) 
legend("topright", c(expression(Δ~italic(T)[air]~'***',"Site***", Δ~italic(T)[air]*' x S***' )), bty = "n", cex = 0.9)

abline(h=1, lty=2, col = "grey69")
axis(1, at = seq(0,14,4), cex.axis = 1.2, labels = T)
axis(2, at = seq(0,2,.5), las = 2, cex.axis = 1, labels = T)
legend("topleft", c("(c)"), bty = "n", cex =1)
####################################

dum<-subset(df,  site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightskyblue1",  xlim = c(0,14), ylim = c(0,2), xaxt = "n", yaxt = "n", xlab = "", ylab = "")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "M" & Treatment == "C")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightskyblue1")

dum<-subset(df, site == "BM" & R_calc == "R.area"   & Spp == "M" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 2, col= "lightpink1")

dum<-subset(df, site == "NM" & R_calc == "R.area"   & Spp == "M" & Treatment == "W")
points(homeo_aclim ~ temp_diff, dum, pch = 6, col= "lightpink1")

dum<-subset(df, Spp == "M" & R_calc == "R.mass" )
m1<-lm(homeo_aclim~temp_diff+site, dum)
anova(m1)
summary(m1)

p1<-plot_model(m1, type= c("pred"), terms= c("temp_diff", "site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, site == "NM")
ablineclip(m2,x1=min(dur$temp_diff,na.rm = TRUE),x2=max(dur$temp_diff,na.rm = TRUE), lwd=1.75, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, site == "BM")
ablineclip(m2,x1=min(dum$temp_diff,na.rm = TRUE),x2=max(dum$temp_diff,na.rm = TRUE), lwd=1.75, lty = 1, col = "black" )

text(7.5, 1.4, expression(italic(r)^2~'= 0.25'),cex=1)

legend("topright", c(expression(Δ~italic(T)[air]~'***',"Site**")), bty = "n", cex = 0.9)

abline(h=1, lty=2, col = "grey69")
axis(1, at = seq(0,14,4), cex.axis = 1.2, labels = T)
axis(2, at = seq(0,2,.5), las = 2, cex.axis = 1, labels = F)
legend("topleft", c("(d)"), bty = "n", cex =1)

dev.off()
