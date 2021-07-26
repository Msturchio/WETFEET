
tiff(file = "2x2 Ncova figure.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(2,2), omi = c(0.5, 0.5, 0.5, 0.1), mar = c(5,2.5,0.5,0.5))


################################################################

dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Spp == "G" & Treatment == "C")
plot(rdref ~ Narea, dum, pch = 6, col= "lightskyblue1", xlim = c(2.5,6.5), ylim = c(0.6, 1.8), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(rdref ~ Narea, dum, pch = 6, col= "lightpink1")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "C" & Spp == "G")
points(rdref ~ Narea, dum, pch = 2, col= "lightskyblue1")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "G")
points(rdref ~ Narea, dum, pch = 2, col= "lightpink1")


dum<-subset(df, Spp == "G" & R_calc == "R.area" )
m1<-lm(rdref~Narea*Treatment*Site, dum)

anova(m1)
summary(m1)

# i.nought= 1.29218
# I1=i.nought + 0
# I2=i.nought + -1.05125
# I2sub= I2 + 0.2700 + 0.45174
# I2w=I2sub+ 0.109
# I1w=I1 +0.109
# B= -0.05625
# B2= B + 0.08167

m1<-lm(rdref~Narea+Treatment,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("Narea", "Treatment"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "C")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$Narea,na.rm = TRUE),x2=max(dum$Narea,na.rm = TRUE), lwd=2, col = "blue" )
dat<-subset(new, group == "W")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
ablineclip(m2,x1=min(dum$Narea,na.rm = TRUE),x2=max(dum$Narea,na.rm = TRUE), lwd=2, col = "firebrick" )
m1<-lm(rdref~Narea*Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("Narea", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$Narea,na.rm = TRUE),x2=max(dur$Narea,na.rm = TRUE), lwd=2, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$Narea,na.rm = TRUE),x2=max(dum$Narea,na.rm = TRUE), lwd=2, lty = 1, col = "black" )

# LOOKY HERE 

coef(lm(predicted~x, dat))
# 
# xx<-subset(dum, Site == "BM" & Narea != "NA")
# ablineclip(I1, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B,lty=1, lwd=2, col ="blue")
# xx<-subset(dum, Site == "NM" & Narea != "NA")
# ablineclip(I2sub, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B2,lty=3, lwd=2, col ="blue")
# xx<-subset(dum, Site == "BM" & Narea != "NA")
# ablineclip(I1w, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B,lty=1, lwd=2, col ="firebrick3")
# xx<-subset(dum, Site == "NM" & Narea != "NA")
# ablineclip(I2w, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B2,lty=3, lwd=2, col ="firebrick3")
legend("topright", c("Ambient", "Warmed"), col=c("lightskyblue1", "lightpink1"), pch=1, lty = 1, horiz = F, bty='n', cex= 1.1)
legend("bottomright", c("North Site", "South Site"), pch=c(2,6), lty =c (1,3), horiz = F, bty='n', cex= 1.1)

mtext(side = 1, expression(italic(N)[area]~(g~N~m^-2)), cex = 1.2, padj = 2, outer= F)
mtext(side = 3, expression(italic(Avicennia~germinans)), cex = 1.2, padj = 0, outer = FALSE)
mtext(side = 2, expression(italic(R)[area^25]*""~(mu*mol~m^-2~s^-1)), cex = 1.2, padj = -1.8, outer= F)
axis(1, at = seq(2.5,6.5,.5), cex.axis = 1.2)
axis(2, las = 2, cex.axis = 1.2)
legend("topleft", c("(a)"), bty = "n", cex = 1.2)
# mtext(side = 2, expression(italic(R)[area]*25^degree*""^C~(mu*mol~m^-2~s^-1)), cex = 1.2, padj = -1.8, outer= F)
##############################################################

dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Spp == "M" & Treatment == "C")
plot(rdref ~ Narea, dum, pch = 2, col= "lightskyblue1", xlim = c(1,4), ylim = c(0.5, 3), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(rdref ~ Narea, dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "C" & Spp == "M")
points(rdref ~ Narea, dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.area"  & Treatment == "W" & Spp == "M")
points(rdref ~ Narea, dum, pch = 6, col= "lightpink1")





dum<-subset(df, Spp == "M" & R_calc == "R.area" )
m1<-lm(rdref~Narea*Treatment*site, dum)
# m1<-lm(rdref~Narea + Treatment + Narea*Site, dum)
anova(m1)
summary(m1)


m1<-lm(rdref~Narea*Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("Narea", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$Narea,na.rm = TRUE),x2=max(dur$Narea,na.rm = TRUE), lwd=2, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$Narea,na.rm = TRUE),x2=max(dum$Narea,na.rm = TRUE), lwd=2, lty = 1, col = "black" )

coef(lm(predicted~x, dat))

# i.nought= 0.40331
# I1=i.nought + 0
# I2=i.nought + -0.47509 + 0.19706
# B= 0.53708
# B2= B + 0.19
# xx<-subset(dum, Site == "BM" & Narea != "NA")
# ablineclip(I2, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B,lty=1, lwd=2, col ="black")
# xx<-subset(dum, Site == "NM" & Narea != "NA")
# ablineclip(I2, x1=min(xx$Narea, na.rm = TRUE),x2=max(xx$Narea, na.rm = TRUE), B2,lty=3, lwd=2, col ="black")


mtext(side = 1, expression(italic(N)[area]~(g~N~m^-2)), cex = 1.2, padj = 2, outer= F)
mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1.2, padj = 0, outer = FALSE)
axis(1, at = seq(1,4,.5), cex.axis = 1.2)
axis(2, las = 2, cex.axis = 1.2)
legend("topleft", c("(b)"), bty = "n", cex = 1.2)



################################################################

 
################################################################
###############################################################
#############

dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Spp == "G" & Treatment == "C")
plot(rdref ~ Nmass..g.N.kg.1., dum, pch = 6, col= "lightskyblue1", xlim = c(10,26), ylim = c(2, 9), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 6, col= "lightpink1")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "G")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 2, col= "lightskyblue1")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "G")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 2, col= "lightpink1")
# legend("top", c("Warmed", "Control"), col=c("firebrick", "blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 1)




dum<-subset(df, Spp == "G" & R_calc == "R.mass" )
m1<-lm(rdref~Nmass..g.N.kg.1.*Treatment*Site, dum)
anova(m1)
summary(m1)


m1<-lm(rdref~Nmass..g.N.kg.1.+Site,dum)
p1<-plot_model(m1, type= c("pred"), terms= c("Nmass..g.N.kg.1.", "Site"))
new<-as.data.frame(p1$data)
dat<-subset(new, group == "NM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dur<-subset(dum, Site == "NM")
ablineclip(m2,x1=min(dur$Nmass..g.N.kg.1.,na.rm = TRUE),x2=max(dur$Nmass..g.N.kg.1.,na.rm = TRUE), lwd=2, lty = 3, col = "black" )
dat<-subset(new, group == "BM")
m2<-lm(predicted~x, dat)
coef(lm(predicted~x, dat))
dum<-subset(dum, Site == "BM")
ablineclip(m2,x1=min(dum$Nmass..g.N.kg.1.,na.rm = TRUE),x2=max(dum$Nmass..g.N.kg.1.,na.rm = TRUE), lwd=2, lty = 1, col = "black" )

# i.nought= 0.24743
# I1=i.nought + 0
# I2=i.nought + -0.36025
# B= 0.26289
# B2= B + 0.08167
# 
# xx<-subset(dum, Site == "BM" & Nmass..g.N.kg.1. != "NA")
# ablineclip(I1, x1=min(xx$Nmass..g.N.kg.1., na.rm = TRUE),x2=max(xx$Nmass..g.N.kg.1., na.rm = TRUE), B,lty=1, lwd=2, col ="black")
# xx<-subset(dum, Site == "NM" & Nmass..g.N.kg.1. != "NA")
# ablineclip(I2, x1=min(xx$Nmass..g.N.kg.1., na.rm = TRUE),x2=max(xx$Nmass..g.N.kg.1., na.rm = TRUE), B,lty=3, lwd=2, col ="black")
mtext(side = 2, expression(italic(R)[mass^25]*""~(n*mol~g^-1~s^-1)), cex = 1.2, padj = -1.8, outer= F) 

mtext(side = 1, expression(italic(N)[mass]~(g~N~kg^-1)), cex = 1.2, padj = 2, outer= F)
# mtext(side = 2, expression(Nmass), cex = 0.8, padj = -1.65, outer= F)
# mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)
# mtext(side = 3, expression(South~Site), cex = 1.5, padj = -0.5, outer = F)
axis(1, at = seq(10,26,2), cex.axis = 1.2)
axis(2, las = 2, cex.axis = 1.2, labels = T)
legend("topleft", c("(c)"), bty = "n", cex = 1.2)


################################3

dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Spp == "M" & Treatment == "C")
plot(rdref ~ Nmass..g.N.kg.1., dum, pch = 2, col= "lightskyblue1", xlim = c(10,22), ylim = c(6, 16), xaxt = "n", yaxt = "n", xlab = "", ylab = "")
dum<-subset(df, Site == "BM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 2, col= "lightpink1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "C" & Spp == "M")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 6, col= "lightskyblue1")
dum<-subset(df, Site == "NM" & R_calc == "R.mass"  & Treatment == "W" & Spp == "M")
points(rdref ~ Nmass..g.N.kg.1., dum, pch = 6, col= "lightpink1")
# legend("top", c("Warmed", "Control"), col=c("firebrick", "blue"), pch=1, lty = 1, horiz = F, bty='n', cex= 1)

dum<-subset(df, Spp == "M" & R_calc == "R.mass" )
m1<-lm(rdref~Nmass..g.N.kg.1.*Treatment*Site, dum)
anova(m1)
summary(m1)
p1<-plot_model(m1, type= c("pred"), terms= c("Nmass..g.N.kg.1."))
new<-as.data.frame(p1$data)
m2<-lm(predicted~x, new)
coef(lm(predicted~x, new))
ablineclip(m2,x1=min(dum$Nmass..g.N.kg.1.,na.rm = TRUE),x2=max(dum$Nmass..g.N.kg.1.,na.rm = TRUE), lwd=2.2, col = "grey69" )

mtext(side = 1, expression(italic(N)[mass]~(g~N~kg^-1)), cex = 1.2, padj = 2, outer= F)
# mtext(side = 3, expression(italic(Spartina~alterniflora)), cex = 1, padj = 0, outer = FALSE)
# mtext(side = 3, expression(North~Site), cex = 1.5, padj = -0.5, outer = F)
axis(1, at = seq(10,22,2), cex.axis = 1.2)
axis(2, las = 2, cex.axis = 1.2)
legend("topleft", c("(d)"), bty = "n", cex = 1.2)


dev.off()
