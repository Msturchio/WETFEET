library(lme4); library(emmeans)
library(car); library(effects); library(doBy)
library(multcomp); library(effects)
library(lme4); library(MuMIn); library(lmerTest)
library(pbkrtest); library(vegan)
library(SingleCaseES); library(plotrix)

rm(list=ls())   # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()       # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")       # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# setwd("F:/WETFEED Analysis/Model Outputs/Analysis/Variance Partitioning")
setwd("E:/WETFEED Analysis/Model Outputs/Analysis/Variance Partitioning")

# df<-read.csv("LogP_SPECIES SEPERATED VARIANCE PARTITIONING2.csv")
# spar<-subset(df, spp == "spar")
# germ<-subset(df, spp == " germ")

df<-read.csv("LogP_BIG VARIANCE PARTITION2.csv")

a<-subset(df, Group2 != "Other")
b<-subset(df, Group2 == "Other")

# germ.a<-subset(germ, Group2 != "Other")
# germ.b<-subset(germ, Group2 == "Other")

##################################################################
tiff(file = "Big Var par figure.tiff", height = 10, width = 8, res = 600, units = "in", compression = "zip+p")
par(mfrow = c(2,1), omi = c(0.1,0.1,0.1,0.1), mar = c(4,4,0.5,1))

xx<-c(-500,500); yy<-c(-500,500)

##################################################################
# Box1: Big chunks for spar

plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,100), xlim=c(0,14))

rect(009, 0, 011, sum(df$LMA), col = "lemonchiffon2") # Main groups + 'others'
rect(009, 0, 011, sum(a$LMA), col = "palegreen2") # Main groups
rect(009, 0, 011, sum(a$LMA[a$Group2 == "Residual" | a$Group2 == "Species"]), col = "paleturquoise2") # Partial main groups
rect(009, 0, 011, sum(a$LMA[a$Group2 == "Species"]), col = "steelblue2") # Partial main groups

rect(006, 0, 008, sum(df$q10), col = "lemonchiffon2") # Main groups + 'others'
rect(006, 0, 008, sum(a$q10), col = "palegreen2") # Main groups
rect(006, 0, 008, sum(a$q10[a$Group2 == "Residual" | a$Group2 == "Species"]), col = "paleturquoise2") # Partial main groups
rect(006, 0, 008, sum(a$q10[a$Group2 == "Species"]), col = "steelblue2") # Partial main groups

rect(003, 0, 005, sum(df$Rmass25), col = "lemonchiffon2") # Main groups + 'others'
rect(003, 0, 005, sum(a$Rmass25), col = "palegreen2") # Main groups
rect(003, 0, 005, sum(a$Rmass25[a$Group2 == "Residual" | a$Group2 == "Species"]), col = "paleturquoise2") # Partial main groups
rect(003, 0, 005, sum(a$Rmass25[a$Group2 == "Species"]), col = "steelblue2") # Partial main groups

rect(000, 0, 002, sum(df$Rarea25), col = "lemonchiffon2") # Main groups + 'others'
rect(000, 0, 002, sum(a$Rarea25), col = "palegreen2") # Main groups
rect(000, 0, 002, sum(a$Rarea25[a$Group2 == "Residual" | a$Group2 == "Species"]), col = "paleturquoise2") # Partial main groups
rect(000, 0, 002, sum(a$Rarea25[a$Group2 == "Species"]), col = "steelblue2") # Partial main groups
axis(1, at = c(1,4,7,10), c(expression(italic(R)[area^25]),expression(italic(R)[mass^25]),expression(italic(Q)[10^25]), "LMA" ))
axis(2, at = seq(0,100,20), las = 2)
legend("topright", c("Species","Residual","Date:Species","Other"), bty = "n",
       col = c("steelblue2", "paleturquoise2","palegreen2","lemonchiffon2"), pch = 15, cex = 1.1)

mtext(side = 2, expression(Proportion~of~variance~explained~"(%)"), cex = 1.2, padj = -2.8, outer= F)
legend("bottomright", c("(a)"), bty = "n")

##################################################################
# Box1: Small chunks for spar

plot(yy ~ xx, pch = NA, xlab="",ylab="",xaxt="n",yaxt="n",ylim=c(0,20), xlim=c(0, 20))

rect(009, 0, 011, sum(b$LMA[b$Group == "Treatment" | b$Group == "Site"
                                 | b$Group == "MoYr:Species:Site"
                                 | b$Group == "Site:Species"
                                 | b$Group == "Treatment:Site:Species" 
                                 | b$Group == "Site:Species:Treatment:MoYr"
                                 | b$Group == "Site:Treatment"]), col = "seagreen2")
rect(009, 0, 011, sum(b$LMA[b$Group == "Treatment" | b$Group == "Site"
                            | b$Group == "MoYr:Species:Site"
                            | b$Group == "Site:Species"
                            | b$Group == "Treatment:Site:Species" 
                            | b$Group == "Site:Species:Treatment:MoYr"]), col = "firebrick")
rect(009, 0, 011, sum(b$LMA[b$Group == "Treatment" | b$Group == "Site"
                            | b$Group == "MoYr:Species:Site"
                            | b$Group == "Site:Species"
                            | b$Group == "Treatment:Site:Species" ]), col = "blue2")
rect(009, 0, 011, sum(b$LMA[b$Group == "Treatment" | b$Group == "Site"
                            | b$Group == "MoYr:Species:Site"
                            | b$Group == "Site:Species"]), col = "mediumaquamarine")
rect(009, 0, 011, sum(b$LMA[b$Group == "Treatment" | b$Group == "Site"
                            | b$Group == "MoYr:Species:Site"]), col = "orangered4")
########################
rect(006, 0, 008, sum(b$q10[b$Group == "Site:Species" | b$Group == "MoYr"
                                 | b$Group == "MoYr:Site"
                                 | b$Group == "Treatment"
                                 | b$Group == "Site"]), col = "slateblue1")
rect(006, 0, 008, sum(b$q10[b$Group == "Site:Species" | b$Group == "MoYr"
                            | b$Group == "MoYr:Site"
                            | b$Group == "Treatment"]), col = "orangered3")
rect(006, 0, 008, sum(b$q10[b$Group == "Site:Species" | b$Group == "MoYr"
                            | b$Group == "MoYr:Site"]), col = "sienna2")
rect(006, 0, 008, sum(b$q10[b$Group == "Site:Species" | b$Group == "MoYr"]), col = "magenta4")
rect(006, 0, 008, sum(b$q10[b$Group == "Site:Species"]), col = "mediumaquamarine")

####################

rect(003, 0, 005, sum(b$Rmass25[b$Group == "MoYr:Site" | b$Group == "Site:Species"
                                   | b$Group == "Species:Treatment"
                                   | b$Group == "MoYr:Species:Site"
                                   | b$Group == "Site:Treatment"]), col = "seagreen2")
rect(003, 0, 005, sum(b$Rmass25[b$Group == "MoYr:Site" | b$Group == "Site:Species"
                                | b$Group == "Species:Treatment"
                                | b$Group == "MoYr:Species:Site"]), col = "orangered4")
rect(003, 0, 005, sum(b$Rmass25[b$Group == "MoYr:Site" | b$Group == "Site:Species"
                                | b$Group == "Species:Treatment"]), col = "dodgerblue2")
rect(003, 0, 005, sum(b$Rmass25[b$Group == "MoYr:Site" | b$Group == "Site:Species"]), col = "mediumaquamarine")
rect(003, 0, 005, sum(b$Rmass25[b$Group == "MoYr:Site" ]), col = "sienna2")

#################

rect(000, 0, 002, sum(b$Rarea25[b$Group == "MoYr:Species:Site" | b$Group == "Species:Treatment"]), col = "dodgerblue2")
rect(000, 0, 002, sum(b$Rarea25[b$Group == "MoYr:Species:Site" ]), col = "orangered4")


axis(1, at = c(1,4,7,10), c(expression(italic(R)[area^25]),expression(italic(R)[mass^25]),expression(italic(Q)[10^25]), "LMA" ))
axis(2, at = seq(0,20,2), las = 2)

mtext(side = 2, expression(Proportion~of~variance~explained~"(%)"), cex = 1.2, padj = -2.8, outer= F)
mtext(side = 1, expression(Traits), cex = 2, padj = 2.6, outer= F)
legend("bottomright", c("(b)"), bty = "n")
legend("topright", c("Date","Treatment", "Site","Site:Species","Date:Site","Date:Species:Site",
"Species:Treatment","Site:Treatment", "Treatment:Site:Species", "Site:Species:Treatment:Date"), bty = "n",
       col = c("magenta4", "orangered3","slateblue1","mediumaquamarine","sienna2", "orangered4", "dodgerblue2"
               , "seagreen2", "blue2", "firebrick"), pch = 15, cex = 1.1)

dev.off()

