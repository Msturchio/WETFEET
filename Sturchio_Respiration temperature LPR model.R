library(lattice);library(doBy); library(lubridate)
library(minpack.lm)

library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(effects); library(nlme)
library(MuMIn); library(car)
library(sjPlot)
library(lattice);library(doBy); library(lubridate)
library(minpack.lm); library(plotrix)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"C:/Users/N01411578/Desktop" # home
work<-"C:/Users/N01411578/Desktop" # work
raw<-"C:/Users/N01411578/Desktop"
hist<-"C:/Users/N01411578/Desktop"
lins<-"C:/Users/N01411578/Desktop"
deets<-"C:/Users/N01411578/Desktop"

setwd(work)


df<-read.csv("completeLMA.csv")
df<-subset(df, KEEP == "Y")


df$Date<-as.Date(df$DateTime, format = "%b %d %Y")
df$R.area<-df$Photo*-1
df$LMA_g.m2<-df$LMA_kg.m2*1000
df$R.mass<-(df$R.area*1000)/df$LMA_g.m2
df$R.area_log<-log(df$R.area)
df$CTleaf2<-df$CTleaf_out*df$CTleaf_out

R<-0.008314472
K<-273.15
df$Tk<-df$CTleaf_out+K
df$Tk_inv<-1/df$Tk

df$var<-interaction(df$UserIDs_in, as.factor(df$Date))

df$alpha<-df$CTleaf_out+K
df$beta<-K+25


##########################################################
##########################################################

# Create empty dataframes to store data 

reps<-as.character(unique(df$var))

newdf<-df[,1:7]
newdf<-subset(newdf, UserIDs_in == "") # Just make a shell of a dataframe
df4res<-data.frame(res=as.numeric(),
                   temp=as.numeric(),
                   timepoint=as.numeric(),
                   UserIDs_in=as.character())
dat.pred<-data.frame(predicted=as.numeric(),
                     observed=as.numeric())
df4pred<-data.frame(pred=as.numeric(),
                    obs=as.numeric())

##########################################################

## For the log polynomial model

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  mod<-lm(log(R.area) ~ CTleaf_out + CTleaf2, dat)
  mod.sum<-summary(mod)
  
  intercept<-mod.sum$coefficients[1,1]
  CTleaf_out.slope<-mod.sum$coefficients[2,1]
  CTleaf2.slope<-mod.sum$coefficients[3,1]
  
  temp.df<-data.frame(intercept, CTleaf_out.slope, CTleaf2.slope)
  temp.df$timepoint<-unique(dat$timepoint)
  temp.df$Treatment<-unique(dat$Treatment)
  temp.df$species<-unique(dat$species)
  temp.df$plot<-unique(dat$plot)
  temp.df$Date<-unique(dat$Date)
  temp.df$Site<-unique(dat$Site)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$rdref<-exp(temp.df$intercept+(temp.df$CTleaf_out.slope*25)+(temp.df$CTleaf2.slope*25**2))
  temp.df$q10<-exp(10*(temp.df$CTleaf_out.slope+(2*temp.df$CTleaf2.slope*25)))
  temp.df$LMA_g.m2<-unique(dat$LMA_g.m2)
  temp.df$a<-intercept
  temp.df$b<-CTleaf_out.slope
  temp.df$c<-CTleaf2.slope
  
  newdf<-rbind(temp.df, newdf)

  pred<-predict(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-log(dat$R.area)
  df4pred<-rbind(df4pred, pred.df)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf_out
  res.df$timepoint<-dat$timepoint
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  # setwd(hist)
  # pdf(paste("WETFEET_LPR_hist_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # hist(res.df$res) # Prints histogram for each leaf sampled
  # dev.off()

  setwd(hist)
  pdf(paste("ALL_WETFEET_LPR_histR.area.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()

  # setwd(lins)
  # pdf(paste("WETFEET_LPR_obs~pred_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # plot(pred.df$pred ~ pred.df$obs)
  # dev.off()

  setwd(lins)
  pdf(paste("ALL_WETFEET_LPR_obs~predR.area.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  m1<-lm(df4pred$pred ~ df4pred$ob); sm1<-summary(m1)
  legend("topleft", as.character(sm1$r.squared))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_WETFEET_LPR_res~tleafR.area.pdf", sep = ""))
  plot(df4res$res ~ df4res$temp); abline(h=0, lty = 2, col = "firebrick")
  dev.off()

}


R.area25LPR<-newdf
R.area25LPR$R_calc<-"R.area"

##########################
############################


reps<-as.character(unique(df$var))

newdf<-df[,1:7]
newdf<-subset(newdf, UserIDs_in == "") # Just make a shell of a dataframe
df4res<-data.frame(res=as.numeric(),
                   temp=as.numeric(),
                   timepoint=as.numeric(),
                   UserIDs_in=as.character())
dat.pred<-data.frame(predicted=as.numeric(),
                     observed=as.numeric())
df4pred<-data.frame(pred=as.numeric(),
                    obs=as.numeric())


for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  mod<-lm(log(R.mass) ~ CTleaf_out + CTleaf2, dat)
  mod.sum<-summary(mod)
  
  intercept<-mod.sum$coefficients[1,1]
  CTleaf_out.slope<-mod.sum$coefficients[2,1]
  CTleaf2.slope<-mod.sum$coefficients[3,1]
  
  temp.df<-data.frame(intercept, CTleaf_out.slope, CTleaf2.slope)
  temp.df$timepoint<-unique(dat$timepoint)
  temp.df$Treatment<-unique(dat$Treatment)
  temp.df$species<-unique(dat$species)
  temp.df$plot<-unique(dat$plot)
  temp.df$Date<-unique(dat$Date)
  temp.df$Site<-unique(dat$Site)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  temp.df$rdref<-exp(temp.df$intercept+(temp.df$CTleaf_out.slope*25)+(temp.df$CTleaf2.slope*25**2))
  temp.df$q10<-exp(10*(temp.df$CTleaf_out.slope+(2*temp.df$CTleaf2.slope*25)))
  temp.df$LMA_g.m2<-unique(dat$LMA_g.m2)
  temp.df$a<-intercept
  temp.df$b<-CTleaf_out.slope
  temp.df$c<-CTleaf2.slope
  
  newdf<-rbind(temp.df, newdf)
  
  pred<-predict(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-log(dat$R.mass)
  df4pred<-rbind(df4pred, pred.df)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf_out
  res.df$timepoint<-dat$timepoint
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  # setwd(hist)
  # pdf(paste("WETFEET_LPR_hist_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # hist(res.df$res) # Prints histogram for each leaf sampled
  # dev.off()
  
  setwd(hist)
  pdf(paste("ALL_WETFEET_LPR_histRmass.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()
  
  # setwd(lins)
  # pdf(paste("WETFEET_LPR_obs~pred_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # plot(pred.df$pred ~ pred.df$obs)
  # dev.off()
  
  setwd(lins)
  pdf(paste("ALL_WETFEET_LPR_obs~predRmass.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  m1<-lm(df4pred$pred ~ df4pred$ob); sm1<-summary(m1)
  legend("topleft", as.character(sm1$r.squared))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_WETFEET_LPR_res~tleafRmass.pdf", sep = ""))
  plot(df4res$res ~ df4res$temp); abline(h=0, lty = 2, col = "firebrick")
  dev.off()
  
}


R.mass25LPR<-newdf
R.mass25LPR$R_calc<-"R.mass"
output<-rbind(R.mass25LPR,R.area25LPR)

setwd(deets)
write.csv(output, "here is the intercepts.csv")

df<-read.csv("here is the intercepts.csv")
df$Date<-as.Date(df$Date, "%Y-%m-%d")
df$Site<-substr(df$UserIDs_in, 1, 2)
df$Plot<-substr(df$UserIDs_in, 3, 3)
df$Spp<-substr(df$UserIDs_in, 4, 4)
df$Treatment<-substr(df$UserIDs_in, 5, 5)
df$MoYr<-as.factor(floor_date(df$Date, "month"))
new<-summaryBy(a+b+c~ Site + Treatment + MoYr + Spp, FUN = c(mean,std.error), na.rm = T, df)
write.csv(new, "them intercepts.csv")
