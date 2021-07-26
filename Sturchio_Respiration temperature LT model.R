library(lattice);library(doBy); library(lubridate)
library(minpack.lm)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"/Volumes/MATT'S USB/WETFEED Analysis" # home
work<-"/Volumes/MATT'S USB/WETFEED Analysis" # work
raw<-"/Volumes/MATT'S USB/WETFEED Analysis"
hist<-"/Volumes/MATT'S USB/WETFEED Analysis"
lins<-"/Volumes/MATT'S USB/WETFEED Analysis"
deets<-"/Volumes/MATT'S USB/WETFEED Analysis"

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
# df<-subset(df, KEEP == "Y")
df$var<-interaction(df$UserIDs_in, as.factor(df$Date))

df$alpha<-df$CTleaf_out+K
df$beta<-K+25

#########################################################
#########################################################

## prints a pdf for every sampling date
## check to make sure data is clean enough for analysis
## can edit raw csv file (in the 'read.csv' line)

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

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  dat$alpha<-1/298.15
  dat$beta<-1/dat$Tk
  mod<-nlsLM(R.area ~ rdref * exp( (Ea/0.008314472) *(alpha-beta)), start = list(rdref = 1, Ea = 30), dat)
  mod.sum<-summary(mod)
  rdref<-mod.sum$parameters[1,1] # isolate the rdref estimate
  rEa<-mod.sum$parameters[2,1] # isolate the Ea estimate
  temp.df<-data.frame(rdref, rEa)
  temp.df$timepoint<-unique(dat$timepoint)
  temp.df$Treatment<-unique(dat$Treatment)
  temp.df$species<-unique(dat$species)
  temp.df$plot<-unique(dat$plot)
  temp.df$Date<-unique(dat$Date)
  temp.df$Site<-unique(dat$Site)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  newdf<-rbind(temp.df, newdf)

  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf_out
  res.df$timepoint<-dat$timepoint
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)

  pred<-fitted(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-dat$R.area
  df4pred<-rbind(df4pred, pred.df)

  # setwd(hist)
  # pdf(paste("WETFEET_LT_hist_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # hist(res.df$res) # Prints histogram for each leaf sampled
  # dev.off()

  setwd(hist)
  pdf(paste("ALL_WETFEET_LT_histRarea.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()

  # setwd(lins)
  # pdf(paste("WETFEET_LT_obs~pred_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # plot(pred.df$pred ~ pred.df$obs)
  # dev.off()

  setwd(lins)
  pdf(paste("ALL_WETFEET_LT_obs~predRarea.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  m1<-lm(df4pred$pred ~ df4pred$ob); sm1<-summary(m1)
  legend("topleft", as.character(sm1$r.squared))
  dev.off()

  setwd(lins)
  pdf(paste("ALL_WETFEET_LT_res~tleafRarea.pdf", sep = ""))
  plot(df4res$res ~ df4res$temp); abline(h=0, lty = 2, col = "firebrick")
  dev.off()
  
}

R.area25LPR<-newdf
R.area25LPR$R_calc<-"R.area"
###########################3
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

##########################################################

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  dat$alpha<-1/298.15
  dat$beta<-1/dat$Tk
  mod<-nlsLM(R.mass ~ rdref * exp( (Ea/0.008314472) *(alpha-beta)), start = list(rdref = 1, Ea = 30), dat)
  mod.sum<-summary(mod)
  rdref<-mod.sum$parameters[1,1] # isolate the rdref estimate
  rEa<-mod.sum$parameters[2,1] # isolate the Ea estimate
  temp.df<-data.frame(rdref, rEa)
  temp.df$timepoint<-unique(dat$timepoint)
  temp.df$Treatment<-unique(dat$Treatment)
  temp.df$species<-unique(dat$species)
  temp.df$plot<-unique(dat$plot)
  temp.df$Date<-unique(dat$Date)
  temp.df$Site<-unique(dat$Site)
  temp.df$UserIDs_in<-unique(dat$UserIDs_in)
  newdf<-rbind(temp.df, newdf)
  
  res<-residuals(mod)
  res.df<-data.frame(res)
  res.df$temp<-dat$CTleaf_out
  res.df$timepoint<-dat$timepoint
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)
  
  pred<-fitted(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-dat$R.mass
  df4pred<-rbind(df4pred, pred.df)
  
  # setwd(hist)
  # pdf(paste("WETFEET_LT_hist_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # hist(res.df$res) # Prints histogram for each leaf sampled
  # dev.off()
  
  setwd(hist)
  pdf(paste("ALL_WETFEET_LT_histRmass.pdf"))
  hist(df4res$res) # Prints histogram for all data
  dev.off()
  
  # setwd(lins)
  # pdf(paste("WETFEET_LT_obs~pred_", as.character(unique(dat$var)), ".pdf", sep = ""))
  # plot(pred.df$pred ~ pred.df$obs)
  # dev.off()
  
  setwd(lins)
  pdf(paste("ALL_WETFEET_LT_obs~predRmass.pdf", sep = ""))
  plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
  m1<-lm(df4pred$pred ~ df4pred$ob); sm1<-summary(m1)
  legend("topleft", as.character(sm1$r.squared))
  dev.off()
  
  setwd(lins)
  pdf(paste("ALL_WETFEET_LT_res~tleafRmass.pdf", sep = ""))
  plot(df4res$res ~ df4res$temp); abline(h=0, lty = 2, col = "firebrick")
  dev.off()
  
}

R.mass25LPR<-newdf
R.mass25LPR$R_calc<-"R.mass"

output<-rbind(R.mass25LPR,R.area25LPR)

setwd(deets)
write.csv(output, "WETFEET_LTmod.csv")
