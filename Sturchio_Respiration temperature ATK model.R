library(lattice);library(doBy); library(lubridate)
library(minpack.lm)

rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

#########################################################
#########################################################

home<-"F:/WETFEED Analysis" # home
work<-"F:/WETFEED Analysis" # work
raw<-"F:/WETFEED Analysis"
hist<-"F:/WETFEED Analysis"
lins<-"F:/WETFEED Analysis"
deets<-"F:/WETFEED Analysis"

setwd(work)


df<-read.csv("completeLMA.csv")
df$LMA_g.m2<-df$LMA_kg.m2*1000
df$Date<-as.Date(df$DateTime, "%b %d %Y")
df$Date<-as.factor(df$Date)
df$R.area<-df$Photo*-1
df$R.area_log<-log(df$Area)
df$CTleaf2<-df$CTleaf_out^2
df$R.mass<-(df$R.area*1000)/df$LMA_g.m2
################################################ df$R.mass<-df$LMA * df$Rarea (Once LMA is added you can calculate R.mass and re-run)

R<-0.008314472
K<-273.15

df<-subset(df, KEEP == "Y")
df$var<-interaction(df$UserIDs_in, as.factor(df$Date))

############################################################################################
############################################################################################

newdf<-with(df, data.frame(filenames, Date, UserIDs_in, LMA_g.m2))
newdf<-subset(newdf, UserIDs_in == "") # Just make a shell of a dataframe
df4res<-data.frame(res=as.numeric(),
                   temp=as.numeric(),
                   Date=as.numeric(),
                   UserIDs_in=as.character())
df4pred<-data.frame(pred=as.numeric(),
                    obs=as.numeric())
#########################################################
#########################################################

## prints a pdf for every sampling date
## check to make sure data is clean enough for analysis
## can edit raw csv file (in the 'read.csv' line)
df$alpha<-df$CTleaf_out+K
df$beta<-K+25

dates<-as.character(unique(df$Date))
setwd(raw)
for (dates in unique(df$Date)){
  dat<-subset(df, df$Date == dates)
  pdf(paste("WETFEET_rawdata_",as.character(unique(dat$Date)),".pdf",sep = ""))
  print(xyplot(R.area ~ CTleaf2 | UserIDs_in, data = dat, main = paste(as.character(unique(dat$Date)))))
  dev.off()
}

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

## Atkin 2005 model (doesn't work)

for (reps in unique(df$var)){
  dat<-subset(df, df$var == reps)
  mod<-nlsLM(Rarea = rdref * (xx - (yy*((CTleaf2+25)/2)))**((CTleaf2-25)/10), start = list(rdref = 1, xx = -2, yy = -2), dat)
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
  res.df$temp<-dat$CTleaf2
  res.df$timepoint<-dat$timepoint
  res.df$UserIDs_in<-dat$UserIDs_in
  res.df<-data.frame(res.df)
  df4res<-rbind(res.df, df4res)

  pred<-fitted(mod)
  pred.df<-data.frame(pred)
  pred.df$obs<-dat$R.area
  df4pred<-rbind(df4pred, pred.df)

    setwd(hist)
    pdf(paste("WETFEET_ATKhist_", as.character(unique(dat$var)), ".pdf", sep = ""))
    hist(res.df$res) # Prints histogram for each leaf sampled
    dev.off()

   setwd(hist)
   pdf(paste("ALL_WETFEET_ATK_hist.pdf"))
   hist(df4res$res) # Prints histogram for all data
   dev.off()

    setwd(lins)
    pdf(paste("WETFEET_ATK_obs~pred_", as.character(unique(dat$var)), ".pdf", sep = ""))
    plot(pred.df$pred ~ pred.df$obs)
    dev.off()

   setwd(lins)
   pdf(paste("ALL_WETFEET_ATK_obs~pred.pdf", sep = ""))
   plot(df4pred$pred ~ df4pred$obs); abline(0, 1, lty = 2, col = "firebrick")
   m1<-lm(df4pred$pred ~ df4pred$ob); sm1<-summary(m1)
   legend("topleft", as.character(sm1$r.squared))
dev.off()

 }

 setwd(deets)
 write.csv(newdf, "WETFEET_ATKmod.csv")
