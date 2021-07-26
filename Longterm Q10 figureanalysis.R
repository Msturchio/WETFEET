
K=273.15
# 

dat<-subset(df, R_calc == "R.area" & Spp == "G" & Site == "BM")
dat$alpha=dat$AirTemp_C.mean+K
dat$beta<-K+28
plot(dat$Rinsitu~dat$AirTemp_C.mean)

mod<-nlsLM(Rinsitu ~ rdref * (q10) ^ ((alpha-beta)/10), start = list(rdref = 1, q10 = 2), dat)
summary(mod)

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.62611 * (1.41738) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,30,1)) ~ seq(16,30,1), type = "l", lwd = 2, col = "grey50")


K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {1.62611 * (1.41738) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,30,1)) ~ seq(16,30,1), type = "l", lwd = 2, col = "grey50")

K<-273.15; df$alpha<-df$Temp_C.mean+K; df$beta<-K+25
q10 <- function(x) {R25est * (Q10est) ^ (((x+273.15)-298.15)/10)}
points(q10(seq(16,30,1)) ~ seq(16,30,1), type = "l", lwd = 2, col = "grey50")

