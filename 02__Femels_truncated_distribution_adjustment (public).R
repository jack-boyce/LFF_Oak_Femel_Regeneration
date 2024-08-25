
## Author: Benjamin Rosenbaum, iDiv

library("ExtDist")
library("truncdist")

set.seed(123)

X = rNormal_trunc_ab(n=1000, mu=1, sigma=2, a=0, b=5)
hist(X)
# built-in max-likelihood optimizer doesn't work well, wants to estimate boundaries too
# est.par <- eNormal_trunc_ab(X)
# est.par
# plot(est.par)

neg.log.lik = function(x, par){ # minimize negative log-likelihood
  return(-lNormal_trunc_ab(x, params=list(mu=par[1], sigma=par[2], a=50, b=500), logL=TRUE) )
}

# start optimizer with naive mean and naive sd as initial values
optim(par=c(mean(X),sd(X)),
      fn=neg.log.lik,
      x=X)



# all strange oaks are considered to be SEI
dat = read.table("auwald_data_femel_Sachsenforst_2023.txt", header=T)
unique(dat$PlotID)

# 15,16,17,18,Pausnitz

#15
dat15SEI = dat[dat$PlotID==15 & dat$Species=="SEI",] # 83
hist(dat15SEI$DBH_mm, breaks=20, xlim=c(0,400))
mean(dat15SEI$DBH_mm)
sd(dat15SEI$DBH_mm)

est15 = optim(par=c(mean(dat15SEI$DBH_mm),sd(dat15SEI$DBH_mm)),
              fn=neg.log.lik,
              x=dat15SEI$DBH_mm)

#hist(rnorm(1000, mean=93.09912, sd=47.99647))
obs = 1-pnorm(50, mean=134.72871, sd=53.88574)
total15 = 83/obs # 88.1
88.1*10000/900 # 979


#16
dat = read.table("Adjusted_auwald_data_femel_Sachsenforst_2023_mod.txt", header=T)
dat16SEI = dat[dat$PlotID==16 & dat$Species=="SEI",] # 54
hist(dat16SEI$DBH_mm, breaks=20, xlim=c(0,400))
mean(dat16SEI$DBH_mm)
sd(dat16SEI$DBH_mm)

est16 = optim(par=c(mean(dat16SEI$DBH_mm),sd(dat16SEI$DBH_mm)),
              fn=neg.log.lik,
              x=dat16SEI$DBH_mm)

#hist(rnorm(1000, mean=93.09912, sd=47.99647))
obs = 1-pnorm(50, mean=160.97323, sd=42.26636)
total16 = 54/obs # 69.3
54.2*10000/675 # 803


#17
dat17SEI = dat[dat$PlotID==17 & dat$Species=="SEI",] # 17
hist(dat17SEI$DBH_mm, breaks=30, xlim=c(0,400))
mean(dat17SEI$DBH_mm)
sd(dat17SEI$DBH_mm)

est17 = optim(par=c(mean(dat17SEI$DBH_mm),sd(dat17SEI$DBH_mm)),
              fn=neg.log.lik,
              x=dat17SEI$DBH_mm)

#hist(rnorm(1000, mean=93.09912, sd=47.99647))
obs = 1-pnorm(50, mean=138.29017, sd=40.59075)
total17 = 17/obs # 17.26
17.26*10000/225 # 767


#18
dat18SEI = dat[dat$PlotID==18 & dat$Species=="SEI",] # 47
hist(dat18SEI$DBH_mm, breaks=20, xlim=c(0,400))
mean(dat18SEI$DBH_mm)
sd(dat18SEI$DBH_mm)

est18 = optim(par=c(mean(dat18SEI$DBH_mm),sd(dat18SEI$DBH_mm)),
              fn=neg.log.lik,
              x=dat18SEI$DBH_mm)

#hist(rnorm(1000, mean=93.09912, sd=47.99647))
obs = 1-pnorm(50, mean=133.83022, sd=56.97596)
total18 = 47/obs # 50.57
50.57*10000/600 # 843

#Pausnitz
datPSEI = dat[dat$PlotID=="Pau?nitz" & dat$Species=="SEI",] # 91
hist(datPSEI$DBH_mm, breaks=20, xlim=c(0,400))
mean(datPSEI$DBH_mm)
sd(datPSEI$DBH_mm)

estP = optim(par=c(mean(datPSEI$DBH_mm),sd(datPSEI$DBH_mm)),
              fn=neg.log.lik,
              x=datPSEI$DBH_mm)

#hist(rnorm(1000, mean=93.09912, sd=47.99647))
obs = 1-pnorm(50, mean=62.07663, sd=31.42542)
totalP = 91/obs # 140
140*10000/400 # 3500

