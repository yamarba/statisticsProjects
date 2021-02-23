#Wide to long **original data in long form but i changed it to wide using STATA then downloded it.
exer2long <- reshape(exer1, idvar="id",         
                     varying=c("pulse1","pulse2","pulse3"), v.names="pulse", 
                     timevar="time", time=1:3, direction="long")

attach(exer2long)

minutes <- time
minutes[time==1] <- 1
minutes[time==2] <- 2
minutes[time==3] <- 3

interaction.plot(minutes, id, pulse, ylim=c(80,120), xlab="Time (in minutes)", ylab="Pulse", 
                 main="Plot of Pulses per Diet by ID", col=c(1:50), legend=F)

interaction.plot(minutes, diet, pulse, type="b", pch=c(19,21), ylim=c(80,        
                                                                      120), xlab="Time (in minutes)", ylab="Pulse",                  
                 main="Mean Pulse of Low Fat Diet & Non-fat Diet", col=c(2,4))

#Analysis of Response Profiles of Pulse Rate 

model <- gls(pulse ~ diet*minutes.f, corr=corSymm(form= ~ time | id), 
             weights = varIdent(form = ~ 1 | minutes.f))

summary(model)
anova(model)

#Analysis of Response Profiles assuming Equal Mean Response at Baseline 

model2 <- gls(pulse ~ I(minutes.f==15) + I(minutes.f==30)
              + I(minutes.f==15 & diet=="1") + I(minutes.f==30 & diet=="1"),                               
              corr=corSymm(form= ~ time | id),
              weights = varIdent(form = ~ 1 | minutes.f))
summary(model2)

model7 <- gls(pulse ~ I(minutes.f==15) + I(minutes.f==30)
              + I(minutes.f==15 & diet=="1") + I(minutes.f==30 & diet=="1"),                               
              corr=corSymm(form= ~ time | id),
              weights = varIdent(form = ~ 1 | minutes.f),method="ML")

model8 <- gls(pulse ~ I(minutes.f==15) + I(minutes.f==30),                               
              corr=corSymm(form= ~ time | id),
              weights = varIdent(form = ~ 1 | minutes.f),method="ML")

#COVARIANCE

mins <- time*2
mins.f <- factor(mins, c(0,1,2))
diet.f <- factor(diet, c(1,2))
ttime <- time
ttime[time==0] <- 1
ttime[time==1] <- 2
ttime[time==2] <- 3

#Unstructured Covariance
modelX <- gls(y ~ diet.f*mins.f, na.action=na.omit, 
              corr=corSymm(form= ~ ttime | id), 
              weights = varIdent(form = ~ 1 | ttime))
summary(modelX)

#Autoregressive Covariance
modelY <- gls(y ~ diet.f*mins.f, na.action=na.omit, corr=corAR1(form= ~ ttime | id)) 
summary(modelY)

#Exponential Covariance
modelZ <- gls(y ~ diet.f*mins.f, na.action=na.omit, corr=corExp(form= ~ mins | id))
summary(modelZ)

anova(modelX, modelY, modelZ)