#### Soil R Models that work for Incubation 2
# Michelle Wang | JAN 2023

# Load packages + functions
library(tidyverse)
library(SoilR)
library(FME)

# Intializations
i = 4 # CHANGE IF INTERESTED, treatment type 1 = 'Soil Control', 2 = 'Corn Stover', 3 = 'AD HLFB', '4 = C-CBP HLFB', 5 = 'DASE HLFB'
n = 2 # start this at 2 for things to save correctly, column index, save to right column in fitFrames

# Initial Soil + Residue input into treatment jars [mg C]
CinitsP <- c(514.4336596, 1145.656643, 1059.347782, 1151.299211, 1188.126723) # Information from INC2 -> IRMS -> "IRMS_summary" -> IRMS_Pre
CinitsV <- c(1113.366093, 1752.370126, 1651.682688, 1751.267604, 1783.200554)

AICc_2pf_tot <- numeric(length=5)
AICc_2ps_tot <- numeric(length=5)
AICc_2pp_tot <- numeric(length=5)

onepf_par <- list(length = 5)
twopf_par <- list(length = 5)
twopp_par <- list(length = 5)
twops_par <- list(length = 5)

# TIME VECTOR
#days <- 0:3650
end_day <- 365*100  ### CHANGE TIME VECTOR [days] ### 
days <- seq(from = 1, to = end_day, by = 365/5)
#days <- seq(0, round(last(CO2flux$time)))  # this days vector is just the length of your data

# INPUT VECTOR - CHANGE HERE, Either you can do inputs_frame = 0 for no inputs, or use the other loop
# C from residue for each treatment [mg]
inputs_valsP <- c(0,	63.28138108,	54.4071363,	63.7547334,	67.28124268) # Information from INC2 -> IRMS -> "IRMS_summary" -> IRMS_Pre  
inputs_valsV <- c(0,	63.49023052,	53.93092504, 63.54292365,	67.28124268) 



# OPTION 2: Inputs throughout year every year, data frame option
# inputs <- data.frame(matrix(ncol = length(inputs_valsP), nrow = length(days)))
# a = 1 # days counter
# b = 1 # treatment counter
# c = 1 # years counter

# # THIS JUST IS NOT RIGHT, it works for an everyday vector
# while (b < 6) {
#   while (a < (end_day+1)) {
#     if (a == 365*c) {  # input end of every year, if you also want first day use this code for if: (a == 1 | a == 365*n)
#       inputs[a, b] <- inputs_valsV[b] # CHANGE V/P HERE
#       c = c + 1
#     }
#     else {
#       inputs[a,b] <- 0
#     }
#     a = a + 1
#   }
#   a = 1
#   b = b + 1
#   c = 1
# }
# inputs_mainframe <- data.frame(days, inputs)

# OPTION 1: Inputs every end of year, 99 inputs in dataframe, this only works for inputs w/ time steps of 365/5 days
inputs_mainframe <- data.frame(days, matrix(0, length(days), 5))
colnames(inputs_mainframe) <- c('days', '1', '2', '3', '4', '5')
a = 2 # column counter
b = 1 # inputs_vals counter
while (a < 7) {
  inputs_mainframe[seq(from = 6, to = length(days), by = 5), a] <-  inputs_valsV[b] ### CHANGE V/P HERE
  a = a + 1
  b = b + 1
}

# OPTION 2: 0 inputs throughout year after first one
# inputs_frame = 0

# CUMM. CO2 PRODUCED VECTOR
totalfitCumm <- as.data.frame(matrix(nrow = length(days), ncol = 21))
totalfitCumm[, 1] <- days 

# Graphing theme
theme_C <- theme_light() +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 30), #for facetwrapped plots
        strip.background = element_rect(color="black", fill="#93C5FF", size=1.5, linetype="solid"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))

# Read in data
CO2flux_0 <- read.csv("data_modV.csv", header=TRUE) ### CHANGE V/P ###

# LOOP THROUGH ALL MODELS
#while (i < 6) {    # COMMENT IN/OUT TO CHECK FOR ONE TREATMENT, loop through 5 treatments

CO2flux <- CO2flux_0 %>%
  filter(Num == i) %>%    # loop through treatment
  select(time, cummCO2) 

Ctotal= CinitsV[i] ### CHANGE V/P, initial total C from IRMS (mg C in treatment) ###
inputs_frame <- inputs_mainframe[, c(1, i+1)] ### CHANGE THIS BY COMMENTING  IN/OUT DEPENDING ON INPUTS OR NOT ### subset main dataframe so it's just the days and relevant treatment

#cost function
eCO2cost=function(pars){
  modelOutput=eCO2func(pars)
  return(modCost(model=modelOutput, obs=CO2flux[,1:2]))
}

# ###one pool model####
# eCO2func = function(pars) {
#   mod=OnepModel(
#     t=days,
#     k = pars[1],
#     C0 = Ctotal,
#     In = inputs_frame,
#     pass=TRUE
#   )
#   AccR=getAccumulatedRelease(mod)
#   return(data.frame(time=days,cummCO2=rowSums(AccR)))
# }
# 
# inipars=c(k=.0001) 
# 
# eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
#                upper=c(Inf),lower=c(0))
# options(scipen = 999)
# onepf_par[[i]] <- eCO2fit$par
# 
# fitmod=OnepModel(t=days, k=eCO2fit$par[1], 
#                  In = inputs_frame,
#                  C0=Ctotal)
# fitCumm=getAccumulatedRelease(fitmod)
# a <- rowSums(fitCumm)
# 
# # #Plot the results, crap fit, which is not unexpected, it also does warn us that Nelder-Mead sucks at single optimization
# # plot(CO2flux[,1:2],type="p",xlab="Days",
# #      ylab="Cummulative respiration (mg C g-1 soil)")  #IT'S NOT mg/g UNLESS I DO DIVIDE BY 50
# # lines(rowSums(fitCumm))
# 
# fitCumm1 <- rowSums(fitCumm)
# totalfitCumm[, n] <- fitCumm1
# n <- n + 1
# 
# fitframe1 <- data.frame(days, fitCumm1)
# 
# #SO SLOW W THIS ON
# # plot1 <- ggplot() +
# #   geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
# #   geom_line(data = fitframe1, aes(x = days, y = fitCumm1)) +
# #   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '1 Pool Model') +
# #   theme_C
# # plot1
# 
# ###two pool feedback model####
# eCO2func=function(pars){
#   mod=TwopFeedbackModel(
#     t=days,
#     ks=pars[1:2],
#     a21=pars[3]*pars[1],
#     a12=pars[4]*pars[2], 
#     C0=Ctotal*c(pars[5],1-pars[5]), 
#     In = inputs_frame,
#     pass=TRUE
#   )
#   AccR=getAccumulatedRelease(mod)
#   return(data.frame(time=days,cummCO2=rowSums(AccR)))
# }
# 
# inipars=c(k1=0.5,k2=0.05,alpha21=0.5,alpha12=0.1,gamma=0.5)
# 
# eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
#                upper=c(Inf,Inf,1,1,1),lower=c(0,0,0,0,0))
# options(scipen = 999)
# twopf_par[[i]] <- eCO2fit$par
# 
# #Run the model again with best parameter set
# fitmod=TwopFeedbackModel(t=days, ks=eCO2fit$par[1:2], 
#                          a21=eCO2fit$par[3]*eCO2fit$par[1],
#                          a12=eCO2fit$par[4]*eCO2fit$par[2], 
#                          C0=Ctotal*c(eCO2fit$par[5],1-eCO2fit$par[5]), 
#                          In = inputs_frame,)
# fitCumm=getAccumulatedRelease(fitmod)
# 
# #Use AIC to evaluate which model is the best fit (should be lowest AIC)
# npars=length(eCO2fit$par)
# AIC_2pf=(2*npars)-2*log(eCO2fit$ms) 
# AICc_2pf =AIC_2pf+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1)) 
# 
# # #Plot the results
# # plot(CO2flux[,1:2],type="p",xlab="Days",
# #      ylab="Cummulative respiration (mg C g-1 soil)")
# # lines(rowSums(fitCumm))
# 
# fitCumm2 <- rowSums(fitCumm)
# totalfitCumm[, n] <- fitCumm2
# n <- n + 1
# fitframe2 <- data.frame(days, fitCumm2)
# 
# ## SO SLOW W THIS ON
# # plot2pf <- ggplot() +
# #   geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
# #   geom_line(data = fitframe2, aes(x = days, y = fitCumm2)) +
# #   labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Feedback Model') +
# #   theme_C
# # plot2pf

###two pool series model####
eCO2func=function(pars){
  mod=TwopSeriesModel(
    t=CO2flux$time, # run this only for the incubation data, this was "days" and it worked prev. till 1/18 caitlin's comments
    ks=pars[1:2],
    a21=pars[3]*pars[1],
    C0=Ctotal*c(pars[4],1-pars[4]), 
    In = 0, # run this for 0 since that's the incubation
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
}

inipars=c(k1=0.5,k2=0.05,alpha21=0.5,gamma=0.5)

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,1,1),lower=c(0,0,0,0))
options(scipen = 999)
twops_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopSeriesModel(t=days, ks=eCO2fit$par[1:2], 
                       a21=eCO2fit$par[3]*eCO2fit$par[1],
                       C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]), 
                       In = 30,)
fitCumm=getAccumulatedRelease(fitmod)

# Try getC to get carbon stocks: ??? why is there a bump in the beginning, is it bc of inputs?
Ct1=getC(fitmod)
plot(days, Ct1[,2], type="l", ylab="Carbon stocks (mg)", xlab="Time (days)") 
lines(days, Ct1[,1], col = 'green')

npars=length(eCO2fit$par)
AIC_2ps=(2*npars)-2*log(eCO2fit$ms) 
AICc_2ps=AIC_2ps+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1)) 

#Plot the results
plot(CO2flux[,1:2],type="p",xlab="Days",
     ylab="Cummulative respiration (mg C g-1 soil)")
lines(rowSums(fitCumm))

fitCumm3 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm3
n <- n + 1
fitframe3 <- data.frame(days, fitCumm3)

## SO SLOW W THIS ON
plot2ps <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
  geom_line(data = fitframe3, aes(x = days, y = fitCumm3)) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Series Model') +
  theme_C
plot2ps

###two pool parallel model####
eCO2func=function(pars){
  mod=TwopParallelModel(
    t=CO2flux$time, # was days
    ks=pars[1:2],
    gam=pars[3],
    C0=Ctotal*c(pars[3],1-pars[3]), 
    In = 0, # was inputs_frame
    pass=TRUE
  )
  AccR=getAccumulatedRelease(mod)
  return(data.frame(time=CO2flux$time,cummCO2=rowSums(AccR)))
}

inipars=c(k1=0.005,k2=0.000000005,gamma=0.08) #for deeper depths, need different starting values

eCO2fit=modFit(f=eCO2cost,p=inipars,method="Nelder-Mead",
               upper=c(Inf,Inf,1),lower=c(0,0,0))

twopp_par[[i]] <- eCO2fit$par

#Run the model again with best parameter set
fitmod=TwopParallelModel(t=days, ks=eCO2fit$par[1:2],
                         gam=eCO2fit$par[3],
                         C0=Ctotal*c(eCO2fit$par[3],1-eCO2fit$par[3]),
                         In = inputs_frame,)
fitCumm=getAccumulatedRelease(fitmod)

# plot
Ct1=getC(fitmod)
plot(days, Ct1[,2], type="l", ylab="Carbon stocks (mg)", xlab="Time (days)") 
lines(days, Ct1[,1], col = 'green')

write.csv(inputs_mainframe, file = 'inputs.csv') ### CHANGE V/P ###


# #Run the model again with best parameter set
# fitmod=TwopSeriesModel(t=days, ks=eCO2fit$par[1:2], 
#                        a21=eCO2fit$par[3]*eCO2fit$par[1],
#                        C0=Ctotal*c(eCO2fit$par[4],1-eCO2fit$par[4]), 
#                        In=0)
# fitCumm=getAccumulatedRelease(fitmod)

npars=length(eCO2fit$par)
AIC_2pp=(2*npars)-2*log(eCO2fit$ms) 
AICc_2pp=AIC_2pp+(((2*npars^2)+2*npars)/(length(CO2flux[,1])-npars-1)) 

#Plot the results
# plot(CO2flux[,1:2],type="p",xlab="Days",
#      ylab="Cummulative respiration (mg C g-1 soil)")
# lines(rowSums(fitCumm))

fitCumm4 <- rowSums(fitCumm)
totalfitCumm[, n] <- fitCumm4
n <- n + 1
fitframe4 <- data.frame(days, fitCumm4)

## SO SLOW W THIS ON
plot2pp <- ggplot() +
  geom_point(data = CO2flux, aes(x = time, y = cummCO2), shape = 1) +
  geom_line(data = fitframe4, aes(x = days, y = fitCumm4)) +
  labs(x = 'Time [days]', y = 'Cumulative CO2 Released [mg]', title = '2 Pool Parallel Model') +
  theme_C
plot2pp

# PLOTTING
# png('2pp_1.png')
# plot(CO2flux[,1:2],
#      type="p",
#      xlab="Days",
#      ylab="Cummulative Carbon Respired [mg]",
#      main = 'Two Pool Parallel Model Fit of (8.5 mm CS) Respired Carbon Data') +
#   lines(fitCumm4, col = 'orange', lwd = '2')
# dev.off()

## AIC 
#AICc_2pf_tot[i] <- AICc_2pf
AICc_2ps_tot[i] <- AICc_2ps
AICc_2pp_tot[i] <- AICc_2pp

#totalfitCumm <- cbind(days, fitCumm1, fitCumm2, fitCumm3, fitCumm4) 

i <- i+1
print(i)
print(n)
}

# Export AICc
# AICc_tot <- data.frame(abs(AICc_2pf_tot), abs(AICc_2pp_tot), abs(AICc_2ps_tot))
AICc_tot <- data.frame(abs(AICc_2pp_tot), abs(AICc_2ps_tot))
rownames(AICc_tot) <- c('Soil Control', 'Corn Stover', 'AD HLFB', 'C-CBP HLFB', 'DASE HLFB')
# colnames(AICc_tot) <- c('2PF', '2PP', '2PS')
colnames(AICc_tot) <- c('2PP', '2PS')
write.csv(AICc_tot, file = 'AICc_totV_in_100.csv') ### CHANGE V/P ###

# Export Parameters
# write.csv(onepf_par, file = 'onepf_parV.csv') ### CHANGE V/P ###
# write.csv(twopf_par, file = 'twopf_parV.csv') ### CHANGE V/P ###
write.csv(twopp_par, file = 'twopp_parV_in_100.csv') ### CHANGE V/P ###
write.csv(twops_par, file = 'twops_parV_in_100.csv') ### CHANGE V/P ###

# Export the cummCO2
write.csv(totalfitCumm, file = 'projectedcummCO2V_in_100.csv') ### CHANGE V/P ###

