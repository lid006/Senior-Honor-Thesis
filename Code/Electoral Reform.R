library("readxl")
library("dplyr")
library("tidyverse")
library("lfe")
library("texreg")
library("olsrr")
library("sandwich")
library("lmtest")
library("car")

mydata <- read_excel("ELECTORAL REFORM STATS.xlsx", sheet = 1)

#difference-in-difference analysis
#Early voting
mydata$y = mydata$TV
mydata = mydata %>%
  mutate(time = (year >= 2016), treated = (EV > 0),
         did1 = time*treated)

#Preregistration
mydata = mydata %>%
  mutate(time = (year >= 2016), treated = (PR > 0),
         did2 = time*treated)

#SDR/EDR
mydata = mydata %>%
  mutate(time = (year >= 2016), treated = (SEDR > 0),
         did3 = time*treated)

#Online Registration
mydata = mydata %>%
  mutate(time = (year >= 2016), treated = (OR > 0),
         did4 = time*treated)

#No-Excuse Absentee Voting
mydata = mydata %>%
  mutate(time = (year >= 2016), treated = (AV > 0),
         did5 = time*treated)

# Composite Scale Regression
mydata$cs1 = (mydata$EV + mydata$PR + mydata$OR + mydata$SEDR + mydata$AV)/5

mydata$cs2 = (mydata$SEDR + mydata$OR + mydata$PR)/3

reg1 <- felm(y ~ cs1 | State + year, data = mydata)
reg2 <- felm(y ~ cs2 | State + year, data = mydata)

# export result to LATEX
texreg(reg1, 
       digits=2, 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE, 
       custom.coef.names ="Composite Scale 1",
       custom.note = "*With Fixed Effect: States and Year")
#export result to LATEX
texreg(reg2, 
       omit.coef = "time|treated", digits=1, 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE, 
       custom.coef.names =c("Early Voting", "Preregistration", "Same/Election Day Registration", "Online Registration", "No-excuse Absentee Voting"),
       custom.note = "*With Fixed Effect: States and Year")


# Individual Treatment Regression
reg <- felm(y ~ did1 + did2 + did3 + did4 + did5 | State + year, data = mydata)
regs <- lm(y ~ did1 + did2 + did3 + did4 + did5 + factor(State) + factor(year), data = mydata)
vif(regs)

#export result to LATEX
texreg(reg, 
       omit.coef = "time|treated", digits=5, 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE, 
       custom.coef.names =c("Early Voting", "Preregistration", "Same/Election Day Registration", "Online Registration", "No-excuse Absentee Voting"),
       custom.note = "*With Fixed Effect: States and Year")
