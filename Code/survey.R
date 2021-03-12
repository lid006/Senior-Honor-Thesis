library(ggplot2)
library(dplyr)
library(xtable)
library(CGPfunctions)
library(reshape)
library(janitor)
library(plyr)
library(schoRsch)
library(xtable)
library(readxl)
library(tidyverse)
library(lfe)
library(texreg)
library(apa)
library(magrittr)        
library(grid)
library(gridExtra)    

dat <- read.csv("survey.csv")
dat <- as.data.frame(dat)

# Data Processing
datCB <- subset(dat, select=c(group, Group, SCO, FB, QID17, Q98))
datCB <- datCB[-2, ]
datCB <- datCB[-1, ]

# Define threshold for high/low Education/Political Knowledge/Match of Candidate
datCB <- datCB %>%
  mutate(Education = (QID17>4)) # education > college, 1, or else 0
datCB <- datCB %>%
  mutate(Score = (SCO>6)) # political knowledge quiz score > 60%, 1, or else 0
datCB <- datCB %>%
  mutate(Match = (Q98>7)) # 80% likelihood to vote for recommended candidate
datCB$Education[datCB$Education == TRUE] <-"High"
datCB$Education[datCB$Education == FALSE] <-"Low"
datCB$Score[datCB$Score == TRUE] <-"High"
datCB$Score[datCB$Score == FALSE] <-"Low"
datCB$Match[datCB$Match == TRUE] <-"High"
datCB$Match[datCB$Match == FALSE] <-"Low"

# Subsetting Data for T-test and KS-test
datControl  = subset(datCB, Group == "control")
datom = subset(datCB, Q98 != "")
datomEx = subset(datom, Group == "experimental")
datEducLow = subset(datom, Education =="Low")
datEducHigh = subset(datom, Education =="High")
datScoreLow = subset(datom, Score =="Low")
datScoreHigh = subset(datom, Score =="High")
datEdLowControl = subset(datControl, Education == "Low")
datEdhighControl = subset(datControl, Education == "High")
datSCLowControl = subset(datControl, Score == "Low")
datSChighControl = subset(datControl, Score == "High")
datEdLowEx = subset(datomEx, Education == "Low")
datEdhighEx = subset(datomEx, Education == "High")
datSCLowEx = subset(datomEx, Score == "Low")
datSCHighEx = subset(datomEx, Score == "High")
datlomat = subset(datomEx, Match == "Low")
dathimat = subset(datomEx, Match == "High")
dathimathied = subset(dathimat, Education == "High")
dathimatlowed = subset(dathimat, Education == "Low")
dathimathisc = subset(dathimat, Score == "High")
dathimatlosc = subset(dathimat, Score == "Low")

datlomathied = subset(datlomat, Education == "High")
datlomatlowed = subset(datlomat, Education == "Low")
datlomathisc = subset(datlomat, Score == "High")
datlomatlosc = subset(datlomat, Score == "Low")

# T-tests and KS tests
# T-test between Control and Experimental Groups on Likelihood to Vote
# reported in LATEX format
t_apa(t_test((datControl$FB),datomEx$FB), format = "latex", es_ci = TRUE)

# KS-test between Control and Experimental Groups on Likelihood to Vote
ks.test(datlomat$FB, dathimat$FB, alternative = "two.sided")

# T-test between Respondents with Low Level of Education/Political Knowledge in Control and Experimental Groups on Likelihood to Vote
# Unpaired two-samples t-test (Welch)
t_apa(t_test(datEdLowControl$FB, datEdLowEx$FB), format = "latex", es_ci = TRUE)
t_apa(t_test(datSCLowControl$FB, datSCLowEx$FB), format = "latex", es_ci = TRUE)

# KS Test between Respondents with Low Level of Education/Political Knowledge in Control and Experimental Groups on Likelihood to Vote
ks.test(datEdLowControl$FB, datEdLow$FB, var.equal = FALSE)
ks.test(datSC$FB, datSCLowControl$FB, var.equal = FALSE)

# T-test between Respondents with High Likelihood Voting for the Recommended Candidate but Different Level of Education/Political Knowledge on Likelihood to Vote
t_apa(t_test(dathimatlowed$FB, dathimathied$FB), format = "latex", es_ci = TRUE)
t_apa(t_test(dathimatlosc$FB, dathimathisc$FB), format = "latex", es_ci = TRUE)

# KS test between Respondents with High Likelihood Voting for the Recommended Candidate but Different Level of Education/Political Knowledge on Likelihood to Vote
ks.test(dathimatlowed$FB, dathimathied$FB, alternative = "two.sided")
ks.test(dathimatlosc$FB, dathimathisc$FB, alternative = "two.sided")

# Regressions
# Linear Models
p1 = lm(datom$FB~ datom$QID17)
summary(p1)

p2 = lm(datom$FB~ datom$Score)
summary(p2)

r3 = lm(as.numeric(datom$SCO) ~ as.numeric(datom$QID17))
summary(r3)

p5 = lm(datomEx$FB~ datomEx$Match)
summary(p5)

# reporting regression results in LATEX
texreg(list(p1), 
       digits=1, 
       custom.coef.names = c("Intercept", "Education Coefficient"), 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE,
       custom.model.names = "Effect of Level of Education on Likelihood to Vote after Survey")

texreg(list(p2), 
       digits=1, 
       custom.coef.names = c("Intercept", "Political Knowledge Coefficient"), 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE,
       custom.model.names = "Effect of Level of Political Knowledge on Likelihood to Vote after Survey")

texreg(list(p5), 
       digits=1, 
       custom.coef.names = c("Intercept", "Candidate Match Coefficient"), 
       include.rsquared = TRUE, include.adjrs = FALSE, include.rmse = TRUE,
       custom.model.names = "Effect of Candidate Matching on Likelihood to Vote after Survey")

# Visualizations

# Likelihood to Vote after Taking the Survey Across All Groups of Respondents
p1 <- ggplot(datom, aes(FB)) + 
  geom_histogram(binwidth=1, fill = "white", color = "grey30")+ scale_x_continuous(breaks=c(1,2,3,4,5,6,7,8,9,10)) 
p1 + xlab("Likelihood to Vote after Taking the Survey") + ylab("Number of Respondents")

# Density Plot
mu <- ddply(datom, "Group", summarise, grp.mean=mean(FB))

p2 <- ggplot(datom, aes(x=FB, color = Group)) + 
  geom_density()+geom_vline(data= mu, aes(xintercept=grp.mean, color=Group),
                           linetype="dashed")
p2 + labs( x = "Likelihood to vote After Taking the Survey", y = "Density")
 
# Boxplots for the Effect of Level of Education/Political Knowledge on Likelihood to Vote 
# after Taking the Survey, by Likelihood to Vote for Recommended Candidate
dathimathied = subset(dathimat, Education == "High")
dathimatlowed = subset(dathimat, Education == "Low")

p3 <- ggplot(dathimat, aes(x=Education, y=FB, fill=Education))+
  geom_boxplot()+ ggtitle("High Likelihood to Vote for Recommended Candidate")
p3 = p3 + theme(legend.position = "none") + ylab("Likelihood to Vote after Taking the Survey")+ xlab("Level of Education")
p4 <- ggplot(datlomat, aes(x=Education, y=FB, fill=Education))+
  geom_boxplot()+ ggtitle("Low Likelihood to Vote for Recommended Candidate")
p4 = p4 + theme(legend.position = "bottom")+ ylab("Likelihood to Vote after Taking the Survey") + xlab("Level of Education") 
grid.arrange(p3, p4, ncol = 2) #display side by side

p5 <- ggplot(dathimat, aes(x=Score, y=FB, fill=Score))+
  geom_boxplot()+ ggtitle("High Likelihood to Vote for Recommended Candidate")
p5 = p5 + theme(legend.position = "none") +ylab("Likelihood to Vote after Taking the Survey")+ xlab("Level of Political Knowledge")
p6 <- ggplot(datlomat, aes(x=Score, y=FB, fill=Score))+
  geom_boxplot()+ ggtitle("Low Likelihood to Vote for Recommended Candidate")
p6 = p6+ theme(legend.position = "bottom")+ labs(fill = "Level of Political Knowledge") + ylab("Likelihood to Vote after Taking the Survey") + xlab("Level of Political Knowledge") 
grid.arrange(p5, p6, ncol = 2)
 