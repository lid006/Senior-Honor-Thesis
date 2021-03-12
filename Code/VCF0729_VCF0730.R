library(haven)
library(foreign)
library(dplyr)
library(tidyverse)
library(scales)

anes_timeseries_cdf <- read_sav("anes_timeseries_cdf_sav/anes_timeseries_cdf.sav")
anes_subset <-subset(anes_timeseries_cdf, select = c(VCF0101, VCF0729, VCF0730, VCF0703, VCF9151))
anes_subset <- na.omit(anes_subset)


anes_subset$ageGroup <- cut(anes_subset$VCF0101, breaks=c(18, 30, 50, 70, 100), right = FALSE, labels = c("Age 18-29","Age 30-49", "Age 50-69", "Age 70+"))

VCF0730 <- table(anes_subset$VCF0730, anes_subset$ageGroup)
VCF0730 <- rbind(VCF0730, Total = colSums(VCF0730))
VCF0730 <- rbind(VCF0730, Percent_correct = percent(VCF0730[2, ]/VCF0730[3, ]))
rownames(VCF0730) <- c("Answered Incorrectly or Don't Know or Missing",  "Answered correctly", "Total", "Percentage Correct")
#stargazer(VCF0730, type = "text", title="Respondents Answering Party with House Majority After the Election", digits=4, out="VCF0730.txt", style = "ajps")
VCF0730 <- xtable(VCF0730)
      custom.model.names ="Respondents Answering Party with House Majority After the Election")
VCF0729 <- table(anes_subset$VCF0729, anes_subset$ageGroup)
VCF0729 <- rbind(VCF0729, Total = colSums(VCF0729))
VCF0729 <- rbind(VCF0729, Percent_correct = percent(VCF0729[2, ]/VCF0729[3, ]))
rownames(VCF0729) <- c("Incorrect Answer/Don't Know/Missing",  "Correct Answer", "Total", "Percentage Correct")
VCF0729 <- xtable(VCF0729)

stargazer(VCF0729, type = "text", title="Respondents Answering Party with House Majority Before the Election", digits=4, out="VCF0729.txt", style = "ajps")




anes_subset <-subset(anes_timeseries_cdf, select = c(VCF0101, VCF0703, VCF9151))
anes_subset <- na.omit(anes_subset)
anes_subset$ageGroup <- cut(anes_subset$VCF0101, breaks=c(18, 30, 50, 70, 100), right = FALSE, labels = c("Age 18-29","Age 30-49", "Age 50-69", "Age 70 and 70+"))

library(dplyr)
library(tidyverse)

VCF0703 <- table(anes_subset$VCF0703, anes_subset$ageGroup)


VCF0703 <- rbind(VCF0703, Total = colSums(VCF0703))
VCF0703_1 <- rbind(VCF0703, Percent_vote = percent(VCF0703[3, ]/VCF0703[4, ]))
rownames(VCF0703_1) <- c("Not registered, and did not vote",  "Registered, but did not vote", "Voted (registered)", "Total", "Percentage Voted")
library(stargazer)
stargazer(VCF0703_1, type = "text", title="Respondents Register and Turnout SUMMARY", digits=4,out="VCF0703.txt", style = "ajps")



VCF0703 <- table(anes_subset$VCF0703, anes_subset$ageGroup)

VCF9151 <- table(anes_subset$VCF9151, anes_subset$ageGroup)
rownames(VCF9151) <- c("Sef-reported Voted",  "Self-reported Did not vote")
 
VCF0703_VCF9151 <- rbind(VCF0703, VCF9151)
VCF0703_VCF9151 <- rbind(VCF0703_VCF9151, Total = colSums(VCF0703_VCF9151)/2)
VCF0703_VCF9151 <- rbind(VCF0703_VCF9151, Percent_vote = percent(VCF0703_VCF9151[3, ]/VCF0703_VCF9151[6, ]))
VCF0703_VCF9151 <- rbind(VCF0703_VCF9151, diff_vote = percent(VCF0703_VCF9151[4, ]-VCF0703_VCF9151[3, ]))

rownames(VCF0703_VCF9151) <- c("Not registered, and did not vote",  "Registered, but did not vote", "Voted (registered)", "Sef-reported Voted",  "Self-reported Did not vote", "Total", "Percentage Voted", "Difference Between Self-Reported and Actural Turnouts")


stargazer(VCF0703_VCF9151, type = "text", title="Respondents Self-reported and Actual Register and Turnout SUMMARY", digits=4, out="VCF0703_VCF9151.txt", style = "ajps")








VCF0703 <- rbind(VCF0703, Total = colSums(VCF0703))
VCF0703 <- rbind(VCF0703, Percent_vote = percent(VCF0703[3, ]/VCF0703[4, ]))
rownames(VCF0703) <- c("Not registered, and did not vote",  "Registered, but did not vote", "Voted (registered)", "Total", "Percentage Voted")
library(stargazer)
stargazer(VCF0703, type = "text", title="Respondents Register and Turnout SUMMARY", digits=4,out="VCF0703.txt", style = "ajps")



