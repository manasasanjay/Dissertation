library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)
library(mosaic)
library(magrittr)
library(lme4)
library(arm)


#first recode controls to be numeric, for some reason they're character
#also mean centre 
#start with ess1 to see which variables to include, and then subset ess1 and ess7 
#to include the same variables and rbind 

ess1_final <- ess1_final %>%
  mutate_at(vars(res_turn, avgeduyrs, sin_par_hh, unemp_rate), as.numeric)


#mean centre ethnic fractionalisation, avg education years, education years 
#of respondent, single parent hh, residential turnover, unemp rate

ess1_final$res_turn_mc <- scale(ess1_final$res_turn, scale = FALSE)
ess1_final$Eth_Frac_mc <- scale(ess1_final$Eth_Frac, scale = FALSE)
ess1_final$eduyrs_mc <- scale(ess1_final$eduyrs, scale = FALSE)
ess1_final$sin_par_hh_mc <- scale(ess1_final$sin_par_hh, scale = FALSE)
ess1_final$unemp_rate_mc <- scale(ess1_final$unemp_rate, scale = FALSE)


colnames(ess1_final)[593:597] <- paste(c("res_turn_mc", "Eth_Frac_mc", "eduyrs_mc", 
                                         "sin_par_hh_mc", "unemp_rate_mc"))

#exclude observations for poland from PL71 region (no census data)
ess1_final <- filter(ess1_final, !(reg_code == "PL71"))

#run a null model just regressing social trust on the regions to test intra-class
#correlation 

m0 <- lmer(soc_trst ~ (1|reg_code), data = ess1_final)
summary(m0)

#icc = 0.21 > 0.1 so MLM makes sense (potential for ecological fallacies)















