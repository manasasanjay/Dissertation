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

m0 <- lmer(soc_trst ~ (1|reg_code), data = ess1_final)
summary(m0)

#icc 0.2, makes sense to do a multilevel model. 

m1 <- lmer(soc_trst ~ Eth_Frac + gndr + eduyrs + ins_trst + agea + lvgptn + 
             crmvct + dvrcdev + empl + yrlvdae + stflife + avgeduyrs + res_turn + 
             sin_par_hh +  as.factor(cntry) + unemp_rate + IRTscores
           + (1|reg_code), data = ess1_final)

summary(m1)


coef(m1)$reg_code[1, 2]/se.coef()

class(ess1_final$avgeduyrs)
ess1_final$avgeduyrs <- as.numeric(ess1_final$avgeduyrs)
ess1_final$res_turn <- as.numeric(ess1_final$res_turn)
ess1_final$sin_par_hh <- as.numeric(ess1_final$sin_par_hh)
ess1_final$unemp_rate <- as.numeric(ess1_final$unemp_rate)
