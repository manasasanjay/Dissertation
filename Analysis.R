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

#split dataset into countries

at_ess1_final <- filter(ess1_final, cntry == "AT")

cz_ess1_final <- filter(ess1_final, cntry == "CZ")

dk_ess1_final <- filter(ess1_final, cntry == "DK")

fr_ess1_final <- filter(ess1_final, cntry == "FR")

ie_ess1_final <- filter(ess1_final, cntry == "IE")

nl_ess1_final <- filter(ess1_final, cntry == "NL")

no_ess1_final <- filter(ess1_final, cntry == "NO")

pl_ess1_final <- filter(ess1_final, cntry == "PL")

pt_ess1_final <- filter(ess1_final, cntry == "PT")

es_ess1_final <- filter(ess1_final, cntry == "ES")

se_ess1_final <- filter(ess1_final, cntry == "SE")

ch_ess1_final <- filter(ess1_final, cntry == "CH")

#run model by country, first run null model to check ICC

m0_at <- lmer(soc_trst ~ (1|reg_code), data = at_ess1_final)
summary(m0_at)

m0_cz <- lmer(soc_trst ~ (1|reg_code), data = cz_ess1_final)
summary(m0_cz)

m0_dk <- lmer(soc_trst ~ (1|reg_code), data = dk_ess1_final)
summary(m0_dk)

m0_fr <- lmer(soc_trst ~ (1|reg_code), data = fr_ess1_final)
summary(m0_fr)

m0_ie <- lmer(soc_trst ~ (1|reg_code), data = ie_ess1_final)
summary(m0_ie)

m0_nl <- lmer(soc_trst ~ (1|reg_code), data = nl_ess1_final)
summary(m0_nl)

m0_no <- lmer(soc_trst ~ (1|reg_code), data = no_ess1_final)
summary(m0_no)

m0_pl <- lmer(soc_trst ~ (1|reg_code), data = pl_ess1_final)
summary(m0_pl)

m0_pt <- lmer(soc_trst ~ (1|reg_code), data = pt_ess1_final)
summary(m0_pt)

m0_es <- lmer(soc_trst ~ (1|reg_code), data = es_ess1_final)
summary(m0_es)

m0_se <- lmer(soc_trst ~ (1|reg_code), data = se_ess1_final)
summary(m0_se)

m0_ch <- lmer(soc_trst ~ (1|reg_code), data = ch_ess1_final)
summary(m0_ch)

m1_at <- lmer(soc_trst ~  Eth_Frac_mc + (1|reg_code), data = at_ess1_final)
summary(m1_at)











