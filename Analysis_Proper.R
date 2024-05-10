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
library(car)


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
ess1_final$avgeduyrs_mc <- scale(ess1_final$avgeduyrs, scale = FALSE)



colnames(ess1_final)[593:597] <- paste(c("res_turn_mc", "Eth_Frac_mc", "eduyrs_mc", 
                                         "sin_par_hh_mc", "unemp_rate_mc"))

#exclude observations for poland from PL71 region (no census data)
ess1_final <- filter(ess1_final, !(reg_code == "PL71"))

#run a null model just regressing social trust on the regions to test intra-class
#correlation 

m0 <- lmer(soc_trst ~ (1|reg_code), data = ess1_final)
summary(m0)

#icc = 0.21 > 0.1 so MLM makes sense (potential for ecological fallacies)

#run a model without any controls, just ethnic frac on social trust 
mbase <- lmer(soc_trst ~ Eth_Frac_mc + (1 + Eth_Frac_mc|reg_code), data = ess1_final)
summary(mbase)

coef(mbase)

#add controls:
#individual level: gender, age, education, income, employment status, cohabitaion, 
#crime victim, length of residence, institutional trust, life satisfaction, immigrant friends
#immigrant colleagues
#contextual: education, unemployment rate, single parent households, residential turnover. 

mcore <- lmer(soc_trst ~ Eth_Frac_mc + gndr + agea + eduyrs_mc + hinctnt + 
                empl + lvgptn + crmvct + yrlvdae + ins_trst + imgfrnd + imgclg + 
                stflife + avgeduyrs_mc + unemp_rate_mc + sin_par_hh_mc + 
                res_turn_mc + (1 + Eth_Frac_mc|reg_code), data = ess1_final)
summary(mcore)

coef(mcore)


mirt <- lmer(soc_trst ~ Eth_Frac_mc + gndr + agea + eduyrs_mc + hinctnt + 
               empl + lvgptn + crmvct + yrlvdae + ins_trst + imgfrnd + imgclg + 
               stflife + avgeduyrs_mc + unemp_rate_mc + sin_par_hh_mc + 
               res_turn_mc + IRTscores + (1 + Eth_Frac_mc + IRTscores|reg_code),
             data = ess1_final)
summary(mirt)
coef(mirt)


vif(mcore)

vif_values <- vif(mcore)

png(filename = "Dissertation GitHub/figures/vifplot.png")
barplot(vif_values, main = "VIF Values", horiz = TRUE, col = "steelblue")
dev.off()

range(ess1_final$IRTscores, na.rm = TRUE)

ess1_final$IRTscores_mc <- scale(ess1_final$IRTscores, scale = FALSE)

range(ess1_final$soc_trst, na.rm = TRUE)
ranef(mirt)
str(ranef(mirt))

ranefs_mirt <- as.data.frame(ranef(mirt)$reg_code[, 2])  # Extract the second column of random effects


colnames(ranefs_mirt) <- "Eth_Frac"  # Rename the column to "Eth_Frac"

ranefs_mirt$IRT <- ranef(mirt)$reg_code[, 3]

png(file = "Dissertation Github/figures/plot1.png", 
    width = 6000, height = 4000, res = 650)
ggplot(data = ranefs_mirt, mapping = aes(x = Eth_Frac, y = IRT)) +
  geom_point() + geom_smooth()
dev.off()

# Display the plot
print(plot1)





