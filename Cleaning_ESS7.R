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

#load survey data 
ess7 <- read_dta("Desktop/PPE/DISS/ESS7e02_3/ESS7e02_3.dta")


unique(ess7$region)

#first split the data by country because it is the integrated file and you 
#cannot assign the codes until you split it 

ess7_ata <- ess7[ess7$cntry == "AT", ]

ess7_cha <- ess7[ess7$cntry == "CH", ]

ess7_cza <- ess7[ess7$cntry == "CZ", ]

ess7_dka <- ess7[ess7$cntry == "DK", ]

ess7_esa <- ess7[ess7$cntry == "ES", ]

ess7_fra <- ess7[ess7$cntry == "FR", ]

ess7_iea <- ess7[ess7$cntry == "IE", ]

ess7_nla <- ess7[ess7$cntry == "NL", ]

ess7_noa <- ess7[ess7$cntry == "NO", ]  

ess7_pla <- ess7[ess7$cntry == "PL", ]  

ess7_pta <- ess7[ess7$cntry == "PT", ]  

ess7_sea <- ess7[ess7$cntry == "SE", ]  

#Recode all the region variable values 

#austria
unique(ess7_ata$region) #leave austria as is, already in the format we need

#switzerland 
unique(ess7_cha$region) #leave switzerland as well

#czechia 
unique(ess7_cza$region) #need to recode czechia to nuts 2 

ess7_cza <- ess7_cza %>% 
  mutate(region = case_when(
    region == "CZ010" ~ "CZ01", 
    region == "CZ020" ~ "CZ02", 
    region == "CZ031" ~ "CZ03", 
    region == "CZ032" ~ "CZ03", 
    region == "CZ041" ~ "CZ04", 
    region == "CZ042" ~ "CZ04", 
    region == "CZ051" ~ "CZ05", 
    region == "CZ052" ~ "CZ05", 
    region == "CZ053" ~ "CZ05", 
    region == "CZ063" ~ "CZ06", 
    region == "CZ064" ~ "CZ06", 
    region == "CZ071" ~ "CZ07", 
    region == "CZ072" ~ "CZ07", 
    region == "CZ080" ~ "CZ08", 
    TRUE ~ NA
   ))

#denmark 
unique(ess7_dka$region) #leave as is already fine 

#spain 
unique(ess7_esa$region) #recode ES63 and ES64 ES63, as ceuta and melilla are coded 
#together in the first wave 

ess7_esa$region <- ifelse(ess7_esa$region == "ES64", "ES63", ess7_esa$region)

#france 
unique(ess7_fra$region)
#need to recode to nuts1 because first wave is in nuts1

ess7_fra <- ess7_fra %>%
  mutate(region = case_when(
    region == "FR10" ~ "FR1", 
    region == "FR21" ~ "FR2", 
    region == "FR22" ~ "FR2", 
    region == "FR23" ~ "FR2", 
    region == "FR24" ~ "FR2", 
    region == "FR25" ~ "FR2", 
    region == "FR26" ~ "FR2", 
    region == "FR30" ~ "FR3", 
    region == "FR41" ~ "FR4", 
    region == "FR42" ~ "FR4", 
    region == "FR43" ~ "FR4", 
    region == "FR51" ~ "FR5", 
    region == "FR52" ~ "FR5", 
    region == "FR53" ~ "FR5", 
    region == "FR61" ~ "FR6", 
    region == "FR62" ~ "FR6", 
    region == "FR63" ~ "FR6", 
    region == "FR71" ~ "FR7", 
    region == "FR72" ~ "FR7", 
    region == "FR81" ~ "FR8", 
    region == "FR82" ~ "FR8", 
    TRUE ~ NA
  ))

#ireland 
unique(ess7_iea$region) #recode to nuts 2 

ess7_iea <- ess7_iea %>%
  mutate(region = case_when(
    region == "IE011" ~ "IE01", 
    region == "IE013" ~ "IE01", 
    region == "IE022" ~ "IE02", 
    region == "IE012" ~ "IE01", 
    region == "IE021" ~ "IE02", 
    region == "IE023" ~ "IE02", 
    region == "IE024" ~ "IE02", 
    region == "IE025" ~ "IE02", 
    TRUE ~ NA
  ))

#netherlands 
unique(ess7_nla$region) #leave as is 

#norway 
unique(ess7_noa$region) #leave as is 

#poland 
unique(ess7_pla$region) #leave as is 

#portugal 
unique(ess7_pta$region) #leave as is 

#sweden 
unique(ess7_sea$region) #recode to nuts 2 

ess7_sea <- ess7_sea %>%
  mutate(region = case_when(
    region == "SE224" ~ "SE22", 
    region == "SE231" ~ "SE23", 
    region == "SE321" ~ "SE32", 
    region == "SE110" ~ "SE11", 
    region == "SE312" ~ "SE31", 
    region == "SE232" ~ "SE23", 
    region == "SE121" ~ "SE12", 
    region == "SE125" ~ "SE12", 
    region == "SE212" ~ "SE21", 
    region == "SE123" ~ "SE12", 
    region == "SE213" ~ "SE21", 
    region == "SE331" ~ "SE33", 
    region == "SE311" ~ "SE31", 
    region == "SE124" ~ "SE12", 
    region == "SE122" ~ "SE12", 
    region == "SE211" ~ "SE21", 
    region == "SE313" ~ "SE31", 
    region == "SE221" ~ "SE22", 
    region == "SE214" ~ "SE21", 
    region == "SE322" ~ "SE32", 
    region == "SE332" ~ "SE33", 
    TRUE ~ NA
  ))

#rbind into one dataset 
ess7_a_full <- rbind(ess7_ata, ess7_cha, ess7_cza, ess7_dka, ess7_esa, 
                     ess7_fra, ess7_iea, ess7_nla, ess7_noa, ess7_pla, 
                     ess7_pta, ess7_sea)
ess7_a_full$reg_code <- ess7_a_full$region

ess7_a_subset <- ess7_a_full

#subset to include only the variables that we want 
#ess7_a_subset <- ess7_a_full[, c("name", "essround", "edition", "proddate", 
                                 #"idno", "cntry", "dweight", "pspwght", 
                                 #"pweight", "anweight", "region", "ppltrst", 
                                 #"pplfair", "pplhlp", "trstlgl", "trstplc", 
                                 #"trstplt", "trstprl", "lrscale", "stflife", 
                                 #"stfeco", "imsmetn", "imdfetn", "imbgeco", 
                                 #"imueclt", "crmvct", "aesfdrk", "dscrgrp", 
                                 #"dscrrce", "dscrntn", "dscrlng", "dscretn", 
                                 #"ctzcntr", "brncntr", "blgetmg", "qfimlng", 
                                 #"qfimwht", "qfimcmt", "pplstrd", "fclcntr", 
                                 #"dfegcf", "dfegcon", "dfeghbg", "smctmbe", 
                                # "gndr", "agea", "domicil", "eduyrs", "hinctnta", 
                                # "imptrad", "pdwrk", "uempla", "uempli", 
                                # "icpart2")]

#recode class 

ess7_a_subset$ppltrst <- as.numeric(ess7_a_subset$ppltrst)
ess7_a_subset$pplfair <- as.numeric(ess7_a_subset$pplfair)
ess7_a_subset$pplhlp <- as.numeric(ess7_a_subset$pplhlp)
ess7_a_subset$trstlgl <- as.numeric(ess7_a_subset$trstlgl)
ess7_a_subset$trstplc <- as.numeric(ess7_a_subset$trstplc)
ess7_a_subset$trstplt <- as.numeric(ess7_a_subset$trstplt)
ess7_a_subset$trstprl <- as.numeric(ess7_a_subset$trstprl)
ess7_a_subset$lrscale <- as.numeric(ess7_a_subset$lrscale)
ess7_a_subset$stflife <- as.numeric(ess7_a_subset$stflife)
ess7_a_subset$stfeco <- as.numeric(ess7_a_subset$stfeco)
ess7_a_subset$imsmetn <- as.numeric(ess7_a_subset$imsmetn)
ess7_a_subset$imdfetn <- as.numeric(ess7_a_subset$imdfetn)
ess7_a_subset$imbgeco <- as.numeric(ess7_a_subset$imbgeco)
ess7_a_subset$imueclt <- as.numeric(ess7_a_subset$imueclt)
ess7_a_subset$crmvct <- as.character(ess7_a_subset$crmvct)
ess7_a_subset$aesfdrk <- as.character(ess7_a_subset$aesfdrk)
ess7_a_subset$dscrgrp <- as.character(ess7_a_subset$dscrgrp)
ess7_a_subset$dscrrce <- as.character(ess7_a_subset$dscrrce)
ess7_a_subset$dscrntn <- as.character(ess7_a_subset$dscrntn)
ess7_a_subset$dscrlng <- as.character(ess7_a_subset$dscrlng)
ess7_a_subset$dscretn <- as.character(ess7_a_subset$dscretn)
ess7_a_subset$ctzcntr <- as.character(ess7_a_subset$ctzcntr)
ess7_a_subset$brncntr <- as.character(ess7_a_subset$brncntr)
ess7_a_subset$blgetmg <- as.character(ess7_a_subset$blgetmg)
ess7_a_subset$qfimlng <- as.numeric(ess7_a_subset$qfimlng)
ess7_a_subset$qfimwht <- as.numeric(ess7_a_subset$qfimwht)
ess7_a_subset$qfimcmt <- as.numeric(ess7_a_subset$qfimcmt)
ess7_a_subset$pplstrd <- as.numeric(ess7_a_subset$pplstrd)
ess7_a_subset$fclcntr <- as.numeric(ess7_a_subset$fclcntr)
ess7_a_subset$dfegcf <- as.character(ess7_a_subset$dfegcf)
ess7_a_subset$dfegcon <- as.character(ess7_a_subset$dfegcon)
ess7_a_subset$dfeghbg <- as.numeric(ess7_a_subset$dfeghbg)
ess7_a_subset$smctmbe <- as.character(ess7_a_subset$smctmbe)
ess7_a_subset$gndr <- as.character(ess7_a_subset$gndr)
ess7_a_subset$domicil <- as.character(ess7_a_subset$domicil)
ess7_a_subset$hinctnta <- as.character(ess7_a_subset$hinctnta)
ess7_a_subset$imptrad <- as.numeric(ess7_a_subset$imptrad)
ess7_a_subset$pdwrk <- as.character(ess7_a_subset$pdwrk)
ess7_a_subset$uempla <- as.character(ess7_a_subset$uempla)
ess7_a_subset$uempli <- as.character(ess7_a_subset$uempli)
ess7_a_subset$icpart2 <- as.character(ess7_a_subset$icpart2)
ess7_a_subset$imwbcnt <- as.numeric(ess7_a_subset$imwbcnt)
ess7_a_subset$imwbcrm <- as.numeric(ess7_a_subset$imwbcrm)
ess7_a_subset$rlgdgr <- as.numeric(ess7_a_subset$rlgdgr)
ess7_a_subset$qfimchr <- as.numeric(ess7_a_subset$qfimchr)
ess7_a_subset$imtcjob <- as.numeric(ess7_a_subset$imtcjob)
ess7_a_subset$imbleco <- as.numeric(ess7_a_subset$imbleco)
ess7_a_subset$imdetbs <- as.numeric(ess7_a_subset$imdetbs)
ess7_a_subset$imdetmr <- as.numeric(ess7_a_subset$imdetmr)
ess7_a_subset$gvtrimg <- as.numeric(ess7_a_subset$gvtrimg)
ess7_a_subset$rlgueim <- as.numeric(ess7_a_subset$rlgueim)
ess7_a_subset$uemp5yr <- as.numeric(ess7_a_subset$uemp5yr)
ess7_a_subset$ipudrst <- as.numeric(ess7_a_subset$ipudrst)
ess7_a_subset$dvrcdeva <- as.numeric(ess7_a_subset$dvrcdeva)

#create a social trust variable, a mean of ppltrst, pplfair, and pplhlp including
#only respondents who have responded to at least 2 out of the three questions 


ess7_a_subset$trst_resp_no <-  rowSums(!is.na(ess7_a_subset[, c("ppltrst", 
                                                                "pplfair", 
                                                                "pplhlp")]))

ess7_a_subset$soc_trst <- ifelse(ess7_a_subset$trst_resp_no >= 2, 
                                 rowMeans(ess7_a_subset[, c("ppltrst", 
                                                            "pplfair", 
                                                            "pplhlp")], 
                                          na.rm = TRUE), NA)
#create an institutional trust variable, a mean of trstlgl, trstplc, trstplt, 
#and trstprl including only respondents who have responded to at least 2 out of 
#the 4 questions 

ess7_a_subset$instrst_resp_no <-  rowSums(!is.na(ess7_a_subset[, c("trstlgl", 
                                                                   "trstplc", 
                                                                   "trstplt", 
                                                                   "trstprl")]))

ess7_a_subset$ins_trst <- ifelse(ess7_a_subset$instrst_resp_no >= 2, 
                                 rowMeans(ess7_a_subset[, c("trstlgl", 
                                                            "trstplc", 
                                                            "trstplt", 
                                                            "trstprl")], 
                                          na.rm = TRUE), NA)

#recode categorical variables 


#respondent or household member victim of burglary/assault in the last 5 years
ess7_a_subset$crmvct <- recode(ess7_a_subset$crmvct, "1" = "Yes", 
                               "2" = "No")

#Feeling of safety walking alone in local area after dark 
ess7_a_subset$aesfdrk <- recode(ess7_a_subset$aesfdrk, "1" = "Very safe", 
                                "2" = "Safe", "3" = "Unsafe", 
                                "4" = "Very unsafe")

#Member of group discriminated against in this country (individual's response)
ess7_a_subset$dscrgrp <- recode(ess7_a_subset$dscrgrp, "1" = "Yes", 
                                "2" = "No")

#Discrimination of respondent's group: colour or race
ess7_a_subset$dscrrce <- recode(ess7_a_subset$dscrrce, "0" = "Not marked", 
                                "1" = "Marked")

#Discrimination of respondent's group: nationality 
ess7_a_subset$dscrntn <- recode(ess7_a_subset$dscrntn, "0" = "Not marked", 
                                "1" = "Marked")

#Discrimination of respondent's group: language 
ess7_a_subset$dscrlng <- recode(ess7_a_subset$dscrlng, "0" = "Not marked", 
                                "1" = "Marked")

#Discrimination of respondent's group: ethnicity 
ess7_a_subset$dscretn <- recode(ess7_a_subset$dscretn, "0" = "Not marked", 
                                "1" = "Marked")

#citizen of country 
ess7_a_subset$ctzcntr <- recode(ess7_a_subset$ctzcntr, "1" = "Yes", 
                                "2" = "No")

#born in the country 
ess7_a_subset$brncntr <- recode(ess7_a_subset$brncntr, "1" = "Yes", 
                                "2" = "No")

#belonging to minority ethnic group in country 
ess7_a_subset$blgetmg <- recode(ess7_a_subset$blgetmg, "1" = "Yes", 
                                "2" = "No")

#Different race or ethnic group: have any close friends 
ess7_a_subset$dfegcf <- recode(ess7_a_subset$dfegcf, "1" = "Yes, several", 
                               "2" = "Yes, a few", "3" = "No, none at all")

#different race or ethnic group: contact, how often 
ess7_a_subset$dfegcon <- recode(ess7_a_subset$dfegcon, "1" = "Never", 
                                "2" = "Less than once a month", 
                                "3" = "Once a month", 
                                "4" = "Several times a month", 
                                "5" = "Once a week", 
                                "6" = "Several times a week", 
                                "7" = "Every day")

#some cultures: much better or all equal 
ess7_a_subset$smctmbe <- recode(ess7_a_subset$smctmbe, 
                                "1" = "Some cultures are much better than others", 
                                "2" = "All cultures are equal")

#gender 
ess7_a_subset$gndr <- recode(ess7_a_subset$gndr, "1" = "Male", 
                             "2" = "Female")

#domicile (respondent's description)
ess7_a_subset$domicil <- recode(ess7_a_subset$domicil, "1" = "A big city", 
                                "2" = "Suburbs or outskirts of a big city", 
                                "3" = "Town or small city", 
                                "4" = "Country village", 
                                "5" = "Farm or home in countryside")

#household's total net income, all sources 
ess7_a_subset <- ess7_a_subset %>%
  mutate(hinctnta = case_when(
    hinctnta == "1" ~ "0-10%", 
    hinctnta == "2" ~ "11-20%", 
    hinctnta == "3" ~ "21-30%", 
    hinctnta == "4" ~ "31-40%", 
    hinctnta == "5" ~ "41-50%", 
    hinctnta == "6" ~ "51-60%",
    hinctnta == "7" ~ "61-70%", 
    hinctnta == "8" ~ "71-80%", 
    hinctnta == "9" ~ "81-90%", 
    hinctnta == "10" ~ "91-100%", 
    TRUE ~ NA
  ))

#create a variable for employment. if pdwrk == 1, then employed, if uempla == 1 
#or uempli == 1, unemployed 

ess7_a_subset$empl <- ifelse(ess7_a_subset$pdwrk == "0" | 
                               ess7_a_subset$uempla == "1" | 
                               ess7_a_subset$uempli == "1", "Unemployed", 
                             ifelse(ess7_a_subset$pdwrk == "1", "Employed", 
                                    NA))


ess7_a_subset$icpart2 <- recode(ess7_a_subset$icpart2, "1" = "Yes", 
                                "2" = "No")

#subset for IRT
ess7_IRT1_subset <- ess7_a_subset[complete.cases(ess7_a_subset[,
                     c("qfimwht", "qfimcmt",
                     "qfimlng", "pplstrd"
                      )]),]

range(rowSums(ess7_IRT1_subset[, c("qfimwht", 
                                   "qfimcmt",
                                   "qfimlng", "pplstrd")]))

hist(rowSums(ess7_IRT1_subset[, c("qfimwht", 
                                  "qfimcmt",
                                  "qfimlng", "pplstrd")]), xlab="IDK",main="",
     br=seq(1,35,1), freq=FALSE)

#Trying ordinal IRT
grm_fit7 <- grm(ess7_IRT1_subset[, c("qfimwht", 
                                    "qfimcmt",
                                    "qfimlng", "pplstrd")])
grm_fit7
par(mfrow=c(2,2))
plot(grm_fit7)

grm_scores7 <- factor.scores.grm(grm_fit7,resp.patterns=ess7_IRT1_subset[, 
              c("qfimwht", 
               "qfimcmt",
               "qfimlng", "pplstrd")])

out7 <- data.frame(scores7 = grm_scores7$score.dat$z1,cumulative_response = 
                     rowSums(ess7_IRT1_subset[, 
                                              c("qfimwht", 
                                                "qfimcmt",
                                                "qfimlng", "pplstrd")]), 
                   idno = ess7_IRT1_subset$idno)

range(out7$scores7)

# ggplot2
ggplot(out7, aes(x=jitter(cumulative_response),y=scores7)) +
  geom_point(size=2,alpha=.5) +
  scale_x_continuous("Correct Responses",breaks = seq(1,39,2)) + 
  scale_y_continuous("Ordered IRT Score",breaks = seq(-3,3,0.5)) +
  theme_clean() +
  theme(plot.background = element_rect(color=NA))


ess7_b <- ess7_IRT1_subset
ess7_b$IRTscores <- grm_scores7$score.dat$z1
ess7_b$cumulative_response <- rowSums(ess7_IRT1_subset[, 
                              c("qfimwht", 
                              "qfimcmt",
                              "qfimlng", "pplstrd")])

#merge with census data
ess7_c <- merge(ess7_b, census_full_11, by = "reg_code", all.x = TRUE)

ess7_d <- merge(ess7_c, census_2011_controls, by = "reg_code", all.x = TRUE)

ess7_final <- ess7_d

