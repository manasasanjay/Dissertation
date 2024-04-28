library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)
library(mosaic)

#load survey data 
ess1 <- read_dta("Desktop/PPE/DISS/ESS1e06_7/ESS1e06_7.dta")

#first split the data by country because it is the integrated file and you 
#cannot assign the codes until you split it 


#Austria 
ess1_ata <- ess1[ess1$cntry == "AT", ]

#Switzerland 
ess1_cha <- ess1[ess1$cntry == "CH", ]

#Czechia 
ess1_cza <- ess1[ess1$cntry == "CZ", ]

#Denmark 
ess1_dka <- ess1[ess1$cntry == "DK", ]

#spain 
ess1_esa <- ess1[ess1$cntry == "ES", ]

#France 
ess1_fra <- ess1[ess1$cntry == "FR", ]

#Hungary 
ess1_hua <- ess1[ess1$cntry == "HU", ]

#Ireland 
ess1_iea <- ess1[ess1$cntry == "IE", ]

#Netherlands 
ess1_nla <- ess1[ess1$cntry == "NL", ]

#Norway 
ess1_noa <- ess1[ess1$cntry == "NO", ]

#Poland
ess1_pla <- ess1[ess1$cntry == "PL", ]

#Portugal
ess1_pta <- ess1[ess1$cntry == "PT", ]

#Sweden 
ess1_sea <- ess1[ess1$cntry == "SE", ]


#Recode all the region variable values 

#Austria 
ess1_ata <- ess1_ata %>% 
  mutate(reg_code = case_when(
    regionat == 1 ~ "AT11", 
    regionat == 2 ~ "AT21",
    regionat == 3 ~ "AT12",
    regionat == 4 ~ "AT31",
    regionat == 5 ~ "AT32", 
    regionat == 6 ~ "AT22", 
    regionat == 7 ~ "AT33", 
    regionat == 8 ~ "AT34", 
    regionat == 9 ~ "AT13", 
    TRUE ~ NA))

#Switzerland 
ess1_cha <- ess1_cha %>%
  mutate(reg_code = case_when(
    regioach == 1 ~ "CH01", 
    regioach == 2 ~ "CH02", 
    regioach == 3 ~ "CH03", 
    regioach == 4 ~ "CH04", 
    regioach == 5 ~ "CH05", 
    regioach == 6 ~ "CH06", 
    regioach == 7 ~ "CH07", 
    TRUE ~ NA
  ))

#Czechia 
ess1_cza <- ess1_cza %>%
  mutate(reg_code = case_when(
    regioncz == 1 ~ "CZ01", 
    regioncz == 2 ~ "CZ02", 
    regioncz == 3 ~ "CZ03", 
    regioncz == 4 ~ "CZ03", 
    regioncz == 5 ~ "CZ04", 
    regioncz == 6 ~ "CZ04", 
    regioncz == 7 ~ "CZ05", 
    regioncz == 8 ~ "CZ05", 
    regioncz == 9 ~ "CZ05", 
    regioncz == 10 ~ "CZ06", 
    regioncz == 11 ~ "CZ06", 
    regioncz == 12 ~ "CZ07", 
    regioncz == 13 ~ "CZ07", 
    regioncz == 14 ~ "CZ08", 
    TRUE ~ NA
  ))

#Denmark 
ess1_dka <- ess1_dka %>%
  mutate(reg_code = case_when(
    regiondk == 1 ~ "DK01", 
    regiondk == 2 ~ "DK01", 
    regiondk == 3 ~ "DK01", 
    regiondk == 4 ~ "DK02", 
    regiondk == 5 ~ "DK02", 
    regiondk == 6 ~ "DK02", 
    regiondk == 7 ~ "DK01", 
    regiondk == 8 ~ "DK03", 
    regiondk == 9 ~ "DK03", 
    regiondk == 10 ~ "DK03", 
    regiondk == 11 ~ "DK03", 
    regiondk == 12 ~ "DK04", 
    regiondk == 13 ~ "DK04", 
    regiondk == 14 ~ "DK04", 
    regiondk == 15 ~ "DK05", 
    TRUE ~ NA
  ))

#Spain
ess1_esa <- ess1_esa %>% 
  mutate(reg_code = case_when(
    regiones == 11 ~ "ES11", 
    regiones == 12 ~ "ES12", 
    regiones == 13 ~ "ES13", 
    regiones == 21 ~ "ES21", 
    regiones == 22 ~ "ES22", 
    regiones == 23 ~ "ES23", 
    regiones == 24 ~ "ES24", 
    regiones == 30 ~ "ES30", 
    regiones == 41 ~ "ES41", 
    regiones == 42 ~ "ES42", 
    regiones == 43 ~ "ES43", 
    regiones == 51 ~ "ES51", 
    regiones == 52 ~ "ES52", 
    regiones == 53 ~ "ES53", 
    regiones == 61 ~ "ES61", 
    regiones == 62 ~ "ES62", 
    regiones == 63 ~ "ES63", 
    regiones == 70 ~ "ES70", 
    TRUE ~ NA
  ))

#France 
ess1_fra <- ess1_fra %>%
  mutate(reg_code = case_when(
    regionfr == 1 ~ "FR1", 
    regionfr == 2 ~ "FR2", 
    regionfr == 3 ~ "FR2", 
    regionfr == 4 ~ "FR3", 
    regionfr == 5 ~ "FR4", 
    regionfr == 6 ~ "FR5", 
    regionfr == 7 ~ "FR6", 
    regionfr == 8 ~ "FR7", 
    regionfr == 9 ~ "FR8", 
    TRUE ~ NA
  ))

#Hungary 
ess1_hua <- ess1_hua %>% 
  mutate(reg_code = case_when(
    regionhu == 1 ~ "HU10", 
    regionhu == 2 ~ "HU21", 
    regionhu == 3 ~ "HU22", 
    regionhu == 4 ~ "HU23", 
    regionhu == 5 ~ "HU31", 
    regionhu == 6 ~ "HU32", 
    regionhu == 7 ~ "HU33", 
    TRUE ~ NA
  ))

#Ireland 
ess1_iea <- ess1_iea %>% 
  mutate(reg_code = case_when(
    regionie == 1 ~ "IE01", 
    regionie == 2 ~ "IE01", 
    regionie == 3 ~ "IE01", 
    regionie == 4 ~ "IE02", 
    regionie == 5 ~ "IE02", 
    regionie == 6 ~ "IE02", 
    regionie == 7 ~ "IE02", 
    regionie == 8 ~ "IE02", 
    TRUE ~ NA
  ))

#Netherlands 
ess1_nla <- ess1_nla %>% 
  mutate(reg_code = case_when(
    regionnl == 111 ~ "NL11", 
    regionnl == 112 ~ "NL11", 
    regionnl == 113 ~ "NL11", 
    regionnl == 121 ~ "NL12", 
    regionnl == 122 ~ "NL12", 
    regionnl == 123 ~ "NL12", 
    regionnl == 131 ~ "NL13", 
    regionnl == 132 ~ "NL13", 
    regionnl == 133 ~ "NL13", 
    regionnl == 211 ~ "NL21", 
    regionnl == 212 ~ "NL21", 
    regionnl == 213 ~ "NL21", 
    regionnl == 221 ~ "NL22", 
    regionnl == 222 ~ "NL22", 
    regionnl == 223 ~ "NL22",
    regionnl == 224 ~ "NL22", 
    regionnl == 230 ~ "NL23", 
    regionnl == 310 ~ "NL31", 
    regionnl == 321 ~ "NL32", 
    regionnl == 322 ~ "NL32", 
    regionnl == 323 ~ "NL32", 
    regionnl == 324 ~ "NL32", 
    regionnl == 325 ~ "NL32", 
    regionnl == 326 ~ "NL32", 
    regionnl == 327 ~ "NL32", 
    regionnl == 331 ~ "NL33", 
    regionnl == 332 ~ "NL33", 
    regionnl == 333 ~ "NL33", 
    regionnl == 334 ~ "NL33", 
    regionnl == 335 ~ "NL33", 
    regionnl == 336 ~ "NL33", 
    regionnl == 341 ~ "NL34", 
    regionnl == 342 ~ "NL34", 
    regionnl == 411 ~ "NL41", 
    regionnl == 412 ~ "NL41", 
    regionnl == 413 ~ "NL41", 
    regionnl == 414 ~ "NL41", 
    regionnl == 421 ~ "NL42", 
    regionnl == 422 ~ "NL42", 
    regionnl == 423 ~ "NL42", 
    TRUE ~ NA
  ))

#Norway 
ess1_noa <- ess1_noa %>%
  mutate(reg_code = case_when(
    regionno == 1 ~ "NO01", 
    regionno == 2 ~ "NO02", 
    regionno == 3 ~ "NO03", 
    regionno == 4 ~ "NO04", 
    regionno == 5 ~ "NO05", 
    regionno == 6 ~ "NO06", 
    regionno == 7 ~ "NO07", 
    TRUE ~ NA
  ))

#Poland 
ess1_pla <- ess1_pla %>%
  mutate(reg_code = case_when(
    regionpl == 2 ~ "PL51", 
    regionpl == 4 ~ "PL61", 
    regionpl == 6 ~ "PL31", 
    regionpl == 8 ~ "PL43", 
    regionpl == 10 ~ "PL71", 
    regionpl == 12 ~ "PL21", 
    regionpl == 14 ~ "PL12", 
    regionpl == 16 ~ "PL52", 
    regionpl == 18 ~ "PL32", 
    regionpl == 20 ~ "PL34", 
    regionpl == 22 ~ "PL63", 
    regionpl == 24 ~ "PL22", 
    regionpl == 26 ~ "PL33", 
    regionpl == 28 ~ "PL62", 
    regionpl == 30 ~ "PL41", 
    regionpl == 32 ~ "PL42", 
    TRUE ~ NA
  ))

#Portugal 
ess1_pta <- ess1_pta %>% 
  mutate(reg_code = case_when(
    regionpt == 1 ~ "PT11", 
    regionpt == 2 ~ "PT16", 
    regionpt == 3 ~ "PT17", 
    regionpt == 4 ~ "PT18", 
    regionpt == 5 ~ "PT15", 
    TRUE ~ NA
  ))

#Sweden 
ess1_sea <-ess1_sea %>%
  mutate(reg_code = case_when(
    regionse == 1 ~ "SE11", 
    regionse == 2 ~ "SE12", 
    regionse == 3 ~ "SE22", 
    regionse == 4 ~ "SE31", 
    regionse == 5 ~ "SE32", 
    regionse == 6 ~ "SE33", 
    regionse == 7 ~ "SE21", 
    regionse == 8 ~ "SE23", 
    TRUE ~ NA
  ))

#rbind into one dataset 
ess1_a_full <- rbind(ess1_ata, ess1_cha, ess1_cza, ess1_dka, ess1_esa, ess1_fra, 
                     ess1_hua, ess1_iea, ess1_nla, ess1_noa, ess1_pla, ess1_pta, 
                     ess1_sea)

#subset to include only the variables you want 
ess1_a_subset <- ess1_a_full[, c("name", "essround", "edition", "proddate", 
                                 "idno", "cntry", "reg_code","dweight", "pspwght", 
                                 "pweight", "anweight", "regionat", "regioach", 
                                 "regioncz", "regiondk", "regiones", "regionfr", 
                                 "regionhu", "regionie", "regionnl", "regionno", 
                                 "regionpl", "regionpt", "regionse", "ppltrst", 
                                 "pplfair", "pplhlp", "lrscale", "stflife", 
                                 "stfeco", "dclmig", "aesfdrk", "ctzcntr", 
                                 "ctzship", "brncntr", "livecntr", "trstlgl", 
                                 "trstplc", "trstplt", "trstprl", "imsmetn", 
                                 "imdfetn", "qfimwht", "qfimcmt", "qfimlng", "imueclt", 
                                 "idetalv", "pplstrd", "imgfrnd", "imgclg", 
                                 "yrlvdae", "empl", "gndr", "agea", "domicil", 
                                 "eduyrs", "hinctnt", "lvgptn", "imptrad", 
                                 "crmvct", "imbgeco", "dscrgrp", "dscrrce", 
                                 "dscrntn", "dscrlng", "dscretn", "blgetmg")]

#recode class if necessary 
ess1_a_subset$ppltrst <- as.numeric(ess1_a_subset$ppltrst)
ess1_a_subset$pplfair <- as.numeric(ess1_a_subset$pplfair)
ess1_a_subset$pplhlp <- as.numeric(ess1_a_subset$pplhlp)
ess1_a_subset$lrscale <- as.numeric(ess1_a_subset$lrscale)
ess1_a_subset$stflife <- as.numeric(ess1_a_subset$stflife)
ess1_a_subset$stfeco <- as.numeric(ess1_a_subset$stfeco)
ess1_a_subset$dclmig <- as.character(ess1_a_subset$dclmig)
ess1_a_subset$aesfdrk <- as.character(ess1_a_subset$aesfdrk)
ess1_a_subset$ctzcntr <- as.numeric(ess1_a_subset$ctzcntr)
ess1_a_subset$ctzship <- as.character(ess1_a_subset$ctzship)
ess1_a_subset$brncntr <- as.character(ess1_a_subset$brncntr)
ess1_a_subset$livecntr <- as.character(ess1_a_subset$livecntr)
ess1_a_subset$trstlgl <- as.numeric(ess1_a_subset$trstlgl)
ess1_a_subset$trstplc <- as.numeric(ess1_a_subset$trstplc)
ess1_a_subset$trstplt <- as.numeric(ess1_a_subset$trstplt)
ess1_a_subset$trstprl <- as.numeric(ess1_a_subset$trstprl)
ess1_a_subset$imsmetn <- as.numeric(ess1_a_subset$imsmetn)
ess1_a_subset$imdfetn <- as.numeric(ess1_a_subset$imdfetn)
ess1_a_subset$qfimwht <- as.numeric(ess1_a_subset$qfimwht)
ess1_a_subset$qfimcmt <- as.numeric(ess1_a_subset$qfimcmt)
ess1_a_subset$qfimlng <- as.numeric(ess1_a_subset$qfimlng)
ess1_a_subset$imueclt <- as.numeric(ess1_a_subset$imueclt)
ess1_a_subset$idetalv <- as.character(ess1_a_subset$idetalv)
ess1_a_subset$pplstrd <- as.numeric(ess1_a_subset$pplstrd)
ess1_a_subset$imgfrnd <- as.character(ess1_a_subset$imgfrnd)
ess1_a_subset$imgclg <- as.character(ess1_a_subset$imgclg)
ess1_a_subset$yrlvdae <- as.numeric(ess1_a_subset$yrlvdae)
ess1_a_subset$empl <- as.character(ess1_a_subset$empl)
ess1_a_subset$gndr <- as.character(ess1_a_subset$gndr)
ess1_a_subset$agea <- as.numeric(ess1_a_subset$agea)
ess1_a_subset$domicil <- as.character(ess1_a_subset$domicil)
ess1_a_subset$eduyrs <- as.numeric(ess1_a_subset$eduyrs)
ess1_a_subset$hinctnt <- as.character(ess1_a_subset$hinctnt)
ess1_a_subset$lvgptn <- as.character(ess1_a_subset$lvgptn)
ess1_a_subset$imptrad <- as.numeric(ess1_a_subset$imptrad)
ess1_a_subset$crmvct <- as.character(ess1_a_subset$crmvct)
ess1_a_subset$imbgeco <- as.numeric(ess1_a_subset$imbgeco)
ess1_a_subset$dscrgrp <- as.character(ess1_a_subset$dscrgrp)
ess1_a_subset$dscrrce <- as.character(ess1_a_subset$dscrrce)
ess1_a_subset$dscrntn <- as.character(ess1_a_subset$dscrntn)
ess1_a_subset$dscrlng <- as.character(ess1_a_subset$dscrlng)
ess1_a_subset$dscretn <- as.character(ess1_a_subset$dscretn)
ess1_a_subset$blgetmg <- as.character(ess1_a_subset$blgetmg)

#create a social trust variable, a mean of ppltrst, pplfair, and pplhlp including
#only respondents who have responded to at least 2 out of the three questions 


ess1_a_subset$trst_resp_no <-  rowSums(!is.na(ess1_a_subset[, c("ppltrst", 
                                                                "pplfair", 
                                                                "pplhlp")]))

ess1_a_subset$soc_trst <- ifelse(ess1_a_subset$trst_resp_no >= 2, 
                                 rowMeans(ess1_a_subset[, c("ppltrst", 
                                                            "pplfair", 
                                                            "pplhlp")], 
                                          na.rm = TRUE), NA)

#create an institutional trust variable, a mean of trstlgl, trstplc, trstplt, 
#and trstprl including only respondents who have responded to at least 2 out of 
#the 4 questions 

ess1_a_subset$instrst_resp_no <-  rowSums(!is.na(ess1_a_subset[, c("trstlgl", 
                                                                   "trstplc", 
                                                                   "trstplt", 
                                                                   "trstprl")]))

ess1_a_subset$ins_trst <- ifelse(ess1_a_subset$instrst_resp_no >= 2, 
                                 rowMeans(ess1_a_subset[, c("trstlgl", 
                                                            "trstplc", 
                                                            "trstplt", 
                                                            "trstprl")], 
                                          na.rm = TRUE), NA)


#recode categorical variables 
#preferred decision level of immigration and refugee policies 

ess1_a_subset$dclmig <- recode(ess1_a_subset$dclmig, "1" = "International Level", 
                               "2" = "European Level", "3" = "National Level", 
                               "4" = "Regional or Local Level")

#feeling of safety of walking alone in local area after dark 
ess1_a_subset$aesfdrk <- recode(ess1_a_subset$aesfdrk, "1" = "Very safe", 
                             "2" = "Safe", "3" = "Unsafe", "4" = "Very unsafe")

#Citizen of reporting country (dummy variable)
ess1_a_subset$ctzcntr <- ifelse(ess1_a_subset$ctzcntr == 1, 1, 0)

#born in the country (dummy variable)
ess1_a_subset$brncntr <- ifelse(ess1_a_subset$brncntr == 1, 1, 0)

#How long ago first came to live in country 
ess1_a_subset$livecntr <- recode(ess1_a_subset$livecntr, "1" = "<1", "2" = "1-5", 
                                 "3" = "6-10", "4" = "11-20", "5" = ">20")

#People of minority race/ethnic group in ideal living area 
ess1_a_subset$idetalv <- recode(ess1_a_subset$idetalv, "1" = "Almost nobody", 
                                "2" = "Some", "3" = "Many", 
                                "4" = "It would make no difference")

#Immigrant Friends 
ess1_a_subset$imgfrnd <- recode(ess1_a_subset$imgfrnd, "1" = "Yes, several", 
                                "2" = "Yes, a few", "3" = "No, none at all")


#Immigrant colleagues 
ess1_a_subset$imgclg <- recode(ess1_a_subset$imgclg, "1" = "Yes, several", 
                               "2" = "Yes, a few", "3" = "No, none at all", 
                               "4" = "Not currently working")




#Employment status 
ess1_a_subset$empl <- recode(ess1_a_subset$empl, "1" = "Employed", 
                             "2" = "Self-Employed", "3" = "Not in paid work")
#recode to a dummy variable 
ess1_a_subset$empl <- ifelse(ess1_a_subset$empl == "Employed" | 
                               ess1_a_subset$empl == "Self-Employed", 1, 0)

#gender
ess1_a_subset$gndr <- recode(ess1_a_subset$gndr, "1" = "Male", "2" = "Female")

#respondent's description of their domicile 
ess1_a_subset$domicil <- recode(ess1_a_subset$domicil, "1" = "A big city", 
                                "2" = "Suburbs or outskirts of a big city", 
                                "3" = "Town or small city", 
                                "4" = "Country village", 
                                "5" = "Farm home in countryside")

#not sure how to work with income yet, come back to it. 

#currently living with a partner 
ess1_a_subset$lvgptn <- recode(ess1_a_subset$lvgptn, "1" = "Yes", "2" = "No")

#Respondent or hh member ever a victime of burglary/assault in the past 5 years 
ess1_a_subset$crmvct <- recode(ess1_a_subset$crmvct, "1" = "Yes", "2" = "No")

#recode respondent income: collapse into 5 bins as opposed to the current 12. 

ess1_a_subset <- ess1_a_subset %>%
  mutate(hinctnt = case_when(
    hinctnt == "1" ~ "0-10%", 
    hinctnt %in% c("2", "3", "4") ~ "11-20%", 
    hinctnt %in% c("5", "6") ~ "21-30%", 
    hinctnt %in% c("7", "8") ~ "31-40%", 
    hinctnt == 9 ~ "41-60%", 
    hinctnt == 10 ~ "61-80%", 
    hinctnt == 11 ~ "81-90%", 
    hinctnt == 12 ~ "91-100%", 
    TRUE ~ NA
  ))
  

#ess1_a_subset <- ess1_a_subset %>%
 # mutate(hinctnt = case_when(
    #hinctnt %in% c("1", "2", "3", "4", "5", "6") ~ "< €24k",
    #hinctnt %in% c("7", "8") ~ "€24k < inc < €36k",
    #hinctnt == "9" ~ "€36k < inc < €60k",
    #hinctnt == "10" ~ "€60k < inc < €90k",
    #hinctnt %in% c("11", "12") ~ "> €90k",
    #TRUE ~ NA  
 # ))

#member of a group discriminated against in this country 

ess1_a_subset$dscrgrp <- recode(ess1_a_subset$dscrgrp, "0" = "Not marked", 
                                "1" = "Marked")

#discrimination on the basis of colour or race 
ess1_a_subset$dscrrce <- recode(ess1_a_subset$dscrrce, "0" = "Not marked", 
                                "1" = "Marked")

#discrimination on the basis of nationality 
ess1_a_subset$dscrntn <- recode(ess1_a_subset$dscrntn, "0" = "Not marked", 
                                "1" = "Marked")

#discrimination on the basis of language
ess1_a_subset$dscrlng <- recode(ess1_a_subset$dscrlng, "0" = "Not marked", 
                                "1" = "Marked")


#discrimination on the basis of ethnicity 
ess1_a_subset$dscretn <- recode(ess1_a_subset$dscretn, "0" = "Not marked", 
                                "1" = "Marked")

#self-identification with belonging to minority ethnic group in country 
ess1_a_subset$blgetmg <- recode(ess1_a_subset$blgetmg, "1" = "Yes", 
                                "2" = "No")


#Method 1: standardise, run PCA
#standardise all the variables to be used in the PCA

ess1_PCA1_subset <- ess1_a_subset

#Allow many/few immigrants of same race/ethnic group as majority
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(imsmetn_std = scale(imsmetn))

#check mean & sd 
mean(ess1_PCA1_subset$imsmetn_std, na.rm = TRUE) #v close to 0
sd(ess1_PCA1_subset$imsmetn_std, na.rm = TRUE) #1 

#Allow many/few immigrants of different race/ethnic group from majority 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(imdfetn_std = scale(imdfetn))

#check mean & sd 
mean(ess1_PCA1_subset$imdfetn_std, na.rm = TRUE) #v close to 0
sd(ess1_PCA1_subset$imdfetn_std, na.rm = TRUE) #1 

#Qualification for immigration: be white 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(qfimwht_std = scale(qfimwht))

#check mean and sd 
mean(ess1_PCA1_subset$qfimwht_std, na.rm = TRUE) #v close to 0 
sd(ess1_PCA1_subset$qfimwht_std, na.rm = TRUE) #1

#Qualification for immigration: be committed to way of life in country 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(qfimcmt_std = scale(qfimcmt))

#check mean and sd 
mean(ess1_PCA1_subset$qfimcmt_std, na.rm = TRUE) #v close to 0 
sd(ess1_PCA1_subset$qfimcmt_std, na.rm = TRUE) #1

#Qualification for immigration: speak official language
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(qfimlng_std = scale(qfimlng))

#check mean and sd 
mean(ess1_PCA1_subset$qfimlng_std, na.rm = TRUE) #v close to 0 
sd(ess1_PCA1_subset$qfimlng_std, na.rm = TRUE) #1

#Country's cultural life undermined or enriched by immigrants 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(imueclt_std = scale(imueclt))

#check mean and sd 
mean(ess1_PCA1_subset$imueclt_std, na.rm = TRUE) #super close to 0 
sd(ess1_PCA1_subset$imueclt_std, na.rm = TRUE) #1

#Better for a country if everyone shares customs and traditions 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(pplstrd_std = scale(pplstrd))

#check mean and sd 
mean(ess1_PCA1_subset$pplstrd_std, na.rm = TRUE) #v close to 0 
sd(ess1_PCA1_subset$pplstrd_std, na.rm = TRUE) #1

#important to follow traditions and customs 
ess1_PCA1_subset <- ess1_PCA1_subset %>%
  mutate(imptrad_std = scale(imptrad))

#check mean and sd 
mean(ess1_PCA1_subset$imptrad_std, na.rm = TRUE) #v close to 0 
sd(ess1_PCA1_subset$imptrad_std, na.rm = TRUE) #1













