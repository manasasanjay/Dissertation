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

#subset to include only the variables that we want 
ess7_a_subset <- ess7_a_full[, c("name", "essround", "edition", "proddate", 
                                 "idno", "cntry", "dweight", "pspwght", 
                                 "pweight", "anweight", "region", "ppltrst", 
                                 "pplfair", "pplhlp", "trstlgl", "trstplc", 
                                 "trstplt", "trstprl", "lrscale", "stflife", 
                                 "stfeco", "imsmetn", "imdfetn", "imbgeco", 
                                 "imueclt", "crmvct", "aesfdrk", "dscrgrp", 
                                 "dscrrce", "dscrntn", "dscrlng", "dscretn", 
                                 "ctzcntr", "brncntr", "blgetmg", "qfimlng", 
                                 "qfimwht", "qfimcmt", "pplstrd", "fclcntr", 
                                 "dfegcf", "dfegcon", "dfeghbg", "smctmbe", 
                                 "gndr", "agea", "domicil", "eduyrs", "hinctnta", 
                                 "imptrad", "pdwrk", "uempla", "uempli", 
                                 "icpart2")]

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















