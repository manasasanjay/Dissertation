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

  




