library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)

#load survey data 
ess1 <- read_dta("Desktop/PPE/DISS/ESS1e06_7/ESS1e06_7.dta")

#first split the data by country because it is the integrated file and you 
#cannot assign the stupid codes until you split it 


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






