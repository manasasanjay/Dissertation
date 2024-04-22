library(haven)
library(readxl)

#load survey data 
ess1 <- read_dta("Desktop/PPE/DISS/ESS1e06_7/ESS1e06_7.dta")

#load census data for each country 
austria_1 <- read_excel("Desktop/PPE/DISS/Austria_2001_Census.xlsx")

czechia_1 <- read_excel("Desktop/PPE/DISS/Czechia_2001_Census.xlsx")

denmark_1 <- read_excel("Desktop/PPE/DISS/Denmark_2001_Census.xlsx")

finland_1 <- read_excel("Desktop/PPE/DISS/Finland_2001_Census.xlsx")

france_1 <- read_excel("Desktop/PPE/DISS/France_2001_Census.xlsx")

hungary_1 <- read_excel("Desktop/PPE/DISS/Hungary_2001_Census.xlsx")

ireland_1 <- read_excel("Desktop/PPE/DISS/Ireland_2001_Census.xlsx")

netherlands_1 <- read_excel("Desktop/PPE/DISS/Netherlands_2001_Census.xlsx")

norway_1 <- read_excel("Desktop/PPE/DISS/Norway_2001_Census.xlsx")

poland_1 <- read_excel("Desktop/PPE/DISS/Poland_2001_Census.xlsx")

portugal_1 <- read_excel("Desktop/PPE/DISS/Portugal_2001_Census.xlsx")

spain_1 <- read_excel("Desktop/PPE/DISS/Spain_2001_Census.xlsx")

sweden_1 <- read_excel("Desktop/PPE/DISS/Sweden_2001_Census.xlsx")

switzerland_1 <- read_excel("Desktop/PPE/DISS/Switzerland_2001_Census.xlsx")

#make a full dataset 

census_full_1 <- rbind(austria_1, czechia_1, denmark_1, finland_1, france_1, 
                       hungary_1, ireland_1, netherlands_1, norway_1, poland_1, 
                       portugal_1, spain_1, sweden_1, switzerland_1)
#rename columns 
colnames(census_full_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
#remove europe column 
census_full_1 <- census_full_1[, !names(census_full_1) %in% c("eur")]

#write some crap code to add codes to the region variables. 

#start by coding everything up to nuts 2, except the nuts1 variables 
#(to be able to compare over time)

ess1a <- ess1 

ess1a$regioncode <- 
  ifelse(ess1a$regionat == 1, ess1a$regioncode == "AT11", #Austria
  ifelse(ess1a$regionat == 2, ess1a$regioncode == "AT21", 
  ifelse(ess1a$regionat == 3, ess1a$regioncode == "AT12", 
  ifelse(ess1a$regionat == 4, ess1a$regioncode == "AT31", 
  ifelse(ess1a$regionat == 5, ess1a$regioncode == "AT32", 
  ifelse(ess1a$regionat == 6, ess1a$regioncode == "AT22", 
  ifelse(ess1a$regionat == 7, ess1a$regioncode == "AT33", 
  ifelse(ess1a$regionat == 8, ess1a$regioncode == "AT34", 
  ifelse(ess1a$regionat == 9, ess1a$regioncode == "AT13", 
  ifelse(ess1a$regioach == 1, ess1a$regioncode == "CH01", #Switzerland
  ifelse(ess1a$regioach == 2, ess1a$regioncode == "CH02", 
  ifelse(ess1a$regioach == 3, ess1a$regioncode == "CH03", 
  ifelse(ess1a$regioach == 4, ess1a$regioncode == "CH04", 
  ifelse(ess1a$regioach == 5, ess1a$regioncode == "CH05", 
  ifelse(ess1a$regioach == 6, ess1a$regioncode == "CH06", 
  ifelse(ess1a$regioach == 7, ess1a$regioncode == "CH07",
  ifelse(ess1a$regioncz == 1, ess1a$regioncode == "CZ01", #Czechia
  ifelse(ess1a$regioncz == 2, ess1a$regioncode == "CZ02", 
  ifelse(ess1a$regioncz == 3, ess1a$regioncode == "CZ03", 
  ifelse(ess1a$regioncz == 4, ess1a$regioncode == "CZ03", 
  ifelse(ess1a$regioncz == 5, ess1a$regioncode == "CZ04", 
  ifelse(ess1a$regioncz == 6, ess1a$regioncode == "CZ04", 
  ifelse(ess1a$regioncz == 7, ess1a$regioncode == "CZ05", 
  ifelse(ess1a$regioncz == 8, ess1a$regioncode == "CZ05", 
  ifelse(ess1a$regioncz == 9, ess1a$regioncode == "CZ05", 
  ifelse(ess1a$regioncz == 10, ess1a$regioncode == "CZ06", 
  ifelse(ess1a$regioncz == 11, ess1a$regioncode == "CZ06", 
  ifelse(ess1a$regioncz == 12, ess1a$regioncode == "CZ07", 
  ifelse(ess1a$regioncz == 13, ess1a$regioncode == "CZ07", 
  ifelse(ess1a$regioncz == 14, ess1a$regioncode == "CZ08", 
  ifelse(ess1a$regiondk == 1, ess1a$regioncode == "DK01", #Denmark
  ifelse(ess1a$regiondk == 2, ess1a$regioncode == "DK01", 
  ifelse(ess1a$regiondk == 3, ess1a$regioncode == "DK01", 
  ifelse(ess1a$regiondk == 4, ess1a$regioncode == "DK02", 
  ifelse(ess1a$regiondk == 5, ess1a$regioncode == "DK02", 
  ifelse(ess1a$regiondk == 6, ess1a$regioncode == "DK02", 
  ifelse(ess1a$regiondk == 7, ess1a$regioncode == "DK01", 
  ifelse(ess1a$regiondk == 8, ess1a$regioncode == "DK03", 
  ifelse(ess1a$regiondk == 9, ess1a$regioncode == "DK03", 
  ifelse(ess1a$regiondk == 10, ess1a$regioncode == "DK03", 
  ifelse(ess1a$regiondk == 11, ess1a$regioncode == "DK03", 
  ifelse(ess1a$regiondk == 12, ess1a$regioncode == "DK04", 
  ifelse(ess1a$regiondk == 13, ess1a$regioncode == "DK04", 
  ifelse(ess1a$regiondk == 14, ess1a$regioncode == "DK04", 
  ifelse(ess1a$regiondk == 15, ess1a$regioncode == "DK05", 
  ifelse(ess1a$regiones == 11, ess1a$regioncode == "ES11", #Spain
  ifelse(ess1a$regiones == 12, ess1a$regioncode == "ES12", 
  ifelse(ess1a$regiones == 13, ess1a$regioncode == "ES13", 
  ifelse(ess1a$regiones == 21, ess1a$regioncode == "ES21", 
  ifelse(ess1a$regiones == 22, ess1a$regioncode == "ES22", 
  ifelse(ess1a$regiones == 23, ess1a$regioncode == "ES23", 
  ifelse(ess1a$regiones == 24, ess1a$regioncode == "ES24", 
  ifelse(ess1a$regiones == 30, ess1a$regioncode == "ES30", 
  ifelse(ess1a$regiones == 41, ess1a$regioncode == "ES41", 
  ifelse(ess1a$regiones == 42, ess1a$regioncode == "ES42", 
  ifelse(ess1a$regiones == 43, ess1a$regioncode == "ES43", 
  ifelse(ess1a$regiones == 51, ess1a$regioncode == "ES51", 
  ifelse(ess1a$regiones == 52, ess1a$regioncode == "ES52", 
  ifelse(ess1a$regiones == 53, ess1a$regioncode == "ES53", 
  ifelse(ess1a$regiones == 61, ess1a$regioncode == "ES61", 
  ifelse(ess1a$regiones == 62, ess1a$regioncode == "ES62", 
  ifelse(ess1a$regiones == 63, ess1a$regioncode == "ES63", 
  ifelse(ess1a$regiones == 70, ess1a$regioncode == "ES70",
  ifelse(ess1a$regionfr == 1, ess1a$regioncode == "FR1", #france
  ifelse(ess1a$regionfr == 2, ess1a$regioncode == "FR2", 
  ifelse(ess1a$regionfr == 3, ess1a$regioncode == "FR2", 
  ifelse(ess1a$regionfr == 4, ess1a$regioncode == "FR3", 
  ifelse(ess1a$regionfr == 5, ess1a$regioncode == "FR4", 
  ifelse(ess1a$regionfr == 6, ess1a$regioncode == "FR5", 
  ifelse(ess1a$regionfr == 7, ess1a$regioncode == "FR6", 
  ifelse(ess1a$regionfr == 8, ess1a$regioncode == "FR7", 
  ifelse(ess1a$regionfr == 9, ess1a$regioncode == "FR8", 
  ifelse(ess1a$regionhu == 1, ess1a$regioncode == "HU10", #Hungary
  ifelse(ess1a$regionhu == 2, ess1a$regioncode == "HU21", 
  ifelse(ess1a$regionhu == 3, ess1a$regioncode == "HU22", 
  ifelse(ess1a$regionhu == 4, ess1a$regioncode == "HU23", 
  ifelse(ess1a$regionhu == 5, ess1a$regioncode == "HU31", 
  ifelse(ess1a$regionhu == 6, ess1a$regioncode == "HU32", 
  ifelse(ess1a$regionhu == 7, ess1a$regioncode == "HU33", 
  ifelse(ess1a$regionie == 1, ess1a$regioncode == "IE01", #Ireland
  ifelse(ess1a$regionie == 2, ess1a$regioncode == "IE01", 
  ifelse(ess1a$regionie == 3, ess1a$regioncode == "IE01", 
  ifelse(ess1a$regionie == 4, ess1a$regioncode == "IE02", 
  ifelse(ess1a$regionie == 5, ess1a$regioncode == "IE02", 
  ifelse(ess1a$regionie == 6, ess1a$regioncode == "IE02", 
  ifelse(ess1a$regionie == 7, ess1a$regioncode == "IE02", 
  ifelse(ess1a$regionie = 8, ess1a$regioncode == "IE02", 
  ifelse(ess1a$regionnl == 111, ess1a$regioncode == "NL11", #netherlands
  ifelse(ess1a$regionnl == 112, ess1a$regioncode == "NL11", 
  ifelse(ess1a$regionnl == 113, ess1a$regioncode == "NL11", 
  ifelse(ess1a$regionnl == 121, ess1a$regioncode == "NL12", 
  ifelse(ess1a$regionnl == 122, ess1a$regioncode == "NL12", 
  ifelse(ess1a$regionnl == 123, ess1a$regioncode == "NL12",
  ifelse(ess1a$regionnl == 131, ess1a$regioncode == "NL13", 
  ifelse(ess1a$regionnl == 132, ess1a$regioncode == "NL13", 
  ifelse(ess1a$regionnl == 133, ess1a$regioncode == "NL13", 
  ifelse(ess1a$regionnl == 211, ess1a$regioncode == "NL21", 
  ifelse(ess1a$regionnl == 212, ess1a$regioncode == "NL21", 
  ifelse(ess1a$regionnl == 213, ess1a$regioncode = "NL21", 
  ifelse(ess1a$regionnl == 221, ess1a$regioncode == "NL22", 
  ifelse(ess1a$regionnl == 222, ess1a$regioncode == "NL22", 
  ifelse(ess1a$regionnl == 223, ess1a$regioncode == "NL22", 
  ifelse(ess1a$regionnl == 224, ess1a$regioncode == "NL22", 
  ifelse(ess1a$regionnl == 230, ess1a$regioncode == "NL23", 
  ifelse(ess1a$regionnl == 310, ess1a$regioncode == "NL31", 
  ifelse(ess1a$regionnl == 321, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 322, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 323, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 324, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 325, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 326, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 327, ess1a$regioncode == "NL32", 
  ifelse(ess1a$regionnl == 331, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 332, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 333, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 334, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 335, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 336, ess1a$regioncode == "NL33", 
  ifelse(ess1a$regionnl == 341, ess1a$regioncode == "NL34", 
  ifelse(ess1a$regionnl == 342, ess1a$regioncode == "NL34", 
  ifelse(ess1a$regionnl == 411, ess1a$regioncode == "NL41", 
  ifelse(ess1a$regionnl == 412, ess1a$regioncode == "NL41", 
  ifelse(ess1a$regionnl == 413, ess1a$regioncode == "NL41", 
  ifelse(ess1a$regionnl == 414, ess1a$regioncode == "NL41", 
  ifelse(ess1a$regionnl == 421, ess1a$regioncode == "NL42", 
  ifelse(ess1a$regionnl == 422, ess1a$regioncode == "NL42", 
  ifelse(ess1a$regionnl == 423, ess1a$regioncode == "NL42", 
  ifelse(ess1a$regionno == 1, ess1a$regioncode == "NO01", #Norway
  ifelse(ess1a$regionno == 2, ess1a$regioncode == "NO02", 
  ifelse(ess1a$regionno == 3, ess1a$regioncode == "NO03",
  ifelse(ess1a$regionno == 4, ess1a$regioncode == "NO04", 
  ifelse(ess1a$regionno == 5, ess1a$regioncode == "NO05", 
  ifelse(ess1a$regionno == 6, ess1a$regioncode == "NO06", 
  ifelse(ess1a$regionno == 7, ess1a$regioncode == "NO07", 
  ifelse(ess1a$regionpl == 2, ess1a$regioncode == "PL51", #Poland
  ifelse(ess1a$regionpl == 4, ess1a$regioncode == "PL61", 
  ifelse(ess1a$regionpl == 6, ess1a$regioncode == "PL31", 
  ifelse(ess1a$regionpl == 8, ess1a$regioncode == "PL43", 
  ifelse(ess1a$regionpl == 10, ess1a$regioncode == "PL71", 
  ifelse(ess1a$regionpl == 12, ess1a$regioncode == "PL21", 
  ifelse(ess1a$regionpl == 14, ess1a$regioncode == "PL12", 
  ifelse(ess1a$regionpl == 16, ess1a$regioncode == "PL52", 
  ifelse(ess1a$regionpl == 18, ess1a$regioncode == "PL32", 
  ifelse(ess1a$regionpl == 20, ess1a$regioncode == "PL34", 
  ifelse(ess1a$regionpl == 22, ess1a$regioncode == "PL63", 
  ifelse(ess1a$regionpl == 24, ess1a$regioncode == "PL22", 
  ifelse(ess1a$regionpl == 26, ess1a$regioncode == "PL33", 
  ifelse(ess1a$regionpl == 28, ess1a$regioncode == "PL62", 
  ifelse(ess1a$regionpl == 30, ess1a$regioncode == "PL41", 
  ifelse(ess1a$regionpl == 32, ess1a$regioncode == "PL42", 
  ifelse(ess1a$regionpt == 1, ess1a$regioncode == "PT11", #Portugal
  ifelse(ess1a$regionpt == 2, ess1a$regioncode == "PT16", 
  ifelse(ess1a$regionpt == 3, ess1a$regioncode == "PT17", 
  ifelse(ess1a$regionpt == 4, ess1a$regioncode == "PT18", 
  ifelse(ess1a$regionpt == 5, ess1a$regioncode == "PT15", 
  ifelse(ess1a$regionse == 1, ess1a$regioncode == "SE11", 
  ifelse(ess1a$regionse == 2, ess1a$regioncode == "SE12", 
  ifelse(ess1a$regionse == 3, ess1a$regioncode == "SE22", 
  ifelse(ess1a$regionse == 4, ess1a$regioncode == "SE31", 
  ifelse(ess1a$regionse == 5, ess1a$regioncode == "SE32", 
  ifelse(ess1a$regionse == 6, ess1a$regioncode == "SE33", 
  ifelse(ess1a$regionse == 7, ess1a$regioncode == "SE21", 
  ifelse(ess1a$regionse == 8, ess1a$regioncode == "SE23", 
  NA)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))






