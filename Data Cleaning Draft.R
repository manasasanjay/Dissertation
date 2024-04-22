library(haven)
library(readxl)
library(dplyr)

#load survey data 
ess1 <- read_dta("Desktop/PPE/DISS/ESS1e06_7/ESS1e06_7.dta")

#load census data for each country 
austria_1 <- read_excel("Desktop/PPE/DISS/Austria_2001_Census.xlsx")

czechia_1 <- read_excel("Desktop/PPE/DISS/Czechia_2001_Censusb.xlsx")

denmark_1 <- read_excel("Desktop/PPE/DISS/Denmark_2001_Censusb.xlsx")

finland_1 <- read_excel("Desktop/PPE/DISS/Finland_2001_Census.xlsx")

france_1 <- read_excel("Desktop/PPE/DISS/France_2001_Census.xlsx")

hungary_1 <- read_excel("Desktop/PPE/DISS/Hungary_2001_Census.xlsx")

ireland_1 <- read_excel("Desktop/PPE/DISS/Ireland_2001_Censusb.xlsx")

netherlands_1 <- read_excel("Desktop/PPE/DISS/Netherlands_2001_Censusb.xlsx")

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

#assign region codes to each country (each individual dataset)

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


#Czechia (coded to nuts3)
ess1_cza <- ess1_cza %>% 
  mutate(reg_code = case_when(
    regioncz == 1 ~ "CZ010", 
    regioncz == 2 ~ "CZ020", 
    regioncz == 3 ~ "CZ031", 
    regioncz == 4 ~ "CZ032", 
    regioncz == 5 ~ "CZ041", 
    regioncz == 6 ~ "CZ042", 
    regioncz == 7 ~ "CZ051", 
    regioncz == 8 ~ "CZ052", 
    regioncz == 9 ~ "CZ053", 
    regioncz == 10 ~ "CZ061", 
    regioncz == 11 ~ "CZ062", 
    regioncz == 12 ~ "CZ071", 
    regioncz == 13 ~ "CZ072", 
    regioncz == 14 ~ "CZ080", 
    TRUE ~ NA
  ))

#Denmark (coded to nuts3)
ess1_dka <- ess1_dka %>% 
  mutate(reg_code = case_when(
    regiondk == 1 ~ "DK001", 
    regiondk == 2 ~ "DK002", 
    regiondk == 3 ~ "DK003", 
    regiondk == 4 ~ "DK004", 
    regiondk == 5 ~ "DK005", 
    regiondk == 6 ~ "DK006", 
    regiondk == 7 ~ "DK007", 
    regiondk == 8 ~ "DK008", 
    regiondk == 9 ~ "DK009", 
    regiondk == 10 ~ "DK00A", 
    regiondk == 11 ~ "DK00B", 
    regiondk == 12 ~ "DK00C", 
    regiondk == 13 ~ "DK00D", 
    regiondk == 14 ~ "DK00E", 
    regiondk == 15 ~ "DK00F", 
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

#france 

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

ess1_iea <- ess1_iea %>%
  mutate(reg_code = case_when(
    regionie == 1 ~ "IE011", 
    regionie == 2 ~ "IE012", 
    regionie == 3 ~ "IE013", 
    regionie == 4 ~ "IE021", 
    regionie == 5 ~ "IE022", 
    regionie == 6 ~ "IE023", 
    regionie == 7 ~ "IE024", 
    regionie == 8 ~ "IE025", 
    TRUE ~ NA
  ))

ess1_nla <- ess1_nla %>%
  mutate(reg_code = case_when(
    regionnl == 111 ~ "NL111", 
    regionnl == 112 ~ "NL112", 
    regionnl == 113 ~ "NL113", 
    regionnl == 121 ~ "NL121", 
    regionnl == 122 ~ "NL122", 
    regionnl == 123 ~ "NL123", 
    regionnl == 131 ~ "NL131", 
    regionnl == 132 ~ "NL132", 
    regionnl == 133 ~ "NL133", 
    regionnl == 211 ~ "NL211", 
    regionnl == 212 ~ "NL212", 
    regionnl == 213 ~ "NL213", 
    regionnl == 221 ~ "NL221", 
    regionnl == 222 ~ "NL222", 
    regionnl == 223 ~ "NL223", 
    regionnl == 224 ~ "NL224", 
    regionnl == 230 ~ "NL230", 
    regionnl == 310 ~ "NL310", 
    regionnl == 321 ~ "NL321", 
    regionnl == 322 ~ "NL322", 
    regionnl == 323 ~ "NL323", 
    regionnl == 324 ~ "NL324", 
    regionnl == 325 ~ "NL325", 
    regionnl == 326 ~ "NL326", 
    regionnl == 327 ~ "NL327", 
    regionnl == 331 ~ "NL331", 
    regionnl == 332 ~ "NL332", 
    regionnl == 333 ~ "NL333", 
    regionnl == 334 ~ "NL334", 
    regionnl == 335 ~ "NL335", 
    regionnl == 336 ~ "NL336", 
    regionnl == 341 ~ "NL341", 
    regionnl == 342 ~ "NL342", 
    regionnl == 411 ~ "NL411", 
    regionnl == 412 ~ "NL412", 
    regionnl == 413 ~ "NL413", 
    regionnl == 414 ~ "NL414", 
    regionnl == 421 ~ "NL421", 
    regionnl == 422 ~ "NL422", 
    regionnl == 423 ~ "NL423", 
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

#put it all into one dataset 
ess1_a_full <- rbind(ess1_ata, ess1_cha, ess1_cza, ess1_dka, ess1_esa, ess1_fra, 
                     ess1_hua, ess1_iea, ess1_nla, ess1_noa, ess1_pla, ess1_pta, 
                     ess1_sea)

#subset dataset to only have variables required 

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
                                 "imdfetn", "qfimwht", "qfimcmt", "imueclt", 
                                 "idetalv", "pplstrd", "imgfrnd", "imgclg", 
                                 "yrlvdae", "empl", "gndr", "agea", "domicil", 
                                 "eduyrs", "hinctnt", "lvgptn", "imptrad", 
                                 "crmvct")]








