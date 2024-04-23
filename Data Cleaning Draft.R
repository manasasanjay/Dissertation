library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)

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

census_full_1 <- rbind(austria_1, czechia_1, denmark_1, france_1, 
                       hungary_1, ireland_1, netherlands_1, norway_1, poland_1, 
                       portugal_1, spain_1, sweden_1, switzerland_1)
#rename columns 
colnames(census_full_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(austria_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(czechia_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(denmark_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(france_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(hungary_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")

colnames(ireland_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(netherlands_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(norway_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(poland_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(portugal_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(spain_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(sweden_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")
colnames(switzerland_1) <- c("reg_code", "reg_name", "eur", "eu_not_rep", 
                             "former_ussr_eur", "north_afr", "other_afr", 
                             "north_am", "other_am", "mid_east", 
                             "former_ussr_asia", "other_asia", "oceania", 
                             "rep", "total")

#remove europe & total column 
austria_1 <- austria_1[, !names(austria_1) %in% c("eur", "total")]
czechia_1 <- czechia_1[, !names(czechia_1) %in% c("eur", "total")]
denmark_1 <- denmark_1[, !names(denmark_1) %in% c("eur", "total")]
france_1 <- france_1[, !names(france_1) %in% c("eur", "total")]
hungary_1 <- hungary_1[, !names(hungary_1) %in% c("eur", "total")]
ireland_1 <- ireland_1[, !names(ireland_1) %in% c("eur", "total")]
netherlands_1 <- netherlands_1[, !names(netherlands_1) %in% c("eur", "total")]
norway_1 <- norway_1[, !names(norway_1) %in% c("eur", "total")]
poland_1 <- poland_1[, !names(poland_1) %in% c("eur", "total")]
portugal_1 <- portugal_1[, !names(portugal_1) %in% c("eur", "total")]
spain_1 <- spain_1[, !names(spain_1) %in% c("eur", "total")]
sweden_1 <- sweden_1[, !names(sweden_1) %in% c("eur", "total")]
switzerland_1 <- switzerland_1[, !names(switzerland_1) %in% c("eur", "total")]


#spain has NA values for ussr asian countries, code them as NA 
spain_1$former_ussr_asia <- NA

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

#calculate ethnic share in each region for each country to calculate HHI 
#first create a total column 

#function to make life easier 

add_total_column <- function(data) {
  start_col <- 3  
  end_col <- 13  
  
  # Calculate row sums for the specified columns
  total <- rowSums(data[, start_col:end_col], na.rm = TRUE)
  
  # Add the total column to the dataset
  data$total <- total
  return(data)
}

#austria 
austria_1 <- add_total_column(austria_1)

#czechia 
czechia_1 <- add_total_column(czechia_1)

#denmark 
denmark_1 <- add_total_column(denmark_1)

#france
france_1 <- add_total_column(france_1)

#hungary 
hungary_1 <- add_total_column(hungary_1)

#ireland 
ireland_1 <- add_total_column(ireland_1)

#netherlands 
netherlands_1 <- add_total_column(netherlands_1)

#norway 
norway_1 <- add_total_column(norway_1)

#poland 
poland_1 <- add_total_column(poland_1)

#portugal 
portugal_1 <- add_total_column(portugal_1)

#spain 
spain_1 <- add_total_column(spain_1)

#sweden 
sweden_1 <- add_total_column(sweden_1)

#switzerland 
switzerland_1 <- add_total_column(switzerland_1)

#calculate ethnic share for each group and HHI 
#HHI = sum(eth_share^2)

#function to compute HHI 

add_HHI_column <- function(data) {
  start_col <- 3  
  end_col <- 13   
  total <- rowSums(data[, start_col:end_col], na.rm = TRUE)
  
  # Calculate the squared proportion of each variable and sum them up
  HHI <- rowSums((data[, start_col:end_col] / total)^2, na.rm = TRUE)
  
  # Add the HHI values as a new column to the dataset
  data$HHI <- HHI
  return(data)
}

#austria 
austria_1 <- add_HHI_column(austria_1)

#czechia
czechia_1 <- add_HHI_column(czechia_1)

#denmark 
denmark_1 <- add_HHI_column(denmark_1)

#france 
france_1 <- add_HHI_column(france_1)

#hungary 
hungary_1 <- add_HHI_column(hungary_1)

#ireland 
ireland_1 <- add_HHI_column(ireland_1)

#netherlands
netherlands_1 <- add_HHI_column(netherlands_1)

#norway 
norway_1 <- add_HHI_column(norway_1)

#poland 
poland_1 <- add_HHI_column(poland_1)

#portugal 
portugal_1 <- add_HHI_column(portugal_1)

#spain
spain_1 <- add_HHI_column(spain_1)

#sweden 
sweden_1 <- add_HHI_column(sweden_1)

#switzerland 
switzerland_1 <- add_HHI_column(switzerland_1)

#Ethnic fractionalisation index = 1- HHI

#austria 
austria_1$Eth_Frac <- (1-austria_1$HHI)

#czechia 
czechia_1$Eth_Frac <- (1-czechia_1$HHI)

#denmark 
denmark_1$Eth_Frac <- (1-denmark_1$HHI)

#france 
france_1$Eth_Frac <- (1-france_1$HHI)

#hungary 
hungary_1$Eth_Frac <- (1-hungary_1$HHI)

#ireland 
ireland_1$Eth_Frac <- (1-ireland_1$HHI)

#netherlands 
netherlands_1$Eth_Frac <- (1-netherlands_1$HHI)

#norway 
norway_1$Eth_Frac <- (1-norway_1$HHI)

#poland 
poland_1$Eth_Frac <- (1-poland_1$HHI)

#portugal 
portugal_1$Eth_Frac <- (1-portugal_1$HHI)

#spain
spain_1$Eth_Frac <- (1-spain_1$HHI)

#sweden 
sweden_1$Eth_Frac <- (1-sweden_1$HHI)

#switzerland 
switzerland_1$Eth_Frac <- (1-switzerland_1$HHI)

#put into a single dataset 

census_full_1 <- rbind(austria_1, czechia_1, denmark_1, france_1, 
                       hungary_1, ireland_1, netherlands_1, norway_1, poland_1, 
                       portugal_1, spain_1, sweden_1, switzerland_1)

#------------------------------------------------------------------------------#

#recode 
ess1_a_subset$ppltrst <- as.numeric(ess1_a_subset$ppltrst)
ess1_a_subset$pplfair <- as.numeric(ess1_a_subset$pplfair)
ess1_a_subset$pplhlp <- as.numeric(ess1_a_subset$pplhlp)
ess1_a_subset$lrscale <- as.numeric(ess1_a_subset$lrscale)
ess1_a_subset$stflife <- as.numeric(ess1_a_subset$stflife)
ess1_a_subset$stfeco <- as.numeric(ess1_a_subset$stfeco)
ess1_a_subset$dclmig <- as.character(ess1_a_subset$dclmig)
ess1_a_subset$aesfdrk <- as.character(ess1_a_subset$aesfdrk)
ess1_a_subset$ctzcntr <- as.character(ess1_a_subset$ctzcntr)
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


trst_data <- ess1_a_subset[, c("ppltrst", "pplfair", "pplhlp")]
trst_data <- na.omit(trst_data)

cronbach.alpha(trst_data) #0.77

instrst_data <- ess1_a_subset[, c("trstlgl", "trstplc", "trstplt", "trstprl")]
instrst_data <- na.omit(instrst_data)

cronbach.alpha(instrst_data) #0.84

#Before being able to do PCA, standardise all the variables to be used as 
#indicators 

ess1_a_subset$lrscale_std <- scale(ess1_a_subset$lrscale, scale = TRUE) #left-right scale

ess1_a_subset$imsmetn_std <- scale(ess1_a_subset$imsmetn, scale = TRUE) #allow many/few same ethnicity immigrants

ess1_a_subset$imdfetn_std <- scale(ess1_a_subset$imdfetn, scale = TRUE) #allow many/few diff ethnicity immigrants

ess1_a_subset$qfimwht_std <- scale(ess1_a_subset$qfimwht, scale = TRUE) #qualificatino for immigration: be white

ess1_a_subset$qfimcmt_std <- scale(ess1_a_subset$qfimcmt, scale = TRUE) #qualification for immigration: be committed to way of life in the country 

ess1_a_subset$imueclt_std <- scale(ess1_a_subset$imueclt, scale = TRUE) #cultural life undermined/enriched by immigrants

ess1_a_subset$pplstrd_std <- scale(ess1_a_subset$pplstrd, scale = TRUE) #better for country if everyone shares customs and trads

ess1_a_subset$imptrad_std <- scale(ess1_a_subset$imptrad, scale = TRUE) #important to follow traditions and customs


png(file = "Dissertation GitHub/figures/corrplot1.png",
    width = 6000, height = 4000, res = 650)

corrplot.mixed(
  cor(ess1_a_subset[,64:71],
      use="complete.obs"),
  upper="ellipse",tl.cex=0.3)

dev.off()



pcafit <- prcomp(na.omit(ess1_a_subset[,64:71], scale.=TRUE))
pcafit

summary(pcafit)

png(file = "Dissertation GitHub/figures/screeplot1.png",
    width = 6000, height = 4000, res = 650)
#screeplot(pcafit,type="barplot")
screeplot(pcafit,type="lines")
dev.off()

nrow(pcafit$x[1])

ess1_a_subset$PC1 <- pcafit$x[,1]



