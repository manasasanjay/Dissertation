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
library(kableExtra)
library(sf)
library(ggpubr)
library(optimx)


#------------------------------Cleaning Census Data 2001------------------------


#load 2001 census data for each country 
austria_1 <- read_excel("Desktop/PPE/DISS/Austria_2001_Census.xlsx")

czechia_1 <- read_excel("Desktop/PPE/DISS/Czechia_2001_Census.xlsx")

denmark_1 <- read_excel("Desktop/PPE/DISS/Denmark_2001_Census.xlsx")

france_1 <- read_excel("Desktop/PPE/DISS/France_2001_Census.xlsx")

ireland_1 <- read_excel("Desktop/PPE/DISS/Ireland_2001_Census.xlsx")

netherlands_1 <- read_excel("Desktop/PPE/DISS/Netherlands_2001_Census.xlsx")

norway_1 <- read_excel("Desktop/PPE/DISS/Norway_2001_Census.xlsx")

poland_1 <- read_excel("Desktop/PPE/DISS/Poland_2001_Census.xlsx")

portugal_1 <- read_excel("Desktop/PPE/DISS/Portugal_2001_Census.xlsx")

spain_1 <- read_excel("Desktop/PPE/DISS/Spain_2001_Census.xlsx")

sweden_1 <- read_excel("Desktop/PPE/DISS/Sweden_2001_Census.xlsx")

switzerland_1 <- read_excel("Desktop/PPE/DISS/Switzerland_2001_Census.xlsx")

census_2001_controls <- read_excel("Desktop/PPE/DISS/2001_Contextual_Controls.xlsx")

#make a full dataset for the first census data 

census_full_2001 <- rbind(austria_1, czechia_1, denmark_1, france_1, ireland_1, 
                          netherlands_1, norway_1, poland_1, portugal_1, 
                          spain_1, sweden_1, switzerland_1)

#rename columns 

colnames(census_full_2001) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                                "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")
#austria 
colnames(austria_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#czechia 
colnames(czechia_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#denmark 
colnames(denmark_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#france 
colnames(france_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                        "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#ireland 
colnames(ireland_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#netherlands
colnames(netherlands_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                             "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")


#norway 
colnames(norway_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                        "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#poland 
colnames(poland_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                        "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#portugal
colnames(portugal_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                          "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#spain 
colnames(spain_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                       "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#sweden 
colnames(sweden_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                        "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")


#switzerland 
colnames(switzerland_1) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                             "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#remove total column 
austria_1 <- austria_1[, !names(austria_1) %in% "Total"]
czechia_1 <- czechia_1[, !names(czechia_1) %in% "Total"]
denmark_1 <- denmark_1[, !names(denmark_1) %in% "Total"]
france_1 <- france_1[, !names(france_1) %in% "Total"]
ireland_1 <- ireland_1[, !names(ireland_1) %in% "Total"]
netherlands_1 <- netherlands_1[, !names(netherlands_1) %in% "Total"]
norway_1 <- norway_1[, !names(norway_1) %in% "Total"]
poland_1 <- poland_1[, !names(poland_1) %in% "Total"]
portugal_1 <- portugal_1[, !names(portugal_1) %in% "Total"]
spain_1 <- spain_1[, !names(spain_1) %in% "Total"]
sweden_1 <- sweden_1[, !names(sweden_1) %in% "Total"]
switzerland_1 <- switzerland_1[, !names(switzerland_1) %in% "Total"]


#calculate ethnic share in each region for each country to calculate HHI 
#first create a total column 

#function to make life easier 

add_total_column <- function(data) {
  start_col <- 3  
  end_col <- 10  
  
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
  end_col <- 10   
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
                       ireland_1, netherlands_1, norway_1, poland_1, 
                       portugal_1, spain_1, sweden_1, switzerland_1)

#------------------------------------------------------------------------------#
#recode NA variables from controls to NA 

census_2001_controls <- census_2001_controls %>%
  mutate(across(everything(), ~ ifelse(. == "NA", NA, .)))

#merge the census data and the controls by region code
#but first rename columns 
colnames(census_2001_controls) <- c("country", "reg_code", "reg", "avgeduyrs", 
                                    "res_turn", "sin_par_hh", "unemp_rate")

merged_census_2001 <- merge(census_full_1, census_2001_controls, by = "reg_code", 
                            all.x = TRUE)

#make all variables numeric 
merged_census_2001 <- merged_census_2001 %>%
  mutate_at(vars(res_turn, avgeduyrs, sin_par_hh, unemp_rate), as.numeric)


#------------------------------Cleaning Census Data 2011------------------------

#load 2011 census data for each country 
austria_11 <- read_excel("Desktop/PPE/DISS/Austria_2011_Census.xlsx")

czechia_11 <- read_excel("Desktop/PPE/DISS/Czechia_2011_Census.xlsx")

denmark_11 <- read_excel("Desktop/PPE/DISS/Denmark_2011_Census.xlsx")

france_11 <- read_excel("Desktop/PPE/DISS/France_2011_Census.xlsx")

ireland_11 <- read_excel("Desktop/PPE/DISS/Ireland_2011_Census.xlsx")

netherlands_11 <- read_excel("Desktop/PPE/DISS/Netherlands_2011_Census.xlsx")

norway_11 <- read_excel("Desktop/PPE/DISS/Norway_2011_Census.xlsx")

poland_11 <- read_excel("Desktop/PPE/DISS/Poland_2011_Census.xlsx")

portugal_11 <- read_excel("Desktop/PPE/DISS/Portugal_2011_Census.xlsx")

spain_11 <- read_excel("Desktop/PPE/DISS/Spain_2011_Census.xlsx")

sweden_11 <- read_excel("Desktop/PPE/DISS/Sweden_2011_Census.xlsx")

switzerland_11 <- read_excel("Desktop/PPE/DISS/Switzerland_2011_Census.xlsx")

census_2011_controls <- read_excel("Desktop/PPE/DISS/2011_Contextual_Controls.xlsx")

#make a full dataset for the first census data 

#austria 
colnames(austria_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                          "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#czechia 
colnames(czechia_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                          "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#denmark 
colnames(denmark_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                          "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#france 
colnames(france_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#ireland 
colnames(ireland_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                          "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#netherlands
colnames(netherlands_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                              "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")


#norway 
colnames(norway_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#poland 
colnames(poland_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#portugal
colnames(portugal_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                           "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#spain 
colnames(spain_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                        "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#sweden 
colnames(sweden_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                         "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")


#switzerland 
colnames(switzerland_11) <- c("reg_code", "reg", "EU_not_RC", "Eur_Oth", "Afr", 
                              "Nor_Am", "Oth_Am", "Asia", "Ocna", "RC", "Total")

#remove total column 
austria_11 <- austria_11[, !names(austria_11) %in% "Total"]
czechia_11 <- czechia_11[, !names(czechia_11) %in% "Total"]
denmark_11 <- denmark_11[, !names(denmark_11) %in% "Total"]
france_11 <- france_11[, !names(france_11) %in% "Total"]
ireland_11 <- ireland_11[, !names(ireland_11) %in% "Total"]
netherlands_11 <- netherlands_11[, !names(netherlands_11) %in% "Total"]
norway_11 <- norway_11[, !names(norway_11) %in% "Total"]
poland_11 <- poland_11[, !names(poland_11) %in% "Total"]
portugal_11 <- portugal_11[, !names(portugal_11) %in% "Total"]
spain_11 <- spain_11[, !names(spain_11) %in% "Total"]
sweden_11 <- sweden_11[, !names(sweden_11) %in% "Total"]
switzerland_11 <- switzerland_11[, !names(switzerland_11) %in% "Total"]

#calculate ethnic share in each region for each country to calculate HHI 
#first create a total column 

#function to make life easier 

add_total_column <- function(data) {
  start_col <- 3  
  end_col <- 10  
  
  # Calculate row sums for the specified columns
  total <- rowSums(data[, start_col:end_col], na.rm = TRUE)
  
  # Add the total column to the dataset
  data$total <- total
  return(data)
}

#austria 
austria_11 <- add_total_column(austria_11)

#czechia 
czechia_11 <- add_total_column(czechia_11)

#denmark 
denmark_11 <- add_total_column(denmark_11)

#france
france_11 <- add_total_column(france_11)

#ireland 
ireland_11 <- add_total_column(ireland_11)

#netherlands 
netherlands_11 <- add_total_column(netherlands_11)

#norway 
norway_11 <- add_total_column(norway_11)

#poland 
poland_11 <- add_total_column(poland_11)

#portugal 
portugal_11 <- add_total_column(portugal_11)

#spain 
spain_11 <- add_total_column(spain_11)

#sweden 
sweden_11 <- add_total_column(sweden_11)

#wtf is wrong with switzerland man 
class(switzerland_11$EU_not_RC)
class(switzerland_11$Eur_Oth)
class(switzerland_11$Afr)
class(switzerland_11$Nor_Am)
class(switzerland_11$Oth_Am)
class(switzerland_11$Asia)
class(switzerland_11$Ocna)

#ah i see stupid oceania has an na 
switzerland_11$Ocna <- as.numeric(switzerland_11$Ocna)

#switzerland 
switzerland_11 <- add_total_column(switzerland_11)

#calculate ethnic share for each group and HHI 
#HHI = sum(eth_share^2)

#function to compute HHI 

add_HHI_column <- function(data) {
  start_col <- 3  
  end_col <- 10   
  total <- rowSums(data[, start_col:end_col], na.rm = TRUE)
  
  # Calculate the squared proportion of each variable and sum them up
  HHI <- rowSums((data[, start_col:end_col] / total)^2, na.rm = TRUE)
  
  # Add the HHI values as a new column to the dataset
  data$HHI <- HHI
  return(data)
}

#austria 
austria_11 <- add_HHI_column(austria_11)

#czechia
czechia_11 <- add_HHI_column(czechia_11)

#denmark 
denmark_11 <- add_HHI_column(denmark_11)

#france 
france_11 <- add_HHI_column(france_11)

#ireland 
ireland_11 <- add_HHI_column(ireland_11)

#netherlands
netherlands_11 <- add_HHI_column(netherlands_11)

#norway 
norway_11 <- add_HHI_column(norway_11)

#poland 
poland_11 <- add_HHI_column(poland_11)

#portugal 
portugal_11 <- add_HHI_column(portugal_11)

#spain
spain_11 <- add_HHI_column(spain_11)

#sweden 
sweden_11 <- add_HHI_column(sweden_11)

#switzerland 
switzerland_11 <- add_HHI_column(switzerland_11)

#Ethnic fractionalisation index = 1- HHI

#austria 
austria_11$Eth_Frac <- (1-austria_11$HHI)

#czechia 
czechia_11$Eth_Frac <- (1-czechia_11$HHI)

#denmark 
denmark_11$Eth_Frac <- (1-denmark_11$HHI)

#france 
france_11$Eth_Frac <- (1-france_11$HHI)

#ireland 
ireland_11$Eth_Frac <- (1-ireland_11$HHI)

#netherlands 
netherlands_11$Eth_Frac <- (1-netherlands_11$HHI)

#norway 
norway_11$Eth_Frac <- (1-norway_11$HHI)

#poland 
poland_11$Eth_Frac <- (1-poland_11$HHI)

#portugal 
portugal_11$Eth_Frac <- (1-portugal_11$HHI)

#spain
spain_11$Eth_Frac <- (1-spain_11$HHI)

#sweden 
sweden_11$Eth_Frac <- (1-sweden_11$HHI)

#switzerland 
switzerland_11$Eth_Frac <- (1-switzerland_11$HHI)

#put into a single dataset 

census_full_11 <- rbind(austria_11, czechia_11, denmark_11, france_11, 
                        ireland_11, netherlands_11, norway_11, poland_11, 
                        portugal_11, spain_11, sweden_11, switzerland_11)

#------------------------------------------------------------------------------#
#recode NA variables from controls to NA 

census_2011_controls <- census_2011_controls %>%
  mutate(across(everything(), ~ ifelse(. == "NA", NA, .)))

#merge the census data and the controls by region code
#but first rename columns 
colnames(census_2011_controls) <- c("country", "reg_code", "reg", "avgeduyrs", 
                                    "res_turn", "sin_par_hh", "unemp_rate")

merged_census_2011 <- merge(census_full_11, census_2011_controls, by = "reg_code", 
                            all.x = TRUE)

#make variables numeric 
merged_census_2011 <- merged_census_2011 %>%
  mutate_at(vars(res_turn, avgeduyrs, sin_par_hh, unemp_rate), as.numeric)


#---------------------------------Cleaning ESS 1--------------------------------
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
                     ess1_iea, ess1_nla, ess1_noa, ess1_pla, ess1_pta, 
                     ess1_sea)

ess1_a_subset <- ess1_a_full 

#subset to include only the variables you want 
#ess1_a_subset <- ess1_a_full[, c("name", "essround", "edition", "proddate", 
# "idno", "cntry", "reg_code","dweight", "pspwght", 
#"pweight", "anweight", "regionat", "regioach", 
#"regioncz", "regiondk", "regiones", "regionfr", 
#"regionhu", "regionie", "regionnl", "regionno", 
#"regionpl", "regionpt", "regionse", "ppltrst", 
#"pplfair", "pplhlp", "lrscale", "stflife", 
# "stfeco", "dclmig", "aesfdrk", "ctzcntr", 
# "ctzship", "brncntr", "livecntr", "trstlgl", 
# "trstplc", "trstplt", "trstprl", "imsmetn", 
# "imdfetn", "qfimwht", "qfimcmt", "qfimlng", "imueclt", 
# "idetalv", "pplstrd", "imgfrnd", "imgclg", 
# "yrlvdae", "empl", "gndr", "agea", "domicil", 
# "eduyrs", "hinctnt", "lvgptn", "imptrad", 
# "crmvct", "imbgeco", "dscrgrp", "dscrrce", 
# "dscrntn", "dscrlng", "dscretn", "blgetmg")]

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
ess1_a_subset$qfimchr <- as.numeric(ess1_a_subset$qfimchr)
ess1_a_subset$imtcjob <- as.numeric(ess1_a_subset$imtcjob)
ess1_a_subset$imbleco <- as.numeric(ess1_a_subset$imbleco)
ess1_a_subset$imwbcnt <- as.numeric(ess1_a_subset$imwbcnt)
ess1_a_subset$imwbcrm <- as.numeric(ess1_a_subset$imwbcrm)
ess1_a_subset$imdetbs <- as.numeric(ess1_a_subset$imdetbs)
ess1_a_subset$imsetbs <- as.numeric(ess1_a_subset$imsetbs)
ess1_a_subset$imdetmr <- as.numeric(ess1_a_subset$imdetmr)
ess1_a_subset$comnlng <- as.numeric(ess1_a_subset$comnlng)
ess1_a_subset$rlgoptp <- as.numeric(ess1_a_subset$rlgoptp)
ess1_a_subset$imprlg <- as.numeric(ess1_a_subset$imprlg)
ess1_a_subset$dvrcdev <- as.numeric(ess1_a_subset$dvrcdev)
ess1_a_subset$ipudrst <- as.numeric(ess1_a_subset$ipudrst)
ess1_a_subset$rlgdgr <- as.numeric(ess1_a_subset$rlgdgr)
ess1_a_subset$facntr <- as.character(ess1_a_subset$facntr)
ess1_a_subset$mocntr <- as.character(ess1_a_subset$mocntr)


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

ess1_a_subset <- ess1_a_subset %>%
  mutate(dclmig = case_when(
    dclmig == "1" ~ "International Level",
    dclmig == "2" ~ "European Level",
    dclmig == "3" ~ "National Level",
    dclmig == "4" ~ "Regional or Local Level",
    TRUE ~ dclmig  # Keep original value if it doesn't match any condition
  ))

#feeling of safety of walking alone in local area after dark 
ess1_a_subset$aesfdrk <- case_match(ess1_a_subset$aesfdrk, "1" ~ "Very safe", 
                                "2" ~ "Safe", "3" ~ "Unsafe", 
                                "4" ~ "Very unsafe")

#Citizen of reporting country (dummy variable)
ess1_a_subset$ctzcntr <- ifelse(ess1_a_subset$ctzcntr == 1, "Yes", "No")

#born in the country (dummy variable)
ess1_a_subset$brncntr <- ifelse(ess1_a_subset$brncntr == 1, "Yes", "No")

#How long ago first came to live in country 
ess1_a_subset$livecntr <- case_match(ess1_a_subset$livecntr, "1" ~ "<1", 
                                     "2" ~ "1-5", 
                                 "3" ~ "6-10", "4" ~ "11-20", "5" ~ ">20")

#People of minority race/ethnic group in ideal living area 
ess1_a_subset$idetalv <- case_match(ess1_a_subset$idetalv, "1" ~ "Almost nobody", 
                                "2" ~ "Some", "3" ~ "Many", 
                                "4" ~ "It would make no difference")

#Immigrant Friends 
ess1_a_subset$imgfrnd <- case_match(ess1_a_subset$imgfrnd, "1" ~ "Yes, several", 
                                "2" ~ "Yes, a few", "3" ~ "No, none at all")


#Immigrant colleagues 
ess1_a_subset$imgclg <- case_match(ess1_a_subset$imgclg, "1" ~ "Yes, several", 
                               "2" ~ "Yes, a few", "3" ~ "No, none at all", 
                               "4" ~ "Not currently working")




#Employment status 
ess1_a_subset$empl <- case_match(ess1_a_subset$empl, "1" ~ "Employed", 
                             "2" ~ "Self-Employed", "3" ~ "Not in paid work")
#recode to a dummy variable 
ess1_a_subset$empl <- ifelse(ess1_a_subset$empl == "Employed" | 
                               ess1_a_subset$empl == "Self-Employed", "Employed", 
                             "Unemployed")

#gender
ess1_a_subset$gndr <- case_match(ess1_a_subset$gndr, "1" ~ "Male", "2" ~ "Female")

#respondent's description of their domicile 
ess1_a_subset$domicil <- case_match(ess1_a_subset$domicil, "1" ~ "A big city", 
                                "2" ~ "Suburbs or outskirts of a big city", 
                                "3" ~ "Town or small city", 
                                "4" ~ "Country village", 
                                "5" ~ "Farm home in countryside")

#not sure how to work with income yet, come back to it. 

#currently living with a partner 
ess1_a_subset$lvgptn <- case_match(ess1_a_subset$lvgptn, "1" ~ "Yes", "2" ~ "No")

#Respondent or hh member ever a victime of burglary/assault in the past 5 years 
ess1_a_subset$crmvct <- case_match(ess1_a_subset$crmvct, "1" ~ "Yes", "2" ~ "No")

#second generation (i.e., either father or mother not born in country, but 
#respondent born in country)
ess1_a_subset <- ess1_a_subset %>%
  mutate(secgen = case_when(
    facntr == "2" | mocntr == "2" & brncntr == "Yes" & ctzcntr == "Yes" 
    ~ "Yes", 
    TRUE ~ "No"
  ))


#Naturalised, more than 10 years in country (i.e., not born in the country, 
#but a citizen and has lived here more than 10 years. )
ess1_a_subset <- ess1_a_subset %>% 
  mutate(nat10 = case_when(
    brncntr == "No" & ctzcntr == "Yes" & livecntr == "11-20" | 
      livecntr == ">20" ~ "Yes", 
    TRUE ~ "No"
  ))


#Naturalised, less than 10 years in country (i.e., not born in the country, 
#but a citizen and has lived here less than 10 years)
ess1_a_subset <- ess1_a_subset %>% 
  mutate(natless10 = case_when(
    brncntr == "No" & ctzcntr == "Yes" & livecntr == "<1" | 
      livecntr == "1-5" | livecntr == "6-10" ~ "Yes", 
    TRUE ~ "No"
  ))

#Non-citizen, more than 10 years in country
ess1_a_subset <- ess1_a_subset %>% 
  mutate(nonctz10 = case_when(
    ctzcntr == "No" & livecntr == "11-20" | 
      livecntr == ">20" ~ "Yes", 
    TRUE ~ "No"
  ))

#Non-citizen, less than 10 years in country 
ess1_a_subset <- ess1_a_subset %>% 
  mutate(nonctzless10 = case_when(
    ctzcntr == "No" & livecntr == "<1" | 
      livecntr == "1-5" | livecntr == "6-10" ~ "Yes", 
    TRUE ~ "No"
  ))

#Native: both parents born in country, born in country, and citizen 
ess1_a_subset <- ess1_a_subset %>%
  mutate(native = case_when(
    facntr == "1" | mocntr == "1" & brncntr == "Yes" & ctzcntr == "Yes" 
    ~ "Yes", 
    TRUE ~ "No"
  ))


#recode respondent income: collapse into 8 bins as opposed to the current 12. 

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

unique(ess1_a_subset$hinctnt)


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

ess1_a_subset$dscrgrp <- case_match(ess1_a_subset$dscrgrp, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#discrimination on the basis of colour or race 
ess1_a_subset$dscrrce <- case_match(ess1_a_subset$dscrrce, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#discrimination on the basis of nationality 
ess1_a_subset$dscrntn <- case_match(ess1_a_subset$dscrntn, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#discrimination on the basis of language
ess1_a_subset$dscrlng <- case_match(ess1_a_subset$dscrlng, "0" ~ "Not marked", 
                                "1" ~ "Marked")


#discrimination on the basis of ethnicity 
ess1_a_subset$dscretn <- case_match(ess1_a_subset$dscretn, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#self-identification with belonging to minority ethnic group in country 
ess1_a_subset$blgetmg <- case_match(ess1_a_subset$blgetmg, "1" ~ "Yes", 
                                "2" ~ "No")


png(file = "Dissertation GitHub/figures/corrplot1.png",
    width = 6000, height = 4000, res = 650)

corrplot.mixed(
  cor(ess1_a_subset[, c("qfimwht", "qfimcmt",
                        "qfimlng", "pplstrd", "comnlng")],
      use="complete.obs"),
  upper="ellipse",tl.cex=0.3)

dev.off()


ess1_IRT1_subset <- ess1_a_subset[complete.cases(ess1_a_subset[,
                               c("qfimwht", "qfimcmt",
                               "qfimlng", "pplstrd",  
                               "qfimchr")]),]

png(file = "Dissertation GitHub/figures/histIRT1.png",
    width = 6000, height = 4000, res = 650)
hist(rowSums(ess1_IRT1_subset[, c("qfimwht", 
                                  "qfimcmt",
                                  "qfimlng", "pplstrd", "qfimchr"
)]),
xlab="IDK",main="", br=seq(1,45,1), freq=FALSE)
dev.off()

#okay how about you first rescale everything to be on a scale of 1-5
#nvm don't do that, you don't need to. 


range(rowSums(ess1_IRT1_subset[, c("qfimwht", 
                                   "qfimcmt",
                                   "qfimlng", "pplstrd", "qfimchr"
)]))


#Trying ordinal IRT
grm_fit <- grm(ess1_IRT1_subset[, c("qfimwht", 
                                    "qfimcmt",
                                    "qfimlng", "pplstrd", "qfimchr"
)])
grm_fit
#par(mfrow=c(3,2))
#plot(grm_fit)

grm_scores <- factor.scores.grm(grm_fit,resp.patterns=ess1_IRT1_subset[, 
                       c("qfimwht", 
                       "qfimcmt",
                       "qfimlng", "pplstrd", "qfimchr")])
out2 <- data.frame(scores2 = grm_scores$score.dat$z1,cumulative_response = 
                     rowSums(ess1_IRT1_subset[, 
                                              c("qfimwht", 
                                                "qfimcmt",
                                                "qfimlng", "pplstrd", "qfimchr")]), 
                   idno = ess1_IRT1_subset$idno)




# BaseR
# plot(jitter(out2$num_correct),out2$scores2,
#      xlab="Correct Responses",
#      ylab="Ordered IRT Score",
#      pch=16,col=rgb(0,0,0,0.25))

# ggplot2

png(file = "Dissertation GitHub/figures/IRT1plot1.png",
    width = 6000, height = 4000, res = 650)
ggplot(out2, aes(x=jitter(cumulative_response),y=scores2)) +
  geom_point(size=2,alpha=.5) +
  scale_x_continuous("Cumulative Responses",breaks = seq(1,45,2)) + 
  scale_y_continuous("Ordered IRT Score",breaks = seq(-3,3,0.5)) +
  theme_clean() +
  theme(plot.background = element_rect(color=NA))
dev.off()

ess1_b <- ess1_IRT1_subset
ess1_b$IRTscores <- grm_scores$score.dat$z1
ess1_b$cumulative_response <- rowSums(ess1_IRT1_subset[, 
                                                       c("qfimwht", 
                                                         "qfimcmt",
                                                         "qfimlng", "pplstrd", 
                                                         "qfimchr")])



range(out2$scores2)


#check how th IRT scores correlate with willingness to allow migrants of different 
#ethnicities
#correlates positively, which given the scale implies that it is predictive of 
#anti-immigration attitudes. As we would expect. 
m1 <- lm(ess1_IRT1_subset$imdfetn ~ out2$scores2) 
summary(m1)


#same for migrants of the same ethnicity. Still positively correlated indicating 
#that it is predictive of anti-immigrant attitudes overall, but it is less positively 
#correlated than for the measure about allowing migrants of different ethnicities. 
m2 <- lm(ess1_IRT1_subset$imsmetn ~ out2$scores2)
summary(m2)

#negatively correlated with importance of tradition, which implies that it predicts
#more traditionalist attitudes. 
m3 <- lm(ess1_IRT1_subset$imptrad ~ out2$scores2)
summary(m3)


ess1_final <- merge(ess1_b, merged_census_2001, by = "reg_code", all.x = TRUE)


#--------------------------Cleaning ESS 7---------------------------------------
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
ess7_a_subset$facntr <- as.character(ess7_a_subset$facntr)
ess7_a_subset$mocntr <- as.character(ess7_a_subset$mocntr)


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
ess7_a_subset$crmvct <- case_match(ess7_a_subset$crmvct, "1" ~ "Yes", 
                               "2" ~ "No")

#Feeling of safety walking alone in local area after dark 
ess7_a_subset$aesfdrk <- case_match(ess7_a_subset$aesfdrk, "1" ~ "Very safe", 
                                "2" ~ "Safe", "3" ~ "Unsafe", 
                                "4" ~ "Very unsafe")

#Member of group discriminated against in this country (individual's response)
ess7_a_subset$dscrgrp <- case_match(ess7_a_subset$dscrgrp, "1" ~ "Yes", 
                                "2" ~ "No")

#Discrimination of respondent's group: colour or race
ess7_a_subset$dscrrce <- case_match(ess7_a_subset$dscrrce, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#Discrimination of respondent's group: nationality 
ess7_a_subset$dscrntn <- case_match(ess7_a_subset$dscrntn, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#Discrimination of respondent's group: language 
ess7_a_subset$dscrlng <- case_match(ess7_a_subset$dscrlng, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#Discrimination of respondent's group: ethnicity 
ess7_a_subset$dscretn <- case_match(ess7_a_subset$dscretn, "0" ~ "Not marked", 
                                "1" ~ "Marked")

#citizen of country 
ess7_a_subset$ctzcntr <- case_match(ess7_a_subset$ctzcntr, "1" ~ "Yes", 
                                "2" ~ "No")

#born in the country 
ess7_a_subset$brncntr <- case_match(ess7_a_subset$brncntr, "1" ~ "Yes", 
                                "2" ~ "No")

#belonging to minority ethnic group in country 
ess7_a_subset$blgetmg <- case_match(ess7_a_subset$blgetmg, "1" ~ "Yes", 
                                "2" ~ "No")

#Different race or ethnic group: have any close friends 
ess7_a_subset$dfegcf <- case_match(ess7_a_subset$dfegcf, "1" ~ "Yes, several", 
                               "2" ~ "Yes, a few", "3" ~ "No, none at all")

#different race or ethnic group: contact, how often 
ess7_a_subset$dfegcon <- case_match(ess7_a_subset$dfegcon, "1" ~ "Never", 
                                "2" ~ "Less than once a month", 
                                "3" ~ "Once a month", 
                                "4" ~ "Several times a month", 
                                "5" ~ "Once a week", 
                                "6" ~ "Several times a week", 
                                "7" ~ "Every day")

class(ess7_a_subset$facntr)


#second generation (i.e., either father or mother not born in country, but 
#respondent born in country)
ess7_a_subset <- ess7_a_subset %>%
  mutate(secgen = case_when(
    facntr == "2" | mocntr == "2" & brncntr == "Yes" & ctzcntr == "Yes" 
    ~ "Yes", 
    TRUE ~ "No"
  ))

#Naturalised, more than 10 years in country (i.e., not born in the country, 
#but a citizen and has lived here more than 10 years. )
ess7_a_subset <- ess7_a_subset %>% 
  mutate(nat10 = case_when(
    brncntr == "No" & ctzcntr == "Yes" & livecnta < 2004 ~ "Yes", 
    TRUE ~ "No"
  ))

#Naturalised, less than 10 years in country (i.e., not born in the country, 
#but a citizen and has lived here less than 10 years)
ess7_a_subset <- ess7_a_subset %>% 
  mutate(natless10 = case_when(
    brncntr == "No" & ctzcntr == "Yes" & livecnta > 2004 ~ "Yes", 
    TRUE ~ "No"
  ))

#Non-citizen, more than 10 years in country
ess7_a_subset <- ess7_a_subset %>% 
  mutate(nonctz10 = case_when(
    ctzcntr == "No" & livecnta < 2004 ~ "Yes", 
    TRUE ~ "No"
  ))

#Non-citizen, less than 10 years in country 
ess7_a_subset <- ess7_a_subset %>% 
  mutate(nonctzless10 = case_when(
    ctzcntr == "No" & livecnta > 2004 ~ "Yes", 
    TRUE ~ "No"
  ))


#Native (both parents born in country, born in country)
ess7_a_subset <- ess7_a_subset %>%
  mutate(native = case_when(
    facntr == "1" | mocntr == "1" & brncntr == "Yes" & ctzcntr == "Yes" 
    ~ "Yes", 
    TRUE ~ "No"
  ))


#some cultures: much better or all equal 
ess7_a_subset$smctmbe <- case_match(ess7_a_subset$smctmbe, 
                                "1" ~ "Some cultures are much better than others", 
                                "2" ~ "All cultures are equal")

#gender 
ess7_a_subset$gndr <- case_match(ess7_a_subset$gndr, "1" ~ "Male", 
                             "2" ~ "Female")

#domicile (respondent's description)
ess7_a_subset$domicil <- case_match(ess7_a_subset$domicil, "1" ~ "A big city", 
                                "2" ~ "Suburbs or outskirts of a big city", 
                                "3" ~ "Town or small city", 
                                "4" ~ "Country village", 
                                "5" ~ "Farm or home in countryside")

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

unique(ess7_a_subset$hinctnta)

#create a variable for employment. if pdwrk == 1, then employed, if uempla == 1 
#or uempli == 1, unemployed 

ess7_a_subset$empl <- ifelse(ess7_a_subset$pdwrk == "0" | 
                               ess7_a_subset$uempla == "1" | 
                               ess7_a_subset$uempli == "1", "Unemployed", 
                             ifelse(ess7_a_subset$pdwrk == "1", "Employed", 
                                    NA))


ess7_a_subset$icpart2 <- case_match(ess7_a_subset$icpart2, "1" ~ "Yes", 
                                "2" ~ "No")

#subset for IRT
ess7_IRT1_subset <- ess7_a_subset[complete.cases(ess7_a_subset[,
                      c("qfimwht", "qfimcmt",
                      "qfimlng", "pplstrd", "qfimchr"
                       )]),]

range(rowSums(ess7_IRT1_subset[, c("qfimwht", 
                                   "qfimcmt",
                                   "qfimlng", "pplstrd", "qfimchr")]))


png(file = "Dissertation GitHub/figures/histIRT2.png",
    width = 6000, height = 4000, res = 650)
hist(rowSums(ess7_IRT1_subset[, c("qfimwht", 
                                  "qfimcmt",
                                  "qfimlng", "pplstrd", 
                                  "qfimchr")]), xlab="IDK",main="",
     br=seq(1,45,1), freq=FALSE)
dev.off()

#Trying ordinal IRT
grm_fit7 <- grm(ess7_IRT1_subset[, c("qfimwht", 
                                     "qfimcmt",
                                     "qfimlng", "pplstrd", "qfimchr")])
grm_fit7
#par(mfrow=c(2,2))
#plot(grm_fit7)

grm_scores7 <- factor.scores.grm(grm_fit7,resp.patterns=ess7_IRT1_subset[, 
                                 c("qfimwht", 
                                 "qfimcmt",
                                 "qfimlng", "pplstrd", "qfimchr")])

out7 <- data.frame(scores7 = grm_scores7$score.dat$z1,cumulative_response = 
                     rowSums(ess7_IRT1_subset[, 
                                              c("qfimwht", 
                                                "qfimcmt",
                                                "qfimlng", "pplstrd", 
                                                "qfimchr")]), 
                   idno = ess7_IRT1_subset$idno)

range(out7$scores7)

# ggplot2

png(file = "Dissertation GitHub/figures/IRT2plot2.png",
    width = 6000, height = 4000, res = 650)
ggplot(out7, aes(x=jitter(cumulative_response),y=scores7)) +
  geom_point(size=2,alpha=.5) +
  scale_x_continuous("Cumulative Responses",breaks = seq(1,45,2)) + 
  scale_y_continuous("Ordered IRT Score",breaks = seq(-3,3,0.5)) +
  theme_clean() +
  theme(plot.background = element_rect(color=NA))
dev.off()


#check how it correlates with other measures 

#correlates positively, which given the scale implies that it is predictive of 
#anti-immigration attitudes. As we would expect. 
m17 <- lm(ess7_IRT1_subset$imdfetn ~ out7$scores7) 
summary(m17)


#same for migrants of the same ethnicity. Still positively correlated indicating 
#that it is predictive of anti-immigrant attitudes overall, but it is less positively 
#correlated than for the measure about allowing migrants of different ethnicities. 
m27 <- lm(ess7_IRT1_subset$imsmetn ~ out7$scores7)
summary(m27)

#negatively correlated with importance of tradition, which implies that it predicts
#more traditionalist attitudes. 
m37 <- lm(ess7_IRT1_subset$imptrad ~ out7$scores7)
summary(m37)


ess7_b <- ess7_IRT1_subset
ess7_b$IRTscores <- grm_scores7$score.dat$z1
ess7_b$cumulative_response <- rowSums(ess7_IRT1_subset[, 
                                                       c("qfimwht", 
                                                         "qfimcmt",
                                                         "qfimlng", "pplstrd", 
                                                         "qfimchr")])

#merge with census data
ess7_final <- merge(ess7_b, merged_census_2011, by = "reg_code", all.x = TRUE)


#----------------------------Descriptive Statistics-----------------------------

#------first grpahs of ethnic heterogeneity
#load shapefile data 
eur_shp <- st_read("Desktop/PPE/Diss/NUTS_RG_20M_2006_3035.shp/NUTS_RG_20M_2006_3035.shp")

nrow(eur_shp$CNTR_CODE == "DK")


#subset shapefile to only have countries you need
eur_shp <- eur_shp[eur_shp$CNTR_CODE %in% c("AT", "CZ", "DK", 
                                            "FR", "IE", "NL", 
                                            "NO", "PL", "PT", 
                                            "ES", "SE", "CH"), ]
#also subset further to only include nuts2 regions for all countries except france
#where it should be nuts1
eur_shp <- eur_shp[ifelse(eur_shp$CNTR_CODE == "FR", 
                          eur_shp$LEVL_CODE == 1, 
                          eur_shp$LEVL_CODE == 2), ]
colnames(eur_shp)[1] <- "reg_code"

#Split into countries 

#austria
at_shp <- eur_shp[eur_shp$CNTR_CODE == "AT", ]

#czechia 
cz_shp <- eur_shp[eur_shp$CNTR_CODE == "CZ", ]

#denmark 
dk_shp <- eur_shp[eur_shp$CNTR_CODE == "DK", ]

#france 
fr_shp <- eur_shp[eur_shp$CNTR_CODE == "FR", ]

#ireland 
ie_shp <- eur_shp[eur_shp$CNTR_CODE == "IE", ]

#netherlands 
nl_shp <- eur_shp[eur_shp$CNTR_CODE == "NL", ]

#norway 
no_shp <- eur_shp[eur_shp$CNTR_CODE == "NO", ]

#poland 
pl_shp <- eur_shp[eur_shp$CNTR_CODE == "PL", ]

#portugal 
pt_shp <- eur_shp[eur_shp$CNTR_CODE == "PT", ]

#spain 
es_shp <- eur_shp[eur_shp$CNTR_CODE == "ES", ]

#sweden 
se_shp <- eur_shp[eur_shp$CNTR_CODE == "SE", ]

#switzerland 
ch_shp <- eur_shp[eur_shp$CNTR_CODE == "CH", ]


#just recode spain's code to combine ceuta and mellila 

es_shp$reg_code <- ifelse(es_shp$reg_code == "ES64", "ES63", es_shp$reg_code)

#split census data by country 

#austria 
at_census_2001 <- merged_census_2001[1:9, ]

#czechia 
cz_census_2001 <- merged_census_2001[17:24, ]

#denmark 
dk_census_2001 <- merged_census_2001[25:29, ]

#france 
fr_census_2001 <- merged_census_2001[48:55, ]

#ireland 
ie_census_2001 <- merged_census_2001[56:57, ]

#netherlands 
nl_census_2001 <- merged_census_2001[58:69, ]

#norway 
no_census_2001 <- merged_census_2001[70:76, ]

#poland 
pl_census_2001 <- merged_census_2001[77:91, ]

#portugal 
pt_census_2001 <- merged_census_2001[92:98, ]

#spain 
es_census_2001 <- merged_census_2001[30:47, ]

#sweden 
se_census_2001 <- merged_census_2001[99:106, ]

#switzerland 
ch_census_2001 <- merged_census_2001[10:16, ]

#merge shapefiles with census 2001

at_merged_shp_cen1 <- merge(at_shp, at_census_2001, by = "reg_code")

cz_merged_shp_cen1 <- merge(cz_shp, cz_census_2001, by = "reg_code")

dk_merged_shp_cen1 <- merge(dk_shp, dk_census_2001, by = "reg_code")

fr_merged_shp_cen1 <- merge(fr_shp, fr_census_2001, by = "reg_code")

ie_merged_shp_cen1 <- merge(ie_shp, ie_census_2001, by = "reg_code")

nl_merged_shp_cen1 <- merge(nl_shp, nl_census_2001, by = "reg_code")

no_merged_shp_cen1 <- merge(no_shp, no_census_2001, by = "reg_code")

pl_merged_shp_cen1 <- merge(pl_shp, pl_census_2001, by = "reg_code")

pt_merged_shp_cen1 <- merge(pt_shp, pt_census_2001, by = "reg_code")

se_merged_shp_cen1 <- merge(se_shp, se_census_2001, by = "reg_code")

es_merged_shp_cen1 <- merge(es_shp, es_census_2001, by = "reg_code")

ch_merged_shp_cen1 <- merge(ch_shp, ch_census_2001, by = "reg_code")

#overall 
eur_shp_merged_cen1 <- merge(eur_shp, merged_census_2001, by = "reg_code")

#do the same for 2011 census data 
#austria 
at_census_2011 <- merged_census_2011[1:9, ]

#czechia 
cz_census_2011 <- merged_census_2011[17:24, ]

#denmark 
dk_census_2011 <- merged_census_2011[25:29, ]

#france 
fr_census_2011 <- merged_census_2011[48:55, ]

#ireland 
ie_census_2011 <- merged_census_2011[56:57, ]

#netherlands 
nl_census_2011 <- merged_census_2011[58:69, ]

#norway 
no_census_2011 <- merged_census_2011[70:76, ]

#poland 
pl_census_2011 <- merged_census_2011[77:91, ]

#portugal 
pt_census_2011 <- merged_census_2011[92:98, ]

#spain 
es_census_2011 <- merged_census_2011[30:47, ]

#sweden 
se_census_2011 <- merged_census_2011[99:106, ]

#switzerland 
ch_census_2011 <- merged_census_2011[10:16, ]

#merge shapefiles with census 2011

at_merged_shp_cen11 <- merge(at_shp, at_census_2011, by = "reg_code")

cz_merged_shp_cen11 <- merge(cz_shp, cz_census_2011, by = "reg_code")

dk_merged_shp_cen11 <- merge(dk_shp, dk_census_2011, by = "reg_code")

fr_merged_shp_cen11 <- merge(fr_shp, fr_census_2011, by = "reg_code")

ie_merged_shp_cen11 <- merge(ie_shp, ie_census_2011, by = "reg_code")

nl_merged_shp_cen11 <- merge(nl_shp, nl_census_2011, by = "reg_code")

no_merged_shp_cen11 <- merge(no_shp, no_census_2011, by = "reg_code")

pl_merged_shp_cen11 <- merge(pl_shp, pl_census_2011, by = "reg_code")

pt_merged_shp_cen11 <- merge(pt_shp, pt_census_2011, by = "reg_code")

se_merged_shp_cen11 <- merge(se_shp, se_census_2011, by = "reg_code")

es_merged_shp_cen11 <- merge(es_shp, es_census_2011, by = "reg_code")

ch_merged_shp_cen11 <- merge(ch_shp, ch_census_2011, by = "reg_code")

#overall 
eur_shp_merged_cen11 <- merge(eur_shp, merged_census_2011, by = "reg_code")

#Austria plots, 2001 and 2011

at_hhi_plot_2001 <- ggplot() +
  geom_sf(at_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



at_hhi_plot_2011 <- ggplot() +
  geom_sf(at_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



at_eth_frac_plots <- ggarrange(at_hhi_plot_2001, at_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
          common.legend = TRUE, legend = "bottom", labels = 
            "Ethnic Fractionalisation by NUTS 2 Region in Austria", 
          hjust = -0.45, 
          font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/ATEthFracDes1&11.png", 
    width = 4000, height = 3000, res = 650)

at_eth_frac_plots

dev.off()

#Czechia plots, 2001 and 2011

cz_hhi_plot_2001 <- ggplot() +
  geom_sf(cz_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



cz_hhi_plot_2011 <- ggplot() +
  geom_sf(cz_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



cz_eth_frac_plots <- ggarrange(cz_hhi_plot_2001, cz_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Region in Czechia", 
                               hjust = -0.45, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/CZEthFracDes1&11.png", 
    width = 4500, height = 3000, res = 650)

cz_eth_frac_plots

dev.off()


#Denmark plots, 2001 and 2011

dk_hhi_plot_2001 <- ggplot() +
  geom_sf(dk_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



dk_hhi_plot_2011 <- ggplot() +
  geom_sf(dk_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



dk_eth_frac_plots <- ggarrange(dk_hhi_plot_2001, dk_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Region in Denmark", 
                               hjust = -0.45, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/DKEthFracDes1&11.png", 
    width = 4500, height = 3000, res = 650)

dk_eth_frac_plots

dev.off()

#France plots, 2001 and 2011

fr_hhi_plot_2001 <- ggplot() +
  geom_sf(fr_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



fr_hhi_plot_2011 <- ggplot() +
  geom_sf(fr_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



fr_eth_frac_plots <- ggarrange(fr_hhi_plot_2001, fr_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 1 Region in France", 
                               hjust = -0.45, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/FREthFracDes1&11.png", 
    width = 4500, height = 3000, res = 650)

fr_eth_frac_plots

dev.off()

#Ireland plots, 2001 and 2011

ie_hhi_plot_2001 <- ggplot() +
  geom_sf(ie_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



ie_hhi_plot_2011 <- ggplot() +
  geom_sf(ie_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



ie_eth_frac_plots <- ggarrange(ie_hhi_plot_2001, ie_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Region in Ireland", 
                               hjust = -0.82, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/IEEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

ie_eth_frac_plots

dev.off()

#Netherlands plots, 2001 and 2011

nl_hhi_plot_2001 <- ggplot() +
  geom_sf(nl_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



nl_hhi_plot_2011 <- ggplot() +
  geom_sf(nl_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



nl_eth_frac_plots <- ggarrange(nl_hhi_plot_2001, nl_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in the Netherlands", 
                               hjust = -0.7, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/NLEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

nl_eth_frac_plots

dev.off()


#Norway plots, 2001 and 2011

no_hhi_plot_2001 <- ggplot() +
  geom_sf(no_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



no_hhi_plot_2011 <- ggplot() +
  geom_sf(no_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



no_eth_frac_plots <- ggarrange(no_hhi_plot_2001, no_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Norway", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/NOEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

no_eth_frac_plots

dev.off()

#Poland plots, 2001 and 2011

pl_hhi_plot_2001 <- ggplot() +
  geom_sf(pl_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



pl_hhi_plot_2011 <- ggplot() +
  geom_sf(pl_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



pl_eth_frac_plots <- ggarrange(pl_hhi_plot_2001, pl_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Poland", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/PLEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

pl_eth_frac_plots

dev.off()

#Portugal plots, 2001 and 2011

pt_hhi_plot_2001 <- ggplot() +
  geom_sf(pt_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



pt_hhi_plot_2011 <- ggplot() +
  geom_sf(pt_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



pt_eth_frac_plots <- ggarrange(pt_hhi_plot_2001, pt_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Portugal", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/PTEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

pt_eth_frac_plots

dev.off()

#Spain plots, 2001 and 2011

es_hhi_plot_2001 <- ggplot() +
  geom_sf(es_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



es_hhi_plot_2011 <- ggplot() +
  geom_sf(es_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



es_eth_frac_plots <- ggarrange(es_hhi_plot_2001, es_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Spain", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/ESEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

es_eth_frac_plots

dev.off()

#Sweden plots, 2001 and 2011

se_hhi_plot_2001 <- ggplot() +
  geom_sf(se_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



se_hhi_plot_2011 <- ggplot() +
  geom_sf(se_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



se_eth_frac_plots <- ggarrange(se_hhi_plot_2001, se_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Sweden", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/SEEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

se_eth_frac_plots

dev.off()

#Switzerland plots, 2001 and 2011

ch_hhi_plot_2001 <- ggplot() +
  geom_sf(ch_merged_shp_cen1, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2001") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



ch_hhi_plot_2011 <- ggplot() +
  geom_sf(ch_merged_shp_cen11, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Blues", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "2011") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))



ch_eth_frac_plots <- ggarrange(ch_hhi_plot_2001, ch_hhi_plot_2011, 
                               nrow = 1, ncol = 2, 
                               common.legend = TRUE, legend = "bottom", labels = 
                                 "Ethnic Fractionalisation by NUTS 2 Regions in Switzerland", 
                               hjust = -0.8, 
                               font.label = list(size = 10, face = "bold"))


png(file = "Dissertation Github/figures/CHEthFracDes1&11.png", 
    width = 6000, height = 4000, res = 650)

ch_eth_frac_plots

dev.off()

png(file = "Dissertation Github/figures/EthFrac.png", width = 10000, height = 8000, 
    res = 650)
ggarrange(at_eth_frac_plots, cz_eth_frac_plots, dk_eth_frac_plots, fr_eth_frac_plots, 
          ie_eth_frac_plots, nl_eth_frac_plots, no_eth_frac_plots, pl_eth_frac_plots, 
          pt_eth_frac_plots, es_eth_frac_plots, se_eth_frac_plots, ch_eth_frac_plots, 
          ncol = 3, nrow = 4, labels = "Ethnic Fractionalisation in 2001 and 2011", 
         font.label = list(size = 14, face = "bold"))
dev.off()

#------------------------------Analysis-----------------------------------------

#first, subset ess1 and ess7 to have the same variables to rbind. 

ess1_final_subset <- ess1_final[, c("name", "essround", "edition", "proddate", 
                                    "idno", "cntry", "reg_code", "dweight", 
                                    "pspwght", "pweight", "anweight", 
                                    "lrscale", "stflife", "stfeco", "aesfdrk", 
                                    "ctzcntr", "imsmetn", "imdfetn", "imueclt", 
                                    "empl", "gndr", "agea", "imptrad", 
                                    "crmvct", "imbgeco", "dscrrce", "dscrntn", 
                                    "dscrlng", "dscretn", "blgetmg", "soc_trst", 
                                    "ins_trst", "secgen", "nat10", "natless10", 
                                    "nonctz10", "nonctzless10", "native", 
                                    "IRTscores", "cumulative_response", 
                                    "HHI", "Eth_Frac", "avgeduyrs", "res_turn", 
                                    "sin_par_hh", "unemp_rate", "domicil", 
                                    "eduyrs", "hinctnt", "lvgptn", "imtcjob", 
                                    "imbleco", "imwbcnt", "imwbcrm", "dvrcdev", 
                                    "rlgdgr", "uemp5yr")]

ess7_final_subset <- ess7_final[, c("name", "essround", "edition", "proddate", 
                                    "idno", "cntry", "reg_code", "dweight", 
                                    "pspwght", "pweight", "anweight", 
                                    "lrscale", "stflife", "stfeco", "aesfdrk",
                                    "ctzcntr", "imsmetn", "imdfetn", "imueclt", 
                                    "empl", "gndr", "agea", "imptrad", "crmvct", 
                                    "imbgeco", "dscrrce", "dscrntn", 
                                    "dscrlng", "dscretn", "blgetmg", "soc_trst", 
                                    "ins_trst", "secgen", "nat10", "natless10", 
                                    "nonctz10", "nonctzless10", "native", 
                                    "IRTscores", "cumulative_response", 
                                    "HHI", "Eth_Frac", "avgeduyrs", "res_turn", 
                                    "sin_par_hh", "unemp_rate", "domicil", 
                                    "eduyrs", "hinctnta", "icpart2", "imtcjob", 
                                    "imbleco", "imwbcnt", "imwbcrm", "dvrcdeva", 
                                    "rlgdgr", "uemp5yr")]



colnames(ess7_final_subset)[49] <- "hinctnt"
colnames(ess7_final_subset)[50] <- "lvgptn"
colnames(ess7_final_subset)[55] <- "dvrcdev"

#rbind both the datasets together 

ess_complete <- rbind(ess1_final_subset, ess7_final_subset)
ess_complete$essround <- as.character(ess_complete$essround)

#run a null model to calculate intra-class correlation 
mnull <- lmer(soc_trst ~ 1 + (1|reg_code), data = ess_complete)
summary(mnull)

#icc = 0.82/3.7 = 0.22. > 0.1, so a hierarchical model makes sense. 

#first, before running any actual models, mean centre all the controls that 
#are continuous 

ess_complete$Eth_Frac_mc <- scale(ess_complete$Eth_Frac, scale = FALSE)

ess_complete$res_turn_mc <- scale(ess_complete$res_turn, scale = FALSE)

ess_complete$sin_par_hh_mc <- scale(ess_complete$sin_par_hh, scale = FALSE)

ess_complete$unemp_rate_mc <- scale(ess_complete$unemp_rate, scale = FALSE)

ess_complete$eduyrs_mc <- scale(ess_complete$eduyrs, scale = FALSE)

ess_complete$avgeduyrs_mc <- scale(ess_complete$avgeduyrs, scale = FALSE)

#run a model without any controls, just ethnic fractionalisation on 
#social trust 

mbase <- lmer(soc_trst ~ Eth_Frac_mc + essround + (1|reg_code), 
              data = ess_complete)
summary(mbase)

#run a model with individual and contextual controls, but don't include IRT scores 
#yet. 

mcore <- lmer(soc_trst ~ Eth_Frac_mc + essround + hinctnt + eduyrs_mc + 
                avgeduyrs_mc + res_turn_mc + sin_par_hh_mc + unemp_rate_mc + 
                crmvct + lrscale + stflife + stfeco + empl + gndr + agea + 
                ins_trst + ctzcntr  + 
                lvgptn + (1 + Eth_Frac_mc|reg_code), 
              data = ess_complete)
summary(mcore)


#run a base model without controls, but interacting IRT with eth frac 

mbaseIRT <- lmer(soc_trst ~ Eth_Frac_mc*IRTscores + essround + 
                   (1 + Eth_Frac_mc*IRTscores|reg_code), 
                 data = ess_complete)
summary(mbaseIRT)

# Create a control object with the specified optimizer
control <- lmerControl(optimizer = "optim")

mcoreIRT <- lmer(soc_trst ~ Eth_Frac_mc*IRTscores + essround + hinctnt + eduyrs_mc + 
                   avgeduyrs_mc + res_turn_mc + sin_par_hh_mc + unemp_rate_mc + 
                   crmvct + stflife + stfeco + empl + gndr + agea + 
                   ins_trst + ctzcntr  + 
                   lvgptn + (1 + Eth_Frac_mc*IRTscores|reg_code), 
                 data = ess_complete, 
                 control = lmerControl(optimizer = "optimx", 
                                       optCtrl = list(method = "nlminb")))
summary(mcoreIRT)


coef(mcoreIRT)












