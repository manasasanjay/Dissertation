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


