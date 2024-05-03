library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)



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













