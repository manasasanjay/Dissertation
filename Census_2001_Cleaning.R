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





