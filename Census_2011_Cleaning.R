library(haven)
library(readxl)
library(dplyr)
library(ltm)
library(corrplot)
library(ggplot2)
library(GGally)
library(ggthemes)

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

#switzerland 
switzerland_11 <- add_total_column(switzerland_11)

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





