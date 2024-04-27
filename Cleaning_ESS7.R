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

ess7_ata <- ess1[ess1$cntry == "AT", ]







