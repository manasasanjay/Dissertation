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
library(sf)

#load shapefile data 
eur_shp <- st_read("Desktop/PPE/Diss/NUTS_RG_20M_2006_3035.shp/NUTS_RG_20M_2006_3035.shp")

nrow(eur_shp$CNTR_CODE == "DK")


#subset shapefile to only have countries you need
eur_shp <- eur_shp[eur_shp$CNTR_CODE %in% c("AT", "CZ", "DK", 
                                                         "FR", "IE", "NL", 
                                                         "NO", "PL", "PT", 
                                                         "ES", "SE", "CH"), ]
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
at_census_2001 <- census_full_1[1:9, ]

#czechia 
cz_census_2001 <- census_full_1[10:17, ]

#denmark 
dk_census_2001 <- census_full_1[18:22, ]

#france 
fr_census_2001 <- census_full_1[23:30, ]

#ireland 
ie_census_2001 <- census_full_1[31:32, ]

#netherlands 
nl_census_2001 <- census_full_1[33:44, ]

#norway 
no_census_2001 <- census_full_1[45:51, ]

#poland 
pl_census_2001 <- census_full_1[52:66, ]

#portugal 
pt_census_2001 <- census_full_1[67:73, ]

#spain 
es_census_2001 <- census_full_1[74:91, ]

#sweden 
se_census_2001 <- census_full_1[92:99, ]

#switzerland 
ch_census_2001 <- census_full_1[100:106, ]

#merge shapefiles with census 

at_merged_shp_cen <- merge(at_shp, at_census_2001, by = "reg_code")

cz_merged_shp_cen <- merge(cz_shp, cz_census_2001, by = "reg_code")

dk_merged_shp_cen <- merge(dk_shp, dk_census_2001, by = "reg_code")

fr_merged_shp_cen <- merge(fr_shp, fr_census_2001, by = "reg_code")

ie_merged_shp_cen <- merge(ie_shp, ie_census_2001, by = "reg_code")

nl_merged_shp_cen <- merge(nl_shp, nl_census_2001, by = "reg_code")

no_merged_shp_cen <- merge(no_shp, no_census_2001, by = "reg_code")

pl_merged_shp_cen <- merge(pl_shp, pl_census_2001, by = "reg_code")

pt_merged_shp_cen <- merge(pt_shp, pt_census_2001, by = "reg_code")

se_merged_shp_cen <- merge(se_shp, se_census_2001, by = "reg_code")

es_merged_shp_cen <- merge(es_shp, es_census_2001, by = "reg_code")

ch_merged_shp_cen <- merge(ch_shp, ch_census_2001, by = "reg_code")

#overall 
eur_shp_merged_cen <- merge(eur_shp, census_full_1, by = "reg_code")

#austria
png(file = "Dissertation Github/figures/ATEthFracDes2001.png", 
    width = 6000, height = 4000, res = 650)
ggplot() +
  geom_sf(at_merged_shp_cen, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Greys", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "Ethnic Fractionalisation by Region in Austria (2001)") + 
  theme(panel.grid = element_blank(),
  axis.text = element_blank(), 
  plot.title = element_text(hjust = 0.5), 
  legend.title = element_text(size = 10))
dev.off()

#czechia
png(file = "Dissertation Github/figures/CZEthFracDes2001.png", 
    width = 6000, height = 4000, res = 650)
ggplot() +
  geom_sf(cz_merged_shp_cen, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Greys", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "Ethnic Fractionalisation by Region in Czechia (2001)") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))
dev.off()



png(file = "Dissertation Github/figures/EurEthFracDes2001.png", 
    width = 6000, height = 4000, res = 650)
ggplot() +
  geom_sf(eur_shp_merged_cen, mapping = aes(fill = Eth_Frac)) + 
  scale_fill_distiller(type = "seq", palette = "Greys", direction = 1) + 
  theme_minimal() +
  labs(fill = "1 - Hirschman-Herfindahl Index",
       title = "Ethnic Fractionalisation by Region in Europe(2001)") + 
  theme(panel.grid = element_blank(),
        axis.text = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_text(size = 10))
dev.off()
































