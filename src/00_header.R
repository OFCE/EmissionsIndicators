library(tidyverse)
library(openxlsx)

###

# Choix de la nomenclature: Soit par produit par produit (pxp), soit industrie par industrie (ixi)
nom <- "pxp"
# Choix de la date
year <- 2015
# Choix de la nomenclature finale
br ="ThreeME_c29" # produits/industries
# NOMENCLATURE GEOGRAPHIQUE (DATA_IN/BRIDGES/COUNTRIES): pays/régions
br.pays ="WD3"
# code pays pour lequel le calcul est effectué
iso = "FR"

# Liste des GES considérés dans le calcul des indicateurs carbone
glist <- c("CO2", "CH4", "N2O", "SF6", "HFC", "PFC")

iso_list <- c("US", "FR", "DE","ES", "IT","PL", "CN", "ID", "IN","BR", "WD")
###
source("src/utils.R")
source("src/desc.R")
## Multi run 
for(iso in iso_list){
  #run.script(nom, year, br, iso, PreLoader = T, Bridging = T)
}


## P