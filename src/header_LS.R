library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)

###
# Choix de la nomenclature: Soit par produit par produit (pxp), soit industrie par industrie (ixi)
nom <- "pxp"

# Chemin d'accès du dossier où sont stockées les données sources issues d'EXIOBASE 3.8 (fichier txt) (Retrieved from:   https://zenodo.org/record/4277368 )
path_user <- str_c("C:/Users/leasy/")
path_github <- str_c(path_user,"Documents/GitHub/")
path_data.source <- str_c(path_user,"Documents/GitHub/EmissionsIndicators/data_in/")
#path_data.source <- str_c("data_in/IOT/")
path_codedata <- str_c(path_user,"Documents/GitHub/EmissionsIndicators/src/")

path_out <- str_c(path_codedata,"data.out/IOT_2015_pxp/")

# Intervalle des années pour extraction des données
year.min <-2010
year.max <- 2010
year <- 2015
# Choix du pays considéré pour calcul empreinte carbone 
iso <-  str_c("FR")

# Liste des GES considérés
glist <- c("CO2", "CH4", "N2O", "SF6", "HFC", "PFC")

