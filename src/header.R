library(tidyverse)
library(data.table)
library(openxlsx)

###
# Choix de la nomenclature: Soit par produit par produit (pxp), soit industrie par industrie (ixi)
nom <- "pxp"

# Chemin d'accès du dossier où sont stockées les données sources issues d'EXIOBASE 3.8 (fichier txt) (Retrieved from:   https://zenodo.org/record/4277368 )
path_user <- str_c("C:/Users/PMalliet/")
path_data.source <- str_c(path_user,"Dropbox (FOSEM)/Databases/EXIOBASE_3.8")
path_data.source <- str_c("data_in/IOT/")

path_out <- "data_out/"

# Intervalle des années pour extraction des données
year.min <-2010
year.max <- 2010
year <- 2015
# Choix du pays considéré pour calcul empreinte carbone 
iso <-  str_c("FR")

# Liste des GES considérés
glist <- c("CO2", "CH4", "N2O", "SF6", "HFC", "PFC")


# Descriptions of nomenclatures
source("data_in/desc/exio3.desc.R")
source("data_in/desc/CPA4.desc.R")
source("src/functions/01_load.bridge.R")
