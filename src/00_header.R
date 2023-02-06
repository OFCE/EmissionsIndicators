library(tidyverse)
library(openxlsx)


###
# Choix de la nomenclature: Soit par produit par produit (pxp), soit industrie par industrie (ixi)
nom <- "pxp"

# Choix de la date
year <- 2015

# Choix de la nomenclature finale
br ="ThreeME"
br.pays ="EU"


# Choix du pays considéré pour calcul empreinte carbone 
iso <-  str_c("EU")

# Liste des GES considérés
glist <- c("CO2", "CH4", "N2O", "SF6", "HFC", "PFC")

# Chemin d'accès du dossier où sont stockées les données sources issues d'EXIOBASE 3.8 (fichier txt) (Retrieved from:   https://zenodo.org/record/4277368 )
path_user <- stringr::str_remove(getwd(), "EmissionsIndicators")
path_project <- stringr::str_remove(getwd(), path_user)
path_data.source <- str_c(path_user,"Dropbox (FOSEM)/Databases/EXIOBASE_3.8")
path_data.source <- str_c("data_in/IOT/")
path_out <- str_c("data_out/IOT_",year,"_",nom,"/")
path_loader <- str_c(path_out, br, "/",br.pays,"/")


path_ResultsTable <- str_c(path_user,path_project,"/results/IO_pays/",year,"/",br.pays,"_",br)
path_ResultsPlot <- str_c(path_user,path_project,"/results/plots/")


path_export <- "ThreeME_V3/data/France/"

# Descriptions of nomenclatures
source("data_in/desc/exio3.desc.R")
source("data_in/desc/CPA4.desc.R")
source("data_in/desc/ThreeME.desc.R")
source("data_in/desc/countries.desc.R")
source("src/functions/01_load.bridge.R")
source("src/functions/01_load.matrix.R")
source("src/functions/01_Leontief.inverse.R")
source("src/functions/01_GHG.extraction.R")
source("src/functions/01_shock.demand.R")
source("src/functions/02_perform.bridge.R")
source("src/functions/02_table.import.R")

sec.desc <- list("exio3.desc" = exio3.desc,
                 "ThreeME.desc" = ThreeME.desc)

### Creation of the subfolders
# Folder where data from the IO calcul are stored in a folder with the name of the nomenclature used. File Indicated by year (y) and country (iso) such that. (ex: data_out_2015_FR)
dir.create(str_c(path_out), recursive = TRUE)
dir.create(str_c(path_ResultsTable), recursive = TRUE)

# Folder to store the source code associated to output plots and tables
dir.create(str_c("src/plots"), recursive = TRUE)
dir.create(str_c("src/tables"), recursive = TRUE)

# Folder to store the image and code associated to src plots and tables
dir.create(str_c("results/plots"), recursive = TRUE)
dir.create(str_c("results/tables"), recursive = TRUE)
