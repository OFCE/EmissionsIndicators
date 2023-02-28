library(zen4R)
library(data.table)

###
source("src/00_header.R")

dir.create(str_c(path_data.source,year), recursive = TRUE)
# zen4R::download_zenodo(doi = "10.5281/zenodo.5589597", path = str_c(path_data.source))

#X: total output
load.matrix("x", year, export = TRUE, row_index = "p.")

#Z conso intérmediaires
load.matrix("Z", year, export = TRUE, col_index = "i.", row_index = "p.")
load.matrix("A", year, export = TRUE, col_index = "i.", row_index = "p.")

## Matrice Y (demande finale par pays et par type)
#Z conso intérmediaires
load.matrix("Y", year, export = TRUE,row_index = "p.")

#F sattelite account 
load.matrix("F", year, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("S", year, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("M", year, satellite = TRUE, export = TRUE, col_index = "i.")
