library(zen4R)
library(data.table)

###


dir.create(str_c(path_data.source,year), recursive = TRUE)
# zen4R::download_zenodo(doi = "10.5281/zenodo.5589597", path = str_c(path_data.source))


#X: total output
load.matrix("x", year, nom, export = TRUE,row_index = "i.")

#Z conso intérmediaires
load.matrix("Z", year,nom, export = TRUE, col_index = "i.", row_index = "p.")
load.matrix("A", year,nom, export = TRUE, col_index = "i.", row_index = "p.")
  
## Matrice Y (demande finale par pays et par type)
#Z conso intérmediaires
load.matrix("Y",year,nom, export = TRUE,row_index = "p.")

#F satellite account 
load.matrix("F", year,nom, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("S",year,nom, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("M", year,nom, satellite = TRUE, export = TRUE, col_index = "i.")

#F satellite account for direct impacts
if (nom == "ixi"){
load.matrix("F_Y",year,nom, export = TRUE, satellite = TRUE, row_index = "p.")
}