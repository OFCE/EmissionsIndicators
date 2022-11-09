library(zen4R)
library(data.table)
###
source("src/00_header.R")

#X: total output
load.matrix("x", year = 2015, export = TRUE, row_index = "i.")

#Z conso intérmediaires
load.matrix("Z", year = 2015, export = TRUE, col_index = "i.", row_index = "p.")
load.matrix("A", year = 2015, export = TRUE, col_index = "i.", row_index = "p.")

## Matrice Y (demande finale par pays et par type)
#Z conso intérmediaires
load.matrix("Y", year = 2015, export = TRUE,row_index = "p.")

#F sattelite account 
load.matrix("F", year = 2015, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("S", year = 2015, satellite = TRUE, export = TRUE, col_index = "i.")
load.matrix("M", year = 2015, satellite = TRUE, export = TRUE, col_index = "i.")
