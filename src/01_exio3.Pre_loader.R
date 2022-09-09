
#for (year in year.min:year.max){

### Settings
path_out <- str_c("data_out/IOT_",year,"_",nom,"/")
dir.create(str_c(path_out), recursive = TRUE)


#X: total output
load.matrix("x", year = 2015, export = TRUE)

#Z conso intérmediaires
load.matrix("Z", year = 2015, export = TRUE)

## Matrice Y (demande finale par pays et par type)
#Z conso intérmediaires
load.matrix("Y", year = 2015, export = TRUE)

#F sattelite account 
load.matrix("F", year = 2015, satellite = TRUE, export = TRUE)
