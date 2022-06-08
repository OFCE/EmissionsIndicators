
#for (year in year.min:year.max){

### Settings
path_out <- str_c("data.out/IOT_",year,"_",nom,"/")
dir.create(str_c(path_out), recursive = TRUE)

### Data loading
A <- fread(file = str_c(path_data.source,"IOT_",year,"_",nom,"/A.txt"),
           sep = "\t",
           header = FALSE) %>%
  as.data.frame()

## Matrice Y (demande finale par pays et par type)
Y <- fread(file = str_c(path_data.source, "IOT_",year,"_",nom,"/Y.txt"),
           sep = "\t",
           header = FALSE) %>%
  as.data.frame()

## F: compte sattelite (avec indicateurs d'impact environnementaux)
Fe <- fread(file = str_c(path_data.source, "IOT_",year,"_",nom,"/satellite/F.txt"),
            sep = "\t",
            header = FALSE) %>%
  as.data.frame()

### Labels extractions
# Extraction des infos sur les lignes (pays * secteurs) et les colonnes (pays * secteurs)
A_pays <- A[1,][-1:-2] %>% as.character()#extraction du nom des pays de A
A_secteurs <- A[2,][-1:-2] %>% as.character()#extraction des noms de secteur de A
A_pays_secteurs <- str_c(A_pays,"_",A_secteurs)# Pour avoir en une seule chaine de caracteres le nom du secteur et de son pays : "Nompays_NomSecteur"


# Extraction des valeurs de A et mise sous forme d'une matrice de type "numeric"
A.mat <- A[-1:-3,-1:-2] # on ne garde que les valeurs numeriques de A et plus les noms de ligne et colonnes
A.mat <- as.numeric(A.mat)# on transforme les valeur de A en valeurs numeriques et non plus en chaines de caracteres
A.mat <- matrix(A.mat,length(A_pays_secteurs),length(A_pays_secteurs),
                dimnames=list(A_pays_secteurs,A_pays_secteurs)) # on cree la matrice A (valeurs numeriques)


### Matrix construction
Y_pays_secteurs <- A_pays_secteurs %>% as.character()
Y_types_DF <- Y[2,-1:-2] %>% as.character()
Y_pays <- Y[1,-1:-2] %>% as.character()
Y_pays_types_DF <- str_c(Y_pays,"_",Y_types_DF)%>%as.character()


Y.mat <- Y[-1:-3,-1:-2]
Y.mat <- as.numeric(Y)
Y.mat <- matrix(Y.mat ,length(A_pays_secteurs),length(Y_pays_types_DF),
               dimnames=list(Y_pays_secteurs,Y_pays_types_DF))


# Recuperations des infos sur les lignes (types d'extensions env) et les colonnes (secteurs concernes) de F
Fe_noms_extensions <- Fe[-1:-2,1]
Fe_pays_secteurs <- A_pays_secteurs

Fe.mat <- Fe[-1:-2,-1]# extraction des valeurs
Fe.mat <-as.numeric(Fe)
Fe.mat <-matrix(Fe,length(F_noms_extensions),length(A_pays_secteurs),
                dimnames=list(F_noms_extensions,A_pays_secteurs))

### Save and export
# A: matrice des coefficients techniques
saveRDS(A.mat, str_c( path_out, "A.rds"))
saveRDS(A_pays_secteurs, str_c(path_out, "A_pays_secteurs.rds"))
saveRDS(A_pays, str_c(path_out, "A_pays.rds"))
saveRDS(A_secteurs, str_c(path_out, "A_secteurs.rds"))
# Y: matrice de demande finale
saveRDS(Y.mat, str_c(path_out, "Y.rds"))
saveRDS(Y_types_DF, str_c(path_out, "Y_types_DF.rds"))
saveRDS(Y_pays_types_DF, str_c(path_out, "Y_pays_types_DF.rds"))
saveRDS(Y_pays, str_c(path_out, "Y_pays.rds"))
# fe: matrice de compte satellite 
saveRDS(Fe.mat, str_c(path_out, "F.rds"))
saveRDS(Fe_noms_extensions, str_c(path_out, "F_noms_extensions.rds"))
saveRDS Fe_pays_secteurs, str_c(path_out, "F_pays_secteurs.rds"))

print(str_c("les fichiers sont sauvegardés à cette adresse : ",path_out))

######## Pour vider la memoire ###########
rm(list=c("A","Y", "Fe"))
#}
