
#for (year in year.min:year.max){

### Settings
path_out <- str_c("data_out/IOT_",year,"_",nom,"/")
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
A.mat <- as.matrix(A)[-1:-3,-1:-2] # on ne garde que les valeurs numeriques de A et plus les noms de ligne et colonnes
A.mat <- as.numeric(unlist(A.mat))# on transforme les valeur de A en valeurs numeriques et non plus en chaines de caracteres
A.mat <- matrix(A.mat,length(A_pays_secteurs),length(A_pays_secteurs),
                dimnames=list(A_pays_secteurs,A_pays_secteurs)) # on cree la matrice A (valeurs numeriques)

#check de données
A_bis <- fread(file = str_c(path_data.source,"bis_IOT_",year,"_",nom,"/A.txt"),
           sep = "\t",
           header = FALSE) %>%
  as.data.frame()
A_bis.mat <- as.matrix(A_bis)[-1:-3,-1:-2] # on ne garde que les valeurs numeriques de A et plus les noms de ligne et colonnes
A_bis.mat <- as.numeric(unlist(A_bis.mat))# on transforme les valeur de A en valeurs numeriques et non plus en chaines de caracteres
A_bis.mat <- matrix(A_bis.mat,length(A_pays_secteurs),length(A_pays_secteurs),
                dimnames=list(A_pays_secteurs,A_pays_secteurs)) # on cree la matrice A (valeurs numeriques)

### Matrix construction
Y_pays_secteurs <- A_pays_secteurs %>% as.character()
Y_types_DF <- Y[2,-1:-2] %>% as.character()
Y_pays <- Y[1,-1:-2] %>% as.character()
Y_pays_types_DF <- str_c(Y_pays,"_",Y_types_DF) %>% as.character()

Y[-1:-3,-1:-2] %>% unlist %>% as.numeric()
Y.mat <- Y[-1:-3,-1:-2]
Y.mat <- as.numeric(unlist(Y.mat))
Y.mat <- matrix(Y.mat ,length(A_pays_secteurs),length(Y_pays_types_DF),
               dimnames=list(Y_pays_secteurs,Y_pays_types_DF))

##check de données
Y_bis <- fread(file = str_c(path_data.source, "bis_IOT_",year,"_",nom,"/Y.txt"),
               sep = "\t",
               header = FALSE) %>%
  as.data.frame()
Y_bis_pays_secteurs <- A_pays_secteurs %>% as.character()
Y_bis_types_DF <- Y_bis[2,-1:-2] %>% as.character()
Y_bis_pays <- Y_bis[1,-1:-2] %>% as.character()
Y_bis_pays_types_DF <- str_c(Y_bis_pays,"_",Y_bis_types_DF) %>% as.character()
Y_bis[-1:-3,-1:-2] %>% unlist %>% as.numeric()
Y_bis.mat <- Y_bis[-1:-3,-1:-2]
Y_bis.mat <- as.numeric(unlist(Y_bis.mat))
Y_bis.mat <- matrix(Y_bis.mat ,length(A_pays_secteurs),length(Y_bis_pays_types_DF),
                dimnames=list(Y_bis_pays_secteurs,Y_bis_pays_types_DF))


# Recuperations des infos sur les lignes (types d'extensions env) et les colonnes (secteurs concernes) de F
Fe_noms_extensions <- Fe[-c(1,2,3),1]
Fe_pays_secteurs <- A_pays_secteurs

#ne marche pas très bien
Fe.mat <- Fe[-1:-2,-1]# extraction des valeurs
Fe.mat <-as.numeric(unlist(Fe.mat))
Fe.mat <-matrix(Fe,length(Fe_noms_extensions),length(A_pays_secteurs),
                dimnames=list(Fe_noms_extensions,A_pays_secteurs))

##test pour Fe à la place
Fe.mat <- Fe %>% as.data.frame() %>% select(-V1) %>% `colnames<-`(Fe_pays_secteurs)
Fe.mat <- Fe.mat[-c(1,2,3),]
Fe.mat = Fe.mat %>% summarise(across(all_of(A_pays_secteurs), ~ as.numeric(.))) %>% `rownames<-`(Fe_noms_extensions)

#check de données
Fe_bis <- fread(file = str_c(path_data.source, "bis_IOT_",year,"_",nom,"/satellite/F.txt"),
            sep = "\t",
            header = FALSE) %>%
  as.data.frame()
Fe_bis_noms_extensions <- Fe_bis[-1:-2,1]
Fe_bis_pays_secteurs <- A_pays_secteurs
Fe_bis.mat <- Fe_bis %>% as.data.frame() %>% select(-V1) %>% `colnames<-`(Fe_bis_pays_secteurs)
Fe_bis.mat <- Fe_bis.mat[-c(1,2),]
Fe_bis.mat = Fe_bis.mat %>% summarise(across(all_of(A_pays_secteurs), ~ as.numeric(.))) %>% `rownames<-`(Fe_bis_noms_extensions)


### Save and export
# A: matrice des coefficients techniques
saveRDS(A.mat, str_c(path_out, "A.rds"))
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
saveRDS(Fe_pays_secteurs, str_c(path_out, "F_pays_secteurs.rds"))

print(str_c("les fichiers sont sauvegardés à cette adresse : ",path_out))

######## Pour vider la memoire ###########
rm(list=c("A","Y", "Fe"))
#}

#comparaison de A calculé et A exiobase:
#besoin de X et de Z

#X: total output
X_exio <- fread(file = str_c(path_data.source,"IOT_",year,"_",nom,"/x.txt"),
                sep = "\t",
                header = FALSE) %>%
  as.data.frame()
X.mat <- as.matrix(X_exio)[-1,-1:-2] %>% as.numeric()
X.mat <- matrix(X.mat,length(A_pays_secteurs),1,
                dimnames=list(A_pays_secteurs,1)) %>% as.data.frame() # on cree la matrice X (valeurs numeriques)
rownames(X.mat) <- A_pays_secteurs
X.mat <- X.mat %>% rename(production=1)
saveRDS(X.mat, str_c(path_out, "x.rds"))

#Z: flow matrix
Z <- fread(file = str_c(path_data.source,"IOT_",year,"_",nom,"/Z.txt"),
                sep = "\t",
                header = FALSE) %>%
  as.data.frame()
Z.mat <- as.matrix(Z)[-1:-3,-1:-2] # on ne garde que les valeurs numeriques de A et plus les noms de ligne et colonnes
Z.mat <- as.numeric(unlist(Z.mat)) # on transforme les valeur de A en valeurs numeriques et non plus en chaines de caracteres
Z.mat <- matrix(Z.mat,length(A_pays_secteurs),length(A_pays_secteurs),
                dimnames=list(A_pays_secteurs,A_pays_secteurs)) # on cree la matrice A (valeurs numeriques)
saveRDS(Z.mat, str_c(path_out, "Z.rds"))

##Comparaison des impacts:
#besoin de S et M
###VERIFIER: DIFFERENCE ENTRE "SATELLITE" ET "IMPACTS"

#S: coefficients d'impact
S <- fread(file = str_c(path_data.source,"IOT_",year,"_",nom,"/satellite/S.txt"),
                sep = "\t",
                header = FALSE) %>%
  as.data.frame()
S_noms_extensions <- S[-1:-3,1]
#A_pays_secteurs=readRDS(str_c(path_out, "A_pays_secteurs.rds"))
S_pays_secteurs <- A_pays_secteurs
S.mat <- S %>% as.data.frame() %>% select(-V1) %>% `colnames<-`(S_pays_secteurs)
S.mat <- S.mat[-c(1,2,3),]
S.mat = S.mat %>% summarise(across(all_of(S_pays_secteurs), ~ as.numeric(.))) %>% `rownames<-`(S_noms_extensions)
saveRDS(S.mat, str_c(path_out, "S.rds"))
saveRDS(S_noms_extensions, str_c(path_out, "S_noms_extensions.rds"))


M <- fread(file = str_c(path_data.source,"IOT_",year,"_",nom,"/satellite/S_Y.txt"),
           sep = "\t",
           header = FALSE) %>%
  as.data.frame()
M_noms_extensions <- M[-1:-3,1]
#Y_types_DF=readRDS(str_c(path_out, "Y_types_DF.rds"))
M_types_DF <- Y_types_DF
M_pays <- M[1,-1] %>% as.character()
M_pays_types_DF <- str_c(M_pays,"_",M_types_DF) %>% as.character()

M.mat <- M[-1:-3,-1]
M.mat <- as.numeric(unlist(M.mat))
M.mat <- matrix(M.mat ,length(M_noms_extensions),length(M_pays_types_DF),
                dimnames=list(M_noms_extensions,M_pays_types_DF))

saveRDS(M.mat, str_c(path_out, "M.rds"))
saveRDS(M_types_DF, str_c(path_out, "M_types_DF.rds"))
saveRDS(M_pays_types_DF, str_c(path_out, "M_pays_types_DF.rds"))
saveRDS(M_pays, str_c(path_out, "M_pays.rds"))

#F_Y : équivalent de F pour la demande
Fy <- fread(file = str_c(path_data.source, "IOT_",year,"_",nom,"/satellite/F_Y.txt"),
            sep = "\t",
            header = FALSE) %>%
  as.data.frame()
#Y_pays_types_DF=readRDS(str_c(path_out, "Y_pays_types_DF.rds"))
FY_pays_types_DF=Y_pays_types_DF
FY_noms_extensions <- Fy[-1:-3,1]
Fy.mat <- Fy[-1:-3,-1]
Fy.mat <- as.numeric(unlist(Fy.mat))
Fy.mat <- matrix(Fy.mat ,length(FY_noms_extensions),length(FY_pays_types_DF),
                dimnames=list(FY_noms_extensions,FY_pays_types_DF))
saveRDS(Fy.mat, str_c(path_out, "Fy.rds"))
