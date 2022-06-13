br <- "ThreeMe"

# Chargement des données I-O sauvegardées par le script exio3.Pre-loader.R
A <-readRDS(str_c("data_out/IOT_",year,"_",nom,"/A_",br,".rds"))
Y <-readRDS(str_c(path_out,"/Y_",br,".rds"))
Fe <-readRDS(str_c("data_out/IOT_",year,"_",nom,"/Fe_",br,".rds"))

## Calcul de L: matrice de Leontief
L <- LeontiefInverse(A)
#

print("Computation of the Leontief matrix (L) : done")


Y_pays_types_DF<-colnames(Y)
y_tot<- as.matrix(Y) %*% as.matrix(rep(1,length(Y_pays_types_DF)))
rm(Y_pays_types_DF)
rm(Y)
#y_tot : demande totale par région et secteur


## calcul de S
# x (production totale (pour CI + pour DF))
#filtrer L ici pour avoir l'indicateur pour un pays donné?
x <- (L %*% y_tot) %>% as.numeric()


#print("J'ai calcule le vecteur de la production totale x. Je vais pouvoir calculer S=Fxdiag(1/x)")

x_1 <- 1/x
x_1[is.infinite(x_1)] <- 0 
x_1 <- as.numeric(x_1)
x_1d <- diag(x_1)


S <- as.matrix(Fe) %*% x_1d
S[is.nan(S)]



# Exportation des résultats sous forme de fichier Rds des différentes matrices calculées
saveRDS( x, str_c("data_out/IOT_",year,"_",nom,"/x.rds"))
saveRDS( S, str_c("data_out/IOT_",year,"_",nom,"/S.rds"))

saveRDS( L, str_c("data_out/IOT_",year,"_",nom,"/L.rds"))




### Distinction of each type of GES

GES_list <- list()
GES_list[["GES.raw"]] <- S %>% as.data.frame %>% filter(str_detect(row.names(.), "CO2") |
                                                          str_detect(row.names(.), "CH4") | 
                                                          str_detect(row.names(.), "N2O") | 
                                                          str_detect(row.names(.), "SF6") | 
                                                          str_detect(row.names(.), "PFC") | 
                                                          str_detect(row.names(.), "HFC") )


# Total par type de gaz (toute source confondue)
for (ges in glist){
  #Row number for each GES in the S matrix
  id_row <- str_which(row.names( GES_list[["GES.raw"]]),str_c(ges))
  
  
  GES_list[[str_c(ges)]] <- GES_list[["GES.raw"]][id_row,] %>% colSums() %>% as.data.frame()
}

# Conversion en MtCO2e
#Suggestion instead: 
#for (ges in c("CH4","N2O","SF6")){GES_list[[ges]] <- GHGToCO2eq(GES_list[[ges]])}

GES_list[["CH4"]] <- 28 * GES_list[["CH4"]]
GES_list[["N2O"]] <- 265 * GES_list[["N2O"]]
GES_list[["SF6"]] <- 23500 * GES_list[["SF6"]]


GES_list[["GES"]] <- GES_list[["CO2"]] +
  GES_list[["CH4"]] +
  GES_list[["N2O"]] +
  GES_list[["SF6"]] +
  GES_list[["HFC"]] + #déjà convertis
  GES_list[["PFC"]]
#Crée un vecteur (1065x1) (de quantité de GES émise par chaque secteur ?)

print("Computation of the environemental impact (S) : done")  


M <- S %*% L
for (ges in c(glist,"GES")){
  M.mat <-  GES_list[[str_c(ges)]] %>% unlist %>% as.numeric %>% diag
  
  
  GES_list[[str_c("M.",ges)]]  <-  M.mat %*% L 
  GES_list[[str_c("M.",ges)]][is.nan(GES_list[[str_c("M.",ges)]])]
  GES_list[[str_c("M.",ges)]] <-   GES_list[[str_c("M.",ges)]] %>% as.data.frame() %>% `rownames<-`(rownames(L))

  
  
  saveRDS(GES_list[[str_c("M.",ges)]], str_c("EXIOBASE/data/IOT_",year,"_",nom,"/M.",ges,".rds"))
}
print("Computation of the environmental impact multipliers (M) : done")


##saveRDS( M, str_c("EXIOBASE/data/IOT_",year,"_",nom,"/M.rds"))
print(str_c("Les matrices S, L et M ainsi que le vecteur x ont ete calculees et enregistrees a l'adresse suivante~:EXIOBASE/data/IOT_",year,"_",nom,"/"))


rm(list=c("L","S","x"))
rm(GES_list)

#Obtenir un dataframe avec les données pour un pays
###exemple pour un seul pays/région
DF_france = Y
DF_france[-str_which(rownames(DF_france),"France"),]<-0
DF_tot_to_france <- as.matrix(DF_france)
DF_tot_to_france <- DF_tot_to_france %*% Id(DF_tot_to_france)

L_france = L
L_france[,-str_which(colnames(L),"France")]<-0
#prochain bloc = pour que L (matrice) "rentre dans une ligne": le total 
#des CI nécessaires à la France pour produire chaque produit
CI_of_france <- as.matrix(L_france) %>% t()
CI_of_france <- CI_of_france %*% Id(CI_of_france)

x_france <- (L_france %*% y_tot) %>% as.numeric

#le reste=A VERIFIER
x_1_france <- 1/x_france
x_1_france[is.infinite(x_1_france)] <- 0 
x_1_france <- as.numeric(x_1_france)
x_1d_france <- diag(x_1_france)
coef_envir_france <- as.matrix(Fe) %*% x_1d_france %>% t()
coef_envir_france <- coef_envir_france %*% Id(coef_envir_france)

impact_production_france <- coef_envir_france %*% L_france %>% t()
impact_production_france <- impact_production_france %*% Id(impact_production_france)

table = data.frame(
  impact_production_france,x_france,DF_tot_to_france,CI_of_france,coef_envir_france
)

###généralisation
TableFinale<-function(pays){
  for (pays in regions) { 
    pays_select=
    M_select=
    x_select=
    DF_select=
    L_select=
    S_select=
    table = data.frame(
      pays_select,M_select,x_select,DF_select,L_select,S_select
      )
  }
}
