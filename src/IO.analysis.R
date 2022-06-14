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
DF_tot_to_france <- as.matrix(DF_france) %*% Id(DF_france) #somme ligne, équivalent de y_tot

L_france = L
L_france[,-str_which(colnames(L),"France")]<-0 #filtrer les colonnes = tous les inputs nécessaires pour produire output français
#prochain bloc = pour que L (matrice) "rentre dans une ligne": le total 
#des CI venant de chaque region-secteur nécessaires à la France
CI_of_france <- as.matrix(L_france) %*% Id(L_france)

production_france <- (L_france %*% y_tot) %>% as.numeric #équivalent de x:
#production de la France grâce à l'input en ligne

#le reste=A VERIFIER
x_1_france <- 1/production_france
x_1_france[is.infinite(x_1_france)] <- 0 
x_1_france <- as.numeric(x_1_france)
x_1d_france <- diag(x_1_france)
S_france <- as.matrix(Fe) %*% x_1d_france #équivalent de S (une case = 
#impact en ligne de la production française réalisée grâce à l'input en colonne)
impact_input_of_france <- as.matrix(S_france %>% t()) %*% Id(S_france %>% t()) #transposer pour somme par ligne 
#(impact total de la production française utilisant l'input en ligne)
#peut-être ne garder que certains impacts? (pour que la somme ait un sens)


M_france <- S_france %*% L_france #équivalent de M (une case =
#impact en ligne de la production française du produit en colonne)
impact_production_france <- as.matrix(M_france %>% t()) %*% Id(M_france %>% t()) #transposer pour somme par ligne 
#(impact total de la production française du produit/output en ligne)

table = data.frame(
  impact_production_france,production_france,DF_tot_to_france,CI_of_france,impact_input_of_france
)

###généralisation
TableFinale<-function(vecteur_régions){
  
}
for (pays in c("US","France")) {
  
  DF=Y
  DF[-str_which(rownames(DF),as.character(pays)),]<-0
  DF_tot <- as.matrix(DF) %*% Id(DF) 
  
  L_select = L
  L_select[,-str_which(colnames(L),as.character(pays))]<-0
  CI <- as.matrix(L_select) %*% Id(L_select)
  
  production <- (L_select %*% y_tot) %>% as.numeric
  
  x_1_select <- 1/production
  x_1_select[is.infinite(x_1_select)] <- 0 
  x_1_select <- as.numeric(x_1_select)
  x_1d_select <- diag(x_1_select)
  S_select <- as.matrix(Fe) %*% x_1d_select
  impact_input <- as.matrix(S_select %>% t()) %*% Id(S_select %>% t())
  
  M_select <- S_select %*% L_select
  impact_production <- as.matrix(M_select %>% t()) %*% Id(M_select %>% t())
  
  nom_pays <- c(rep(pays,1056))

  assign(str_c("table_",pays),
         data.frame(nom_pays,
           impact_production,production,DF_tot,CI,impact_input
           )
         )
}

bigtable=rbind(table_US,table_France)

