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
#x (production totale (pour CI + pour DF))
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


#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays

#Chemin pour exporter
#dir.create(str_c(path_out, "/IO_pays"), recursive = TRUE)
#path_IOpays_tables <- str_c(path_out, "/IO_pays")

#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y et L au préalable)
for (pays in c("France","EU","US","Chine","Amerique du N.","Amerique du S.","Afrique","Russie","Europe (autres)","Asie","Moyen-Orient","Oceanie" )) {
  
  #Colonne nom pays (pas nécessaire si pas rbind par la suite)
  nom_pays <- c(rep(pays,1056))
  
  #Colonne demande finale
  DF=Y
  #mettre à 0 les entrées des autres pays (demande finale adressée au pays en question)
  DF[-str_which(rownames(DF),as.character(pays)),]<-0
  DF_tot <- as.matrix(DF) %*% Id(DF) #somme de toutes les composantes
  #interprétation : quantité (en ) consommée dans le monde et produite par ce pays
  
  #Vecteur production (identique au vecteur monde)
  ##x (production totale (pour CI + pour DF))
  production <- (L %*% DF_tot) %>% as.numeric
  #interprétation : quantité d'inputs (en ) nécessaire pour produire une unité d'output dans ce pays
  
  #Matrice S ("impact producteur") : impact environnemental (uniquement demande pays)
  x_1_select <- 1/production
  x_1_select[is.infinite(x_1_select)] <- 0 
  x_1_select <- as.numeric(x_1_select)
  x_1d_select <- diag(x_1_select)
  S_select <- as.matrix(Fe) %*% x_1d_select
  #interprétation : impact de la production de ce pays (par input)
  
  #Convertir en CO2 équivalent (éliminer les autres impacts)
  GES_list_select <- list()
  ##Sélectionner les impacts
  GES_list_select[["GES.raw"]] <- S_select %>% as.data.frame %>% 
    filter(str_detect(row.names(.), "CO2") |
          str_detect(row.names(.), "CH4") | 
          str_detect(row.names(.), "N2O") | 
          str_detect(row.names(.), "SF6") | 
          str_detect(row.names(.), "PFC") | 
          str_detect(row.names(.), "HFC") )
  ##Faire la conversion
  for (ges in glist){
    #Row number for each GES in the S matrix
    id_row <- str_which(row.names(GES_list_select[["GES.raw"]]),str_c(ges))
    GES_list_select[[str_c(ges)]] <- GES_list_select[["GES.raw"]][id_row,] %>% colSums() %>% as.data.frame()
    GES_list_select[[ges]] <- GHGToCO2eq(GES_list_select[[ges]]) #dossier functions
  }
  ##Total par type de gaz (toute source confondue)
  GES_list_select[["GES"]] <- GES_list_select[["CO2"]] +
    GES_list_select[["CH4"]] +
    GES_list_select[["N2O"]] +
    GES_list_select[["SF6"]] +
    GES_list_select[["HFC"]] +
    GES_list_select[["PFC"]]
  #Colonne impact GES
  GES_impact_prod <- GES_list_select[["GES"]]
  #interprétation : impact de la production de ce pays en CO2 équivalent
  
  #Vecteur équivalent à M ("impact demande finale" adressée au pays)
  ##(donc déjà en CO2eq)
  GES_impact_DF <- as.matrix(GES_impact_prod %>% t()) %*% L %>% t()
  #interprétation : impact de la demande de ces produits
  
  #Créer le tableau en assemblant les colonnes
  GES_impact_producteur=as.numeric(unlist(GES_impact_prod))
  GES_impact_demande=as.numeric(unlist(GES_impact_DF))
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production,GES_impact_producteur,GES_impact_demande
           )
         )
  
  #Mettre à 0 la production pour les autres pays (tableau spécifique à un seul pays)
  io_table[-str_which(rownames(io_table),as.character(pays)),5]<-0
  
  #Créer une colonne produits, ordonner les colonnes
  io_table$produits=rownames(io_table)
  io_table$produits=sub(".*?_", "",io_table$produits)
  io_table = io_table %>% select(nom_pays,produits,DF_tot,production,GES_impact_producteur,GES_impact_demande)
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
}

#Charger les datasets
for (pays in c("France","EU","US","Chine","Amerique du N.","Amerique du S.","Afrique","Russie","Europe (autres)","Asie","Moyen-Orient","Oceanie" )) {
  IO <- readRDS(str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  }
