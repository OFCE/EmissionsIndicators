br <- "ThreeMe"

# Chargement des données I-O sauvegardées par le script exio3.Pre-loader.R
A <-readRDS(str_c("data_out/IOT_",year,"_",nom,"/A_",br,".rds"))
Y <-readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y_",br,".rds"))
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
  GES_list[["HFC"]] +
  GES_list[["PFC"]]


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
#saveRDS( M.alt, str_c("EXIOBASE/data/IOT_",year,"_",nom,"/M.alt.rds"))
#saveRDS( M.dir, str_c("EXIOBASE/data/IOT_",year,"_",nom,"/M.dir.rds"))
#saveRDS( M.ndir, str_c("EXIOBASE/data/IOT_",year,"_",nom,"/M.ndir.rds"))
print(str_c("Les matrices S, L, M et M.dir ainsi que le vecteur x ont ete calculees et enregistrees a l'adresse suivante~:EXIOBASE/data/IOT_",year,"_",nom,"/"))
#où est M.dir ?


rm(list=c("L", "L.s1","S","x"))
#où est L.s1
rm(GES_list)