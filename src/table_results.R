
c("Autriche","Belgique","Bulgarie","Chypre","République Tchèque","Allemagne",
  "Danemark","Estonie","Espagne","Finlande","France","Grèce","Croatie","Hongrie",
  "Irlande","Italie","Lituanie","Luxembourg","Lettonnie","Malte","Pays-bas",
  "Pologne","Portugal","Roumanie","Suède","Slovénie","Slovaquie","Royaume-Uni",
  "Reste du monde")
#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y, Fe et L au préalable)
for (iso in c("FR", "DE", "IT", "PL", "SP", "GB")) {
  
  #Colonne nom pays (pas nécessaire si pas rbind par la suite)
  #length()=204
  
  #Colonne demande finale
  DF <-readRDS(str_c(path_loader,"Y_",br.pays,"_",br,".rds")) 
  #mettre à 0 les entrées des autres pays (demande finale du pays en question adressée aux autres pays)
  DF[,-str_which(colnames(DF),as.character(iso))]<-0
  DF_tot <- as.matrix(DF) %*% Id(DF)
  
  nom_pays <- c(rep(iso,nrow(DF_tot))) 
  
  #somme de toutes les composantes
  #interprétation : quantité consommée par ce pays et produite dans le monde
  
  #Vecteur production du pays
  production_pays <- readRDS(str_c(path_loader,"X_",br.pays,"_",br,".rds")) 
  production_pays[-str_which(rownames(production_pays),as.character(iso)),]<-0
  production_pays <- as.numeric(unlist(production_pays))
  
  #Impacts producteur du pays
  
  
  GES_S <- readRDS(str_c(path_loader,"S.mat_",ghg,".rds"))
  
  
  #Sélectionner les impacts de la production du pays en question
  GES_S[-str_which(rownames(GES_S),as.character(iso)),]<-0
  GES_S <-  GES_S %>% unlist() %>% as.numeric()
  
  #Impacts demande du pays
  #GES_impact_M_select <- GES_impact_M
  #####GES_impact_M_select[-str_which(rownames(GES_impact_M_select),as.character(pays)),]<-0
  
  #contribution de chaque secteur à la DF de ce pays: 
  #colonne Y divisée par somme colonne (total de chaque DF)
  Y_select <-  t(DF)
  Y_select[,-str_which(colnames(Y_select),as.character(iso))]<-0
  Y_sectors.tot<-colSums(Y_select)
  
  y_2 <- 1/Y_sectors.tot
  y_2[is.infinite(y_2)] <- 0 
  y_2d <- as.numeric(y_2) %>% diag
  Y_sectors.share <- t(DF) %*% y_2d
  
  GES_M <- readRDS(str_c(path_loader,"M.mat_",ghg,".rds")) %>% select(-id) %>% as.matrix() %>% t

  GES_M[,-str_which(colnames(GES_M),as.character(iso))]<-0
  #GES_M <-  (GES_M) %*% t(Y_sectors.share)
  GES_M <- GES_M %>% unlist() %>% as.numeric()
  
  #Impacts VA du pays
  # impact_VA_select <- impact_VA
  # #Sélectionner les impacts de la production du pays en question
  # impact_VA_select[-str_which(rownames(impact_VA_select),as.character(pays)),]<-0
  # impact_VA_select = impact_VA_select%>%unlist()%>%as.numeric()
  # 
  # #par composante
  # GES_VA_compo_select = GES_VA_compo
  # GES_VA_compo_select[-str_which(rownames(GES_VA_compo_select),as.character(pays)),]<-0
  # 
  # 
  
  #Créer le tableau en assemblant les colonnes
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,
                    production_pays,
                    GES_S,
                    GES_M #,
                    #impact_VA_select,
                    #GES_VA_compo_select
         )
  )
  
  #Mettre à 0 la production pour les autres pays (tableau spécifique à un seul pays)
  #io_table$production[-str_which(rownames(io_table),as.character(pays)),]<-0
  #-> fait automatiquement avec production_2
  
  #Créer une colonne produits, ordonner les colonnes, nettoyer le dataframe

  io_table$produits <- sub(".*?_", "",rownames(io_table))
  io_table$regions <- sub("_.*", "",rownames(io_table))
  io_table <-  io_table %>% 
    select(regions,nom_pays,produits,DF_tot,production_pays,
           GES_S,
           GES_M)
  io_table[sapply(io_table, simplify = 'matrix', is.infinite)] <- 0
  io_table[sapply(io_table, simplify = 'matrix', is.nan)] <- 0
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_ResultsTable, "/Summary_EMS.",ghg,"_", iso, ".rds"))
  
}
