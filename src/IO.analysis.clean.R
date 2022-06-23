#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays

br <- "CPA2002_Niv1"
# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_loader,"Y_",br,".rds"))
Fe <-readRDS(str_c(path_loader,"Fe_",br,".rds"))
Z <-readRDS(str_c(path_loader,"Z_",br,".rds"))
X <-readRDS(str_c(path_loader,"X_",br,".rds"))

#Calcul des coefficients techniques
##Matrice de Leontief
A <- sweep(Z, 
           MARGIN = 2,
           STATS=X$production,
           FUN='/',
           check.margin = TRUE)
A[is.na(as.data.frame(A))] <- 0
saveRDS(A, str_c(path_loader, "A_",br,".rds"))
##Inverse de Leontief
L <- LeontiefInverse(A)
saveRDS(L, str_c(path_loader, "L_",br,".rds"))

#Matrice S (impact producteur)
x <- ((L %*% as.matrix(Y)) %*% Id(Y)) %>% as.numeric
x_1 <- 1/x
x_1[is.infinite(x_1)] <- 0 
x_1d <- as.numeric(x_1) %>% diag()
S <- (as.matrix(Fe) %*% x_1d) %>% `colnames<-`(rownames(X))
S[is.nan(S)]
S_volume <- S %*% as.matrix(X)

#Matrice M (impact demande et CI)
M <- S %*% L 
M_volume <- M %*% as.matrix(Y)

#contribution de chaque secteur à la DF: 
#colonne Y divisée par somme colonne (total de chaque DF)
Y_sectors.tot<-colSums(Y)
y_2 <- 1/Y_sectors.tot
y_2[is.infinite(y_2)] <- 0 
y_2d <- as.numeric(y_2) %>% diag
Y_sectors.share <- as.matrix(Y) %*% y_2d
M_vol.dim <- M_volume %*% t(Y_sectors.share)


#Chemin pour exporter les données
dir.create(str_c(path_codedata, "results/IO_pays/", year,"/", br,"/"), recursive = TRUE)
path_results_tables <- str_c(path_codedata, "results/IO_pays/", year,"/", br,"/")
#Chemin pour exporter les plots
format = "pdf"
dir.create(str_c(path_codedata, "results/plots/", year,"/", br,"/", format), recursive = TRUE)
path_results_plots <- str_c(path_codedata, "results/plots/", year,"/", br,"/", format, "/")

#Attention, il faut mettre "Europe" et non "Europe (autres)", sinon la sélection ne marche pas

#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y, Fe et L au préalable)
for (pays in c("France","EU","US","Chine","Amerique du N.","Amerique du S.","Afrique","Russie","Europe","Asie","Moyen-Orient","Oceanie")) {
  
  #Colonne nom pays (pas nécessaire si pas rbind par la suite)
  nom_pays <- c(rep(pays,ncol(Z))) #length()=204
  
  #Colonne demande finale
  DF=Y
  #mettre à 0 les entrées des autres pays (demande finale du pays en question adressée aux autres pays)
  DF[,-str_which(colnames(DF),as.character(pays))]<-0
  DF_tot <- as.matrix(DF) %*% Id(DF) #somme de toutes les composantes
  #interprétation : quantité consommée par ce pays et produite dans le monde
  
  #Vecteur production du pays
  production_pays <- X
  production_pays[-str_which(rownames(production_pays),as.character(pays)),]<-0
  production_pays=as.numeric(unlist(production_pays))
  
  #Impacts du pays
  S_vol.select <- Fe #Fe est l'équivalent de S %*% X mais par produit au lieu d'avoir l'impact de la production mondiale
  #Sélectionner les impacts de la production du pays en question
  S_vol.select[,-str_which(colnames(S_vol.select),as.character(pays))]<-0
  
  #interprétation : impact de la production de ce pays (par output)
  impact_prod = t(S_vol.select) %*% Id(t(S_vol.select))
  
  M_vol.select <- M_vol.dim
  M_vol.select[,-str_which(colnames(M_vol.select),as.character(pays))]<-0
  #(ou sinon filtrer les colonnes de L)
  impact_dem = t(M_vol.select) %*% Id(t(M_vol.select))
  
  #Sélection des impacts GES et Conversion en CO2eq
  listdf=list(S=S_vol.select,M=M_vol.select)
  index=1
  for (matrix in listdf) {
    GES_list <- list()
    GES_list[["GES.raw"]] <- matrix %>% 
      as.data.frame %>% 
      filter(str_detect(row.names(.), "CO2") | 
               str_detect(row.names(.), "CH4") | 
               str_detect(row.names(.), "N2O") | 
               str_detect(row.names(.), "SF6") | 
               str_detect(row.names(.), "PFC") | 
               str_detect(row.names(.), "HFC") )
    for (ges in glist){
      #Row number for each GES in the S matrix
      id_row <- str_which(row.names(GES_list[["GES.raw"]]),str_c(ges))
      GES_list[[str_c(ges)]] <- GES_list[["GES.raw"]][id_row,] %>% colSums() %>% as.data.frame()
      GES_list[[ges]] <- GHGToCO2eq(GES_list[[ges]])
    }
    GES_list[["GES"]] <- GES_list[["CO2"]] +
      GES_list[["CH4"]] +
      GES_list[["N2O"]] +
      GES_list[["SF6"]] +
      GES_list[["HFC"]] +
      GES_list[["PFC"]]
    assign(str_c("GES_impact_",names(listdf)[index]), GES_list[["GES"]])
    index=index+1
  }
  #interprétation : impact de la production et de la demande de ce pays en CO2 équivalent
  GES_impact_S=as.numeric(unlist(GES_impact_S))
  GES_impact_M=as.numeric(unlist(GES_impact_M))
  
  
  #Créer le tableau en assemblant les colonnes
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production_pays,impact_prod,impact_dem,GES_impact_S,GES_impact_M
         )
  )
  
  #Mettre à 0 la production pour les autres pays (tableau spécifique à un seul pays)
  #io_table$production[-str_which(rownames(io_table),as.character(pays)),]<-0
  #-> fait automatiquement avec production_2
  
  #Créer une colonne produits, ordonner les colonnes, nettoyer le dataframe
  io_table$pays.produits=rownames(io_table)
  io_table$produits=sub(".*?_", "",io_table$pays.produits)
  io_table$regions=sub("_.*", "",io_table$pays.produits)
  io_table = io_table %>% 
    select(regions,nom_pays,produits,DF_tot,production_pays,impact_prod,impact_dem,GES_impact_S,GES_impact_M)
  io_table[sapply(io_table, simplify = 'matrix', is.infinite)] <- 0
  io_table[sapply(io_table, simplify = 'matrix', is.nan)] <- 0
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_results_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_results_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  
  #Créer un graphique
  plot=IO %>% 
    #par produits
    group_by(produits) %>%
    filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
    mutate(agg.demande_impact=sum(GES_impact_M),
           agg.producteur_impact=sum(GES_impact_S),
           agg.production=sum(production_pays),
           agg.demande_finale=sum(DF_tot)) %>%
    ungroup() %>%
    #format long pour afficher les deux indicateurs
    pivot_longer(
      cols = c("agg.producteur_impact","agg.demande_impact"),
      names_to = "indicator",
      values_to = "impact") %>%
    as.data.frame() %>% 
    #plot
    ggplot( 
      aes(x= produits, 
          y = impact,
          fill = indicator)) +
    geom_bar(stat='identity',position = "dodge") +
    theme(axis.text.x = element_text(angle = 25, size=5, vjust = 1, hjust=1),
          plot.title =element_text(size=12, face='bold', hjust=0.5),
          panel.background = element_blank(),
          panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
          plot.margin = unit(c(10,5,5,5), "mm"))+
    labs(title="Impacts prodcuteur et consommateur",
         x ="Secteurs", y = "Impact GES (CO2eq)",
         fill="Indicateur") +
    scale_fill_manual(labels = c("Demande", "Production"), values = c("indianred1", "cornflowerblue"))
  
  
  #Exporter le plot et le charger dans l'environnement
  ggsave(filename=str_c("plot.secteurs_", pays, ".",format), 
         plot=plot, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  #quelques problèmes pour save
  assign(str_c("plot.secteur_",pays),plot)
  
  rm(IO,IO_all,io_table,plot)
  
  
}

#Créer grand dataframe
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
(sum(IO_all$impact_dem)-sum(IO_all$impact_prod))/sum(IO_all$impact_dem)*100
(sum(IO_all$GES_impact_M)-sum(IO_all$GES_impact_S))/sum(IO_all$GES_impact_M)*100

#Plot mondial par secteur
monde_secteurs <- IO_all %>% 
  group_by(produits) %>%
  filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  mutate(agg.demande_impact=sum(GES_impact_M),
         agg.producteur_impact=sum(GES_impact_S),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>% 
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact"),
    names_to = "indicator",
    values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= produits, 
        y = impact,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts prodcuteur et consommateur",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production"), values = c("indianred1", "cornflowerblue"))
monde_secteurs
ggsave(filename=str_c("plot.monde_secteurs.",format), 
       plot=monde_secteurs, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)

#Plot mondial par pays
monde_pays <- IO_all %>% 
  group_by(nom_pays) %>%
  mutate(agg.demande_impact=sum(GES_impact_M),
         agg.producteur_impact=sum(GES_impact_S),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5)) %>% 
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact"),
    names_to = "indicator",
    values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= nom_pays, 
        y = impact,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=10, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts prodcuteur et consommateur",
       x ="Région ou pays", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production"), values = c("indianred1", "cornflowerblue"))
monde_pays
ggsave(filename=str_c("plot.monde_pays.",format), 
       plot=monde_pays, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)
