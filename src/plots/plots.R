#Chemin pour exporter les plots
format = "pdf"
dir.create(str_c(path_codedata, "results/plots/", year,"/",br_pays,"_",br,"/", format, "/test"), recursive = TRUE)
path_results_plots <- str_c(path_codedata, "results/plots/", year,"/",br_pays,"_",br,"/", format, "/test/")

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
  
  #Impacts producteur du pays
  GES_impact_S_select <- GES_impact_S
  #Sélectionner les impacts de la production du pays en question
  GES_impact_S_select[-str_which(rownames(GES_impact_S_select),as.character(pays)),]<-0
  GES_impact_S_select = GES_impact_S_select%>%unlist()%>%as.numeric()
  
  #Impacts demande du pays
  #GES_impact_M_select <- GES_impact_M
  #####GES_impact_M_select[-str_which(rownames(GES_impact_M_select),as.character(pays)),]<-0
  
  #contribution de chaque secteur à la DF de ce pays: 
  #colonne Y divisée par somme colonne (total de chaque DF)
  Y_select=Y
  Y_select[,-str_which(colnames(Y_select),as.character(pays))]<-0
  Y_sectors.tot<-colSums(Y_select)
  y_2 <- 1/Y_sectors.tot
  y_2[is.infinite(y_2)] <- 0 
  y_2d <- as.numeric(y_2) %>% diag
  Y_sectors.share <- as.matrix(Y) %*% y_2d
  
  GES_impact_M_select = GES_impact_M
  GES_impact_M_select[,-str_which(colnames(GES_impact_M_select),as.character(pays))]<-0
  GES_impact_M_select = t(GES_impact_M_select) %*% t(Y_sectors.share)
  GES_impact_M_select=GES_impact_M_select%>%unlist()%>%as.numeric()
  
  #Impacts VA du pays
  impact_VA_select <- impact_VA
  #Sélectionner les impacts de la production du pays en question
  impact_VA_select[-str_which(rownames(impact_VA_select),as.character(pays)),]<-0
  impact_VA_select = impact_VA_select%>%unlist()%>%as.numeric()
  
  #par composante
  GES_VA_compo_select = GES_VA_compo
  GES_VA_compo_select[-str_which(rownames(GES_VA_compo_select),as.character(pays)),]<-0
  
  
  
  #Créer le tableau en assemblant les colonnes
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production_pays,GES_impact_S_select,GES_impact_M_select,impact_VA_select,GES_VA_compo_select
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
    select(regions,nom_pays,produits,DF_tot,production_pays,GES_impact_S_select,GES_impact_M_select,impact_VA_select,Etat,Travail,Capital,Cout_production)
  io_table[sapply(io_table, simplify = 'matrix', is.infinite)] <- 0
  io_table[sapply(io_table, simplify = 'matrix', is.nan)] <- 0
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_results_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_results_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  
  #Créer un graphique avec les trois indicateurs
  plot=IO %>% 
    #par produits
    group_by(produits) %>%
    filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
    mutate(agg.demande_impact=sum(GES_impact_M_select),
           agg.producteur_impact=sum(GES_impact_S_select),
           agg.VA_impact=sum(impact_VA_select),
           agg.production=sum(production_pays),
           agg.demande_finale=sum(DF_tot)) %>%
    ungroup() %>%
    #format long pour afficher les deux indicateurs
    pivot_longer(
      cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact"),
      names_to = "indicator",
      values_to = "impact") %>%
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
    labs(title="Impacts",
         x ="Secteurs", y = "Impact GES (CO2eq)",
         fill="Indicateur") +
    scale_fill_manual(labels = c("Demande", "Production","VA"), values = c("indianred1", "cornflowerblue","orange1"))
  
  #Créer un graphique décomposition de la VA
  plot2=IO %>% 
    #par produits
    group_by(produits) %>%
    filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
    mutate(agg.Etat=sum(Etat),
           agg.Travail=sum(Travail),
           agg.Capital=sum(Capital),
           agg.Cout=sum(Cout_production)) %>%
    ungroup() %>%
    #format long pour afficher les deux indicateurs
    pivot_longer(
      cols = c("agg.Etat","agg.Travail","agg.Capital","agg.Cout"),
      names_to = "composante",
      values_to = "impact") %>%
    ggplot( 
      aes(x= produits, 
          y = impact,
          fill = composante)) +
    geom_bar(stat='identity',position = position_stack(vjust = 1, reverse = FALSE)) +
    theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
          plot.title =element_text(size=12, face='bold', hjust=0.5),
          panel.background = element_blank(),
          panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
          plot.margin = unit(c(10,5,5,5), "mm"))+
    labs(title="Impacts",
         x ="Secteurs", y = "Impact GES (CO2eq)",
         fill="Indicateur") +
    scale_fill_manual(labels = c("E", "L","K","CP"), values = c("gray95", "gray85","gray75","gray65"))
  
  
}

#Créer grand dataframe (monde)
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))

#Plot mondial par secteur
monde_secteurs <- IO_all %>% 
  group_by(produits) %>%
  filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  mutate(agg.demande_impact=sum(GES_impact_M_select),
         agg.producteur_impact=sum(GES_impact_S_select),
         agg.VA_impact=sum(impact_VA_select),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>% 
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact"),
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
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production","VA"), values = c("indianred1", "cornflowerblue","orange1"))
monde_secteurs
ggsave(filename=str_c("plot.monde_secteurs.",format), 
       plot=monde_secteurs, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)

#Plot mondial par pays
monde_pays <- IO_all %>% 
  group_by(nom_pays) %>%
  mutate(agg.demande_impact=sum(GES_impact_M_select),
         agg.producteur_impact=sum(GES_impact_S_select),
         agg.VA_impact=sum(impact_VA_select),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5)) %>% 
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact"),
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
  labs(title="Impacts",
       x ="Région ou pays", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production","VA"), values = c("indianred1", "cornflowerblue","orange1"))
monde_pays
ggsave(filename=str_c("plot.monde_pays.",format), 
       plot=monde_pays, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)



