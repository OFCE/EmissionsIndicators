#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays
library(ggpubr)

br <- "ThreeMe"
# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_out,"/Y_",br,".rds"))
Fe <-readRDS(str_c(path_out,"/Fe_",br,".rds"))
Z <-readRDS(str_c(path_out,"/Z_",br,".rds"))
X <-readRDS(str_c(path_out,"/X_",br,".rds"))

#Calcul des coefficients techniques
##Matrice de Leontief
A <- sweep(Z, 
           MARGIN = 2,
           STATS=X$production,
           FUN='/',
           check.margin = TRUE)
A[is.na(as.data.frame(A))] <- 0
##Inverse de Leontief
L <- LeontiefInverse(A)

#Matrice S (impact producteur)
x <- ((L %*% as.matrix(Y)) %*% Id(Y)) %>% as.numeric
x_1 <- 1/x
x_1[is.infinite(x_1)] <- 0 
x_1 <- as.numeric(x_1)
x_1d <- diag(x_1)
S <- (as.matrix(Fe) %*% x_1d) %>% `colnames<-`(rownames(X))
S[is.nan(S)]

#Matrice M (impact demande et CI)
M <- S %*% L 


#Chemin pour exporter
#dir.create(str_c(path_out, "/IO_pays"), recursive = TRUE)
path_IOpays_tables <- str_c(path_out, "IO_pays/")

#Attention, il faut mettre "Europe" et non "Europe (autres)", sinon la sélection ne marche pas

#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y, Fe et L au préalable)
for (pays in c("France","EU","US","Chine","Amerique du N.","Amerique du S.","Afrique","Russie","Europe","Asie","Moyen-Orient","Oceanie")) {
  
  #Colonne nom pays (pas nécessaire si pas rbind par la suite)
  nom_pays <- c(rep(pays,204)) #length()=204
  
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
  Fe_select <- Fe
  #Sélectionner les impacts de la production du pays en question
  Fe_select[,-str_which(colnames(Fe_select),as.character(pays))]<-0
  
  #Matrice S ("impact producteur") : impact environnemental (uniquement demande pays)
  x_1_select <- 1/production_pays
  x_1_select[is.infinite(x_1_select)] <- 0 
  x_1_select <- as.numeric(x_1_select)
  x_1d_select <- diag(x_1_select)
  S_select <- as.matrix(Fe_select) %*% x_1d_select
  #interprétation : impact de la production de ce pays (par input)
  impact_prod = t(S_select) %*% Id(t(S_select))
  
  M_select <- M
  M_select[,-str_which(colnames(M_select),as.character(pays))]<-0
  #ou sinon filtrer les colonnes de L
  impact_dem = t(M_select) %*% Id(t(M_select))
  
  #Sélection des impacts GES et Conversion en CO2eq
  listdf=list(S=S_select,M=M_select)
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
  saveRDS(io_table, str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  
  #Créer un graphique
  plot=IO %>% 
    group_by(produits) %>%
    filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>%
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
  
  
  #Exporter le plot (pdf) et le charger dans l'environnement
  ggsave(filename=str_c("plot.secteurs_", pays, ".pdf"), 
         plot=plot, 
         device="pdf",
         path=path_IOpays_tables,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  #quelques problèmes pour save
  assign(str_c("plot.secteur_",pays),plot)
  
  rm(IO,IO_all,io_table,plot)
  
  
}

#Créer grand dataframe
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))

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
ggsave(filename=str_c("plot.monde.secteurs_", pays, ".pdf"), 
       plot=monde_secteurs, 
       device="pdf",
       path=path_IOpays_tables,
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
  theme(axis.text.x = element_text(angle = 25, size=6, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts prodcuteur et consommateur",
       x ="Région ou pays", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production"), values = c("indianred1", "cornflowerblue"))
monde_pays
ggsave(filename=str_c("plot.monde.pays_", pays, ".pdf"), 
       plot=monde_pays, 
       device="pdf",
       path=path_IOpays_tables,
       width = 280 , height = 200 , units = "mm", dpi = 600)
