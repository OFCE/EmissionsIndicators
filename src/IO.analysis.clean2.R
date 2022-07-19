#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays

br <- "CPA2002_Niv1"
br_pays <- "monde.12"
# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_loader,"Y_",br_pays,"_",br,".rds"))
Fe <-readRDS(str_c(path_loader,"Fe_",br_pays,"_",br,".rds"))
Z <-readRDS(str_c(path_loader,"Z_",br_pays,"_",br,".rds"))
X <-readRDS(str_c(path_loader,"X_",br_pays,"_",br,".rds"))

#Calcul des coefficients techniques
##Matrice de Leontief
A <- sweep(Z, 
           MARGIN = 2,
           STATS=X$production,
           FUN='/',
           check.margin = TRUE)
A[is.na(as.data.frame(A))] <- 0
saveRDS(A, str_c(path_loader, "A_",br_pays,"_",br,".rds"))
##Inverse de Leontief
L <- LeontiefInverse(A)
saveRDS(L, str_c(path_loader, "L_",br_pays,"_",br,".rds"))

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

#Valeur ajoutée
#Sélection des variables correspondant à la VA, et somme des composantes
Fe_VA = t(Fe) %>% as.data.frame()
#VA brutes
Fe_VA$gross.VA <- apply(Fe_VA[,c(1:9)], 1, sum)
#VA nette (sans consumption of fixed capital)
Fe_VA$net.VA <- apply(Fe_VA[,c(1:5)], 1, sum)+apply(Fe_VA[,c(7:9)], 1, sum)
Fe_VA$Etat <- apply(Fe_VA[,c(1:2)], 1, sum)
Fe_VA$Travail <- apply(Fe_VA[,c(3:5)], 1, sum)
Fe_VA$Capital <- apply(Fe_VA[,c(7:9)], 1, sum)
#Toutes les composantes
Fe_VA = Fe_VA[,c(1:9,1114:1118)] %>% as.data.frame()
#Vecteur VA
VA=Fe_VA$gross.VA %>% as.data.frame

#Fonction qui donne la VA à partir des matrices production et consommations intermédiares
ValeurAjoutee.calcul <- function(prod,IO){
  CI= (t(IO) %*% Id(IO)) %>% as.data.frame()
  VA= prod-CI
  return(VA)
}
VA=ValeurAjoutee.calcul(X,Z)
#Parts de VA
VA.share=VA/sum(VA)
sum(VA.share)


#Conversion des impacts production et demande
listdf=list(S=Fe,M=M_volume)
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
    GES_list[[str_c(ges)]] <- GES_list[["GES.raw"]][id_row,] %>%  colSums() %>% as.data.frame()
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
#impact GES producteur
GES_impact_S=as.numeric(unlist(GES_impact_S)) %>% as.data.frame(row.names=rownames(Z), col.names=GES_impact_S)
#impact GES demande
GES_impact_M=as.numeric(unlist(GES_impact_M)) %>% as.data.frame(row.names=colnames(Y), col.names=GES_impact_M)
#passage de l'impact GES producteur à l'impact VA (reventilation via les parts de VA)
impact_VA = (VA.share * sum(GES_impact_S)) %>% as.data.frame(row.names=rownames(Z), col.names=impact_VA)


#Valeur ajoutée par composante: impact GES par composante
Fe_VA$rows =  rownames(Fe_VA)
Fe_VA_compo = Fe_VA %>% pivot_longer(
  cols = c("Etat","Travail","Capital","Operating surplus: Consumption of fixed capital"),
  names_to = "beneficiary",
  values_to = "value") %>% 
  mutate(share = value / sum(VA)) %>%
  select(beneficiary,share,rows) %>%
  as.data.frame() %>%
  pivot_wider(names_from = beneficiary,
              values_from = share) %>%
  select(-rows) %>% as.data.frame()
row.names(Fe_VA_compo) <- Fe_VA$rows
GES_VA_compo = Fe_VA_compo * sum(GES_impact_S)
GES_VA_compo=rename(GES_VA_compo, "Cout_production"="Operating surplus: Consumption of fixed capital")
(sum(GES_VA_compo)-sum(GES_impact_S))/sum(GES_VA_compo)*100


#Chemin pour exporter les données
dir.create(str_c(path_codedata, "results/IO_pays/", year,"/",br_pays,"_",br), recursive = TRUE)
path_results_tables <- str_c(path_codedata, "results/IO_pays/", year,"/",br_pays,"_",br,"/")
#Chemin pour exporter les plots
format = "pdf"
dir.create(str_c(path_codedata, "results/plots/", year,"/",br_pays,"_",br,"/", format), recursive = TRUE)
path_results_plots <- str_c(path_codedata, "results/plots/", year,"/",br_pays,"_",br,"/", format, "/")

#Attention, il faut mettre "Europe" et non "Europe (autres)", sinon la sélection ne marche pas

rm(list = ls()[grep("^IO", ls())])

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
  ggsave(filename=str_c("plot.secteurs_", pays, ".",format), 
         plot=plot, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  
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
  ggsave(filename=str_c("plot.secteurs.va_", pays, ".",format), 
         plot=plot2, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  
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
        y = impact/10^12,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (Gt CO2eq)",
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
        y = impact/10^12,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=10, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Région ou pays", y = "Impact GES (Gt CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production","VA"), values = c("indianred1", "cornflowerblue","orange1"))
monde_pays
ggsave(filename=str_c("plot.monde_pays.",format), 
       plot=monde_pays, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)



