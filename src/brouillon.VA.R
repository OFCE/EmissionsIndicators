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

#contribution de chaque secteur à la DF: 
#colonne Y divisée par somme colonne (total de chaque DF)
Y_sectors.tot<-colSums(Y)
y_2 <- 1/Y_sectors.tot
y_2[is.infinite(y_2)] <- 0 
y_2d <- as.numeric(y_2) %>% diag
Y_sectors.share <- as.matrix(Y) %*% y_2d
M_vol.dim <- M_volume %*% t(Y_sectors.share)

#Valeur ajoutée
Fe_VA = t(Fe) %>% as.data.frame()
Fe_VA$gross.VA <- apply(Fe_VA[,c(1:9)], 1, sum)
Fe_VA$net.VA <- apply(Fe_VA[,c(1:5)], 1, sum)+apply(Fe_VA[,c(7:9)], 1, sum)
Fe_VA$Etat <- apply(Fe_VA[,c(1:2)], 1, sum)
Fe_VA$Travail <- apply(Fe_VA[,c(3:5)], 1, sum)
Fe_VA$Capital <- apply(Fe_VA[,c(7:9)], 1, sum)
Fe_VA = Fe_VA[,c(1:9,1114:1118)] %>% as.data.frame()
VA=Fe_VA$gross.VA %>% as.data.frame


#Vérification VA = PIB = Somme des demandes
(sum(Y)-sum(Fe_VA$gross.VA))/sum(Y)*100

#Passage production-VA via la matrice des coefficients techniques?
CI= (t(Z) %*% Id(Z)) %>% as.data.frame() #somme de Z par colonne
#donne les CI de chaque secteur

#Passage à la VA
VA.alt = X-CI

#Fonction qui donne la VA à partir des matrices production et consommations intermédiares
ValeurAjoutee.calcul <- function(prod,IO){
  CI= (t(IO) %*% Id(IO)) %>% as.data.frame()
  VA= prod-CI
  return(VA)
}
VA=ValeurAjoutee.calcul(X,Z)
VA.share=VA/sum(VA)
sum(VA.share)

#Valeur ajoutée par composante
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


#Conversion
listdf=list(S=Fe,M=M_vol.dim)
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
GES_impact_S=as.numeric(unlist(GES_impact_S)) %>% as.data.frame(row.names=rownames(Z), col.names=GES_impact_S)
GES_impact_M=as.numeric(unlist(GES_impact_M)) %>% as.data.frame(row.names=rownames(Z), col.names=GES_impact_M)
impact_VA = (VA.share * sum(GES_impact_S)) %>% as.data.frame(row.names=rownames(Z), col.names=impact_VA)
sum(GES_impact_S)==sum(GES_impact_M)
sum(GES_impact_M)==sum(impact_VA)

#Chemin pour exporter les données
dir.create(str_c(path_codedata, "results/IO_pays/", year,"/",br_pays,"_",br,"/test"), recursive = TRUE)
path_results_tables <- str_c(path_codedata, "results/IO_pays/", year,"/",br_pays,"_",br,"/test/")
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
  GES_impact_M_select <- GES_impact_M
  #Sélectionner les impacts de la production du pays en question
  GES_impact_M_select[-str_which(rownames(GES_impact_M_select),as.character(pays)),]<-0
  GES_impact_M_select = GES_impact_M_select%>%unlist()%>%as.numeric()
  
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
  
  #Créer un graphique
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
  
    plot +
      geom_bar(data=test,
               aes(x=produits, y=impact, fill=composante), 
               position="dodge", stat="identity", alpha=0.2) +
      scale_fill_manual(labels = c("E", "L","K","CP"), values = c("gray95", "gray85","gray75","gray65"))
    
  
  #Exporter le plot et le charger dans l'environnement
  ggsave(filename=str_c("plot.secteurs_", pays, ".",format), 
         plot=plot, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  ggsave(filename=str_c("plot.secteurs.va_", pays, ".",format), 
         plot=plot2, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)

  assign(str_c("plot.secteur_",pays),plot)
  assign(str_c("plot.va_",pays),plot2)
  
  rm(IO,IO_all,io_table,plot)
  
  
}

plotdata=IO_Oceanie %>% 
  #par produits
  group_by(produits) %>%
  filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
  mutate(agg.demande_impact=sum(GES_impact_M_select),
         agg.producteur_impact=sum(GES_impact_S_select),
         agg.VA_impact=sum(impact_VA_select),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot),
         agg.Etat=sum(Etat),
         agg.Travail=sum(Travail),
         agg.Capital=sum(Capital),
         agg.Cout=sum(Cout_production)) %>%
  ungroup() %>%
  #format long pour afficher les deux indicateurs
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact",
             "agg.Etat","agg.Travail","agg.Capital","agg.Cout"),
    names_to = "indicator",
    values_to = "impact") %>%
  mutate(ind=ifelse(indicator=="agg.Etat"|indicator=="agg.Travail"|indicator=="agg.Capital"|indicator=="agg.Cout",
                    "VA_comp",indicator)) 
plot= plotdata %>%
  ggplot( 
    ) +
  geom_bar(aes(x= produits, 
               y = impact,
               fill = factor(indicator),
               group=factor(ind) 
               ),
           position=position_dodge(width=NULL),
           stat='identity') +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual( 
                    values = c("gray75", "gray65","indianred1",
                               "gray95", "cornflowerblue","gray85","orange1"))
plot

#Créer grand dataframe
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
(sum(IO_all$impact_dem)-sum(IO_all$impact_prod))/sum(IO_all$impact_dem)*100
(sum(IO_all$GES_impact_M)-sum(IO_all$GES_impact_S))/sum(IO_all$GES_impact_M)*100
(sum(IO_all$GES_impact_M)-sum(IO_all$impact_VA_select))/sum(IO_all$GES_impact_M)*100

#vérification VA
IO_all$impact_VA_select==rowSums(IO_all[,9:12])

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



