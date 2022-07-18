#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays

br <- "CPA2002_Niv1"
br_pays <- "EU1"
#path_loader <- str_c(path_out, br_pays,"_", br, "/")
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

rm(list = ls()[grep("^IO", ls())])

#Attention, il faut mettre "Europe" et non "Europe (autres)", sinon la sélection ne marche pas

#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y, Fe et L au préalable)
for (pays in c("Autriche","Belgique","Bulgarie","Chypre","République Tchèque","Allemagne",
               "Danemark","Estonie","Espagne","Finlande","France","Grèce","Croatie","Hongrie",
               "Irlande","Italie","Lituanie","Luxembourg","Lettonnie","Malte","Pays-bas",
               "Pologne","Portugal","Roumanie","Suède","Slovénie","Slovaquie","Royaume-Uni",
               "Reste du monde")) {
  
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
rm(IO, io_table)  
}

#rm(list = ls(pattern = "^IO_*"))
#or rm(list = ls()[grep("^IO", ls())])

#Créer grand dataframe (monde)
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
saveRDS(IO_all, str_c(path_results_tables, "IO_all_",br_pays,"_",br,".rds"))

#Plot mondial par secteur
#inutile, identique au graphinque monde12
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

#Plot européen par pays
EU_pays <- IO_all %>% 
  filter(nom_pays != "Reste du monde") %>%
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
       plot=EU_pays, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)

#plot VA pour EU
EU_secteurs_VA=IO_all %>% 
  filter(nom_pays != "Reste du monde") %>%
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
  geom_bar(stat='identity',
           position = position_stack(reverse = FALSE)) +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("E", "L","K","CP"), values = c("gray95", "gray85","gray75","gray65"))
monde_secteurs_VA
ggsave(filename=str_c("plot.monde_secteurs_va.", pays, ".",format), 
       plot=EU_secteurs_VA, 
       device="pdf",
       path=path_results_plots,
       width = 280 , height = 200 , units = "mm", dpi = 600)


#graph par groupes de pays pour faciliter les comparaisons
##groupes de pays (EuroVoc)
eastern <- c("Bulgarie","République Tchèque","Hongrie",
             "Roumanie","Pologne","Slovaquie","Slovénie","Croatie")
western <- c("Autriche","Belgique","Allemagne","France","Pays-bas",
             "Luxembourg","Royaume-Uni")
southern <- c("Chypre","Espagne","Grèce","Italie",
              "Portugal","Malte")
northern <- c("Danemark","Estonie","Lituanie","Lettonnie","Finlande","Irlande",
              "Suède")

IO_all2 <- IO_all %>%
  mutate(EU_region = ifelse(nom_pays %in% eastern, "eastern",
                            ifelse(nom_pays %in% western, "western",
                                   ifelse(nom_pays %in% southern, "southern",
                                          "northern"))))
facet_EU <- IO_all2 %>% 
  filter(nom_pays != "Reste du monde") %>%
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
  scale_fill_manual(
    labels = c("Demande", "Production","VA"), 
    values = c("indianred1", "cornflowerblue","orange1")) +
  facet_grid(~EU_region, scales="free_x")
facet_EU

graphtest = IO_all2 %>% 
  filter(nom_pays != "Reste du monde",
         produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  group_by(produits) %>%
  mutate(agg.demande_impact=sum(GES_impact_M_select),
         agg.producteur_impact=sum(GES_impact_S_select),
         agg.VA_impact=sum(impact_VA_select),
         total_impact = agg.demande_impact+agg.producteur_impact+agg.VA_impact,
         part_demande = agg.demande_impact/total_impact,
         part_producteur = agg.producteur_impact/total_impact,
         part_VA = agg.VA_impact/total_impact) %>%
  ungroup() %>%
  pivot_longer(
    cols = c("part_demande","part_producteur","part_VA"),
    names_to = "parts",
    values_to = "part_impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= produits, 
        y = part_impact,
        fill = parts)) +
  geom_bar(stat='identity',position = "stack") +
  theme(axis.text.x = element_text(angle = 25, size=10, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Région ou pays", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(
    labels = c("Demande", "Production","VA"), 
    values = c("indianred1", "cornflowerblue","orange1"))

graphtest

#quel impact est le plus important ?
part_D = IO_all2 %>% 
  filter(nom_pays != "Reste du monde",
         produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  pivot_longer(
    cols = c("GES_impact_M_select","GES_impact_S_select","impact_VA_select"),
    names_to = "cat",
    values_to = "val") %>%
  select(produits,cat,val) %>%
  group_by(produits,cat) %>%
  mutate(valeur=sum(val)) %>%
  select(-val) %>% distinct() %>% ungroup(cat) %>%
  mutate(val2 = valeur[cat == "GES_impact_M_select"]/sum(valeur)) %>%
  ungroup() %>%
  arrange(val2) %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= reorder(produits,val2), 
        y = valeur,
        fill = cat)) + #group=valeur
  geom_col(position = position_fill(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 25, size=5, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title=NULL,
       x =NULL, y = NULL,
       fill="Indicateur") +
  scale_fill_manual(
    labels = c("Demande", "Production","VA"), 
    values = c("indianred1", "cornflowerblue","orange1"))

part_P = IO_all2 %>% 
  filter(nom_pays != "Reste du monde",
         produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  pivot_longer(
    cols = c("GES_impact_S_select","GES_impact_M_select","impact_VA_select"),
    names_to = "cat",
    values_to = "val") %>%
  select(produits,cat,val) %>%
  group_by(produits,cat) %>%
  mutate(valeur=sum(val)) %>%
  select(-val) %>% distinct() %>% ungroup(cat) %>%
  mutate(val2 = valeur[cat == "GES_impact_S_select"]/sum(valeur)) %>%
  ungroup() %>%
  arrange(val2) %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= reorder(produits,val2), 
        y = valeur,
        fill = factor(cat, levels=c("GES_impact_S_select","GES_impact_M_select","impact_VA_select")))) + #group=valeur
  geom_col(position = position_fill(reverse = TRUE)) +
  theme(axis.text.x = element_text(angle = 25, size=5, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title=NULL,
       x =NULL, y = NULL,
       fill="Indicateur") +
  scale_fill_manual(
    labels = c("Production", "Demande","VA"), 
    values = c("cornflowerblue", "indianred1","orange1"))

part_VA = IO_all2 %>% 
  filter(nom_pays != "Reste du monde",
         produits != "SERVICES EXTRA-TERRITORIAUX") %>%
  pivot_longer(
    cols = c("GES_impact_M_select","GES_impact_S_select","impact_VA_select"),
    names_to = "cat",
    values_to = "val") %>%
  select(produits,cat,val) %>%
  group_by(produits,cat) %>%
  mutate(valeur=sum(val)) %>%
  select(-val) %>% distinct() %>% ungroup(cat) %>%
  mutate(val2 = valeur[cat == "impact_VA_select"]/sum(valeur)) %>%
  ungroup() %>%
  arrange(val2) %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= reorder(produits,val2), 
        y = valeur,
        fill = cat)) + #group=valeur
  geom_col(position = position_fill(reverse = FALSE)) +
  theme(axis.text.x = element_text(angle = 25, size=5, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title=NULL,
       x =NULL, y = NULL,
       fill="Indicateur") +
  scale_fill_manual(
    labels = c("Demande", "Production","VA"), 
    values = c("indianred1", "cornflowerblue","orange1")) 

#library(ggpubr)
figure=ggarrange(part_D, part_P, part_VA,
          common.legend = TRUE, labels=NULL,
          ncol = 3, nrow = 1, heights = c(6, 6, 6))

figure=annotate_figure(figure,
                top = text_grob("Parts de chaque impact par secteur", face = "bold", size = 12),
                bottom = text_grob("Secteur"),
                left = text_grob("Parts (en %)", rot = 90))

table_EU %>% 
  #filter(nom_pays != "Reste du monde") %>%
  mutate(binaire = ifelse(nom_pays == "Reste du monde",
                          "Reste",
                          "EU")) %>% 
  group_by(binaire) %>%
  mutate(agg.demande_impact=sum(GES_impact_M_select),
         agg.producteur_impact=sum(GES_impact_S_select),
         agg.VA_impact=sum(impact_VA_select)) %>%
  ungroup() %>%
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact"),
    names_to = "indicator",
    values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= binaire, 
        y = impact,
        fill = indicator)) +
  geom_col(position = "dodge") + #ou "fill"
  theme(axis.text.x = element_text(angle = 25, size=6, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Région ou pays", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production","VA"), 
                    values = c("indianred1", "cornflowerblue","orange1")) #+
  facet_grid(~binaire, scales="free_x")
