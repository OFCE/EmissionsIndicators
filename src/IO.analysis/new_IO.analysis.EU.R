#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays


br <- "CPA2002_Niv1"
br.pays <- "EU1"
br_pays <- "EU1"
path_loader <- str_c(path_out, br_pays,"_", br, "/")
# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_loader,"Y_",br_pays,"_",br,".rds"))
Fe <-readRDS(str_c(path_loader,"Fe_",br_pays,"_",br,".rds"))
Z <-readRDS(str_c(path_loader,"Z_",br_pays,"_",br,".rds"))
X <-readRDS(str_c(path_loader,"X_",br_pays,"_",br,".rds"))

# Données Eurostat (PIB/hab et Population des pays)
pib=read_xlsx(str_c(path_data.source,"eurostat/PIBhab.xlsx"),
              sheet="Feuille 1",
              skip=8,
              col_names = TRUE)
pib=pib[-c(1,41:49),c("TIME",year)]
pib=setnames(pib,old=c("TIME",year),new=c("nom_pays","PIB.hab"))
pib[pib$nom_pays=="Allemagne (jusqu'en 1990, ancien territoire de la RFA)",]$nom_pays = "Allemagne"
pib[pib$nom_pays=="Tchéquie",]$nom_pays = "République Tchèque"
pib[pib$nom_pays=="Pays-Bas",]$nom_pays = "Pays-bas"
#changer pour "lettonnie" en attendant de corriger la coquille
pib[pib$nom_pays=="Lettonie",]$nom_pays = "Lettonnie"

pop=read_xlsx(str_c(path_data.source,"eurostat/population.xlsx"),
              sheet="Feuille 1",
              skip=6,
              col_names = TRUE)
pop=pop[-c(1,56:66),c("TIME",year)]
pop=setnames(pop,old=c("TIME",year),new=c("nom_pays","population"))
pop$population = as.numeric(pop$population)
pop[pop$nom_pays=="Allemagne (jusqu'en 1990, ancien territoire de la RFA)",]$nom_pays = "Allemagne"
pop[pop$nom_pays=="Tchéquie",]$nom_pays = "République Tchèque"
pop[pop$nom_pays=="Pays-Bas",]$nom_pays = "Pays-bas"
#pareil
pop[pop$nom_pays=="Lettonie",]$nom_pays = "Lettonnie"

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

#Valeur ajoutée
#Sélection des variables correspondant à la VA, et somme des composantes
Fe_VA=Composantes.VA(Fe)
VA=ValeurAjoutee.calcul(X,Z)

#Impacts
S_coef=divide(Vect=as.vector(X),Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = FALSE,volume=FALSE)
S.VA_coef=divide(Vect=VA,Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = FALSE,volume=FALSE)
M_coef=divide(Vect=as.vector(X),Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = TRUE,volume=FALSE)
S_volume=divide(Vect=as.vector(X),Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = FALSE,volume=TRUE)
S.VA_volume=divide(Vect=VA,Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = FALSE,volume=TRUE)
M_volume=divide(Vect=as.vector(X),Fe.mat=Fe,Y.mat=Y,L.mat=L,demand = TRUE,volume=TRUE)

##à modifier avec fonction?
listdf=list(S_coef=S_coef,S_vol=S_volume,M_coef=M_coef,M_vol=M_volume,S.VA_coef=S.VA_coef,S.VA_vol=S.VA_volume)
index=1
for (matrix in listdf) {
  GES_list <- list()
  GES_list[["GES.raw"]] <- matrix %>% 
    as.data.frame %>% 
    filter(str_detect(row.names(.), "CO2") | str_detect(row.names(.), "CH4") | 
             str_detect(row.names(.), "N2O") | str_detect(row.names(.), "SF6") | 
             str_detect(row.names(.), "PFC") | str_detect(row.names(.), "HFC") )
  for (ges in glist){
    #Row number for each GES in the matrix
    id_row <- str_which(row.names(GES_list[["GES.raw"]]),str_c(ges))
    GES_list[[str_c(ges)]] <- GES_list[["GES.raw"]][id_row,] %>%  colSums() %>% as.data.frame()
    GES_list[[ges]] <- GHGToCO2eq(GES_list[[ges]])
  }
  GES_list[["GES"]] <- GES_list[["CO2"]] + GES_list[["CH4"]] +
    GES_list[["N2O"]] + GES_list[["SF6"]] + GES_list[["HFC"]] +
    GES_list[["PFC"]]
  df.impact=as.numeric(unlist(GES_list[["GES"]])) %>% 
    as.data.frame(row.names=rownames(Z), 
                  col.names=str_c("GES_impact_",names(listdf)[index]))
  assign(str_c("GES_impact_",names(listdf)[index]), 
         df.impact)
  saveRDS(df.impact, str_c(path_loader,"GES_impact_",names(listdf)[index],".rds"))
  index=index+1
}

Fe_VA_compo=Fe_VA[,c(6,12:14)]/rowSums(Fe_VA[,c(6,12:14)]) 
GES_VA_compo = Fe_VA_compo * rowSums(GES_impact_S.VA_vol)
GES_VA_compo[is.na(GES_VA_compo)] <- 0
GES_VA_compo=rename(GES_VA_compo, "Cout_production"="Operating surplus: Consumption of fixed capital")

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
  DF_tot=shock.demand(as.matrix(Y) %*% Id(Y),iso=pays)
  #interprétation : quantité consommée par ce pays et produite dans le monde
  
  #Vecteur production du pays
  production_pays <- as.numeric(unlist(shock.demand(X,iso=pays)))
  
  #Vecteur VA du pays
  VA_pays <- as.numeric(unlist(shock.demand(VA,iso=pays)))
  
  #Impacts producteur, VA, demande du pays, en volume et coef
  GES_impact_S_coef_select <- as.numeric(unlist(shock.demand(GES_impact_S_coef,iso=pays)))
  GES_impact_S_vol_select <- as.numeric(unlist(shock.demand(GES_impact_S_vol,iso=pays)))
  GES_impact_M_coef_select <- as.numeric(unlist(shock.demand(GES_impact_M_coef,iso=pays)))
  GES_impact_M_vol_select <- as.numeric(unlist(shock.demand(GES_impact_M_vol,iso=pays)))
  GES_impact_S.VA_coef_select <- as.numeric(unlist(shock.demand(GES_impact_S.VA_coef,iso=pays)))
  GES_impact_S.VA_vol_select <- as.numeric(unlist(shock.demand(GES_impact_S.VA_vol,iso=pays)))
  #par composante
  GES_VA_compo_select = shock.demand(GES_VA_compo,iso=pays)
  
  #Créer le tableau en assemblant les colonnes
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production_pays,VA_pays,
                    GES_impact_S_coef_select,GES_impact_S_vol_select,
                    GES_impact_M_coef_select,GES_impact_M_vol_select,
                    GES_impact_S.VA_coef_select,GES_impact_S.VA_vol_select,
                    GES_VA_compo_select
         )
  )
  
  #Créer une colonne produits, ordonner les colonnes, nettoyer le dataframe
  io_table$pays.produits=rownames(io_table)
  io_table$produits=sub(".*?_", "",io_table$pays.produits)
  io_table$regions=sub("_.*", "",io_table$pays.produits)
  io_table = io_table %>% 
    select(regions,nom_pays,produits,DF_tot,production_pays,VA_pays,
           GES_impact_S_coef_select,GES_impact_S_vol_select,
           GES_impact_M_coef_select,GES_impact_M_vol_select,
           GES_impact_S.VA_coef_select,GES_impact_S.VA_vol_select,
           Etat,Travail,Capital,Cout_production)
  io_table[sapply(io_table, simplify = 'matrix', is.infinite)] <- 0
  io_table[sapply(io_table, simplify = 'matrix', is.nan)] <- 0
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_results_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_results_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  saveRDS(IO, str_c(path_results_tables, "IO_",pays,"_",br_pays,"_",br,".rds"))
  
  #Ajouter données eurostat
  IO_ponderation=merge(IO,pib,by.x="nom_pays")
  IO_ponderation=merge(IO_ponderation,pop,by.x="nom_pays")
  assign(str_c("IO_ponderation_",pays),IO_ponderation)
  saveRDS(IO_ponderation, str_c(path_results_tables, "IO_ponderation_",pays,"_",br_pays,"_",br,".rds"))
  
  #Créer un graphique avec les trois indicateurs
  plot=IO %>% 
    #par produits
    group_by(produits) %>%
    filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
    mutate(agg.demande_impact=sum(GES_impact_M_vol_select),
           agg.producteur_impact=sum(GES_impact_S_vol_select),
           agg.VA_impact=sum(GES_impact_S.VA_vol_select),
           agg.production=sum(production_pays),
           agg.demande_finale=sum(DF_tot),
           agg.VA=sum(VA_pays)) %>%
    ungroup() %>%
    #format long pour afficher les deux indicateurs
    pivot_longer(
      cols = c("agg.producteur_impact","agg.demande_impact","agg.VA_impact"),
      names_to = "indicator",
      values_to = "impact") %>%
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
    labs(title="Impact environnemental par secteur",
         x ="Secteurs", y = "Impact GES (Gt CO2eq)",
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
          y = impact/10^12,
          fill = composante)) +
    geom_bar(stat='identity',position = position_stack(vjust = 1, reverse = FALSE)) +
    theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
          plot.title =element_text(size=12, face='bold', hjust=0.5),
          panel.background = element_blank(),
          panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
          plot.margin = unit(c(10,5,5,5), "mm"))+
    labs(title="Décomposition de l'impact environnemental par secteur",
         x ="Secteurs", y = "Impact GES (Gt CO2eq)",
         fill="Rémunération de :") +
    scale_fill_manual(labels = c("Taxes", "Travail","Capital (net)","Capital (dépréciation)"), 
                      values = c("yellow", "tomato","slateblue1","slateblue"))
  ggsave(filename=str_c("plot.secteurs.va_", pays, ".",format), 
         plot=plot2, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  
  radar.data=IO %>%
    group_by(produits) %>%
    mutate(agg.demande_impact=mean(GES_impact_M_coef_select),
           agg.producteur_impact=mean(GES_impact_S_coef_select),
           agg.VA_impact=mean(GES_impact_S.VA_coef_select)) %>%
    ungroup() %>%
    distinct(produits,agg.demande_impact,agg.producteur_impact,agg.VA_impact)
  radar_data.secteurs=radar.data$produits
  radar.data=radar.data%>%
    select(-produits) %>%  t() %>% 
    as.data.frame() %>%
    `colnames<-`(radar_data.secteurs) %>%
    add_rownames( var = "group" )
  plot3 = ggradar(radar.data,
                  axis.labels=gsub('\\s','\n',colnames(radar.data[,-1])),
                  axis.label.size = 2,
                  axis.label.offset = 1.1,
                  grid.min = 0,
                  grid.mid = max(radar.data[,-1])/2,
                  grid.max = max(radar.data[,-1]),
                  grid.line.width=0.1,
                  values.radar = c("0", floor(max(radar.data[,-1])/2), floor(max(radar.data[,-1]))),
                  grid.label.size = 4,
                  gridline.label.offset = 0.3 * (max(radar.data[,-1])+150),
                  label.gridline.min = FALSE,
                  gridline.min.colour="gray",
                  gridline.min.linetype="longdash",
                  label.gridline.mid = TRUE,
                  gridline.mid.colour="gray",
                  gridline.mid.linetype="longdash",
                  label.gridline.max = TRUE,
                  gridline.max.colour="gray",
                  gridline.max.linetype="longdash",
                  group.line.width = 0.5,
                  group.point.size = 1,
                  background.circle.transparency=0,
                  legend.title = "Indicateur",
                  legend.text.size = 10,
                  fill=TRUE,
                  fill.alpha = 0.25,
                  plot.title = str_c("Coefficient environnemental pour : ", pays))+
    theme(legend.title = element_text(size=12)) +
    scale_fill_manual(labels = c("Demande", "Production","VA"), #
                      values = c("indianred1", "cornflowerblue","orange1")) +
    scale_colour_manual(labels = c("Demande", "Production","VA"), #
                        values = c("indianred1", "cornflowerblue","orange1")) +
    guides(fill="none")
  assign(str_c("radar.plot.secteurs_",pays),plot3)
  ggsave(filename=str_c("radar.plot.secteurs_", pays, ".",format), 
         plot=plot3, 
         device="pdf",
         path=path_results_plots,
         width = 280 , height = 200 , units = "mm", dpi = 600)
  
  rm(IO, io_table, IO_all, radar.data, plot, plot2, plot3)  
}

IO_ponderation_all <- do.call("rbind",mget(ls(pattern = "^IO_ponderation*")))
saveRDS(IO_ponderation_all, str_c(path_results_tables, "IO_ponderation_all_",br_pays,"_",br,".rds"))
rm(list = ls()[grep("^IO_ponderation", ls())])
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
saveRDS(IO_all, str_c(path_results_tables, "IO_all_",br_pays,"_",br,".rds"))

EU_secteurs <- table_EU_ponderation %>% 
  filter(produits != "SERVICES EXTRA-TERRITORIAUX", 
         nom_pays != "Reste du monde") %>% 
  group_by(produits) %>% 
  mutate(agg.producteur_impact=sum(GES_impact_S_vol_select),
         agg.demande_impact=sum(GES_impact_M_vol_select),
         agg.VA_impact=sum(GES_impact_S.VA_vol_select),
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

###############
#Créer grand dataframe (monde)
IO_ponderation_all <- readRDS(str_c(path_results_tables, "IO_ponderation_all_",br_pays,"_",br,".rds"))

