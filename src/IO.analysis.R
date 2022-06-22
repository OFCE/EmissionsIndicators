#Rappel du calcul IO 
#Égalité comptable de base 
#X = CI + Y 
#X - CI = Y
#(1-a)X = Y avec (1-a)^1 = L
#X = L.Y 

br <- "ThreeMe"

# Chargement des données I-O sauvegardées par le script exio3.loader.R
#A <-readRDS(str_c(path_out,"/A_",br,".rds"))
Y <-readRDS(str_c(path_out,"/Y_",br,".rds"))
Fe <-readRDS(str_c(path_out,"/Fe_",br,".rds"))

Z <-readRDS(str_c(path_out,"/Z_",br,".rds"))
X <-readRDS(str_c(path_out,"/X_",br,".rds"))

#S <-readRDS(str_c(path_out,"/S_",br,".rds"))
#M <-readRDS(str_c(path_out,"/M_",br,".rds")) #S_Y
Fy <-readRDS(str_c(path_out,"/Fy_",br,".rds"))

#Vérification de l'équation comptable:
(sum(Y)+sum(Z)-sum(X))/sum(X)*100 #une toute petite erreur (1.654953e-12%)

#Ne fonctione pas sur les impacts... S n'est pas l'équivalent de S_Y?
(sum(M)-sum(S))/sum(M)*100
#ici non plus
(sum(Fe)-sum(Fy))/sum(Fe)*100

#Calcul de A (marche aussi avec d'autres fonctions, résultats identiques)
A.alternative <- sweep( Z , 
                        MARGIN = 2 , 
                        STATS=X$production , 
                        FUN='/' ,
                        check.margin = TRUE)
A.alternative[is.na(as.data.frame(A.alternative))] <- 0 
sum(A.alternative)

## Calcul de L: matrice de Leontief
#L <- LeontiefInverse(A)
L <- LeontiefInverse(A.alternative)


#fonction valeurs négatives
valeurs.negatives<-function(dataframe){ #donne le nombre de valeurs négatives dans le df
  has.neg <- apply(dataframe, 1, function(row) any(row < 0))
  return(length(which(has.neg)))
}

#(sum(L)-sum(L.alternative))/sum(L) * 100 #29%
valeurs.negatives(L)
#valeurs.negatives(L.alternative)
valeurs.negatives(Z)
valeurs.negatives(X)
#

print("Computation of the Leontief matrix (L) : done")


Y_pays_types_DF<-colnames(Y)
y_tot<- as.matrix(Y) %*% as.matrix(rep(1,length(Y_pays_types_DF)))
rm(Y_pays_types_DF)
rm(Y)
#y_tot : demande totale par région et secteur


## calcul de S
#x (production totale (pour CI + pour DF))
x <- (L %*% y_tot) %>% as.numeric()#%>% as.matrix() %>% t()
X.calc <- (L %*% as.matrix(Y)) 
(sum(X)-sum(X.calc))/sum(X) * 100 #petite erreur (-8.318899e-12%)


#Checks sur les valeurs (un peu répétitif)
#A SUPPRIMER PUISQUE OK
#########
#test pour y
I <- diag(rep(1, dim(A.alternative)[1]))
checkY=as.matrix((I-A.alternative))%*%as.matrix(X)
sum(checkY)==sum(Y)
(sum(Y)-sum(checkY))/sum(Y) *100 #ok (et Y n'est pas dans le calcul de de checkY)

#test pour x
checkX= as.matrix(L) %*% as.matrix(Y)
(sum(X)-sum(L %*% as.matrix(Y)))/sum(X)*100 #ok

checkX= as.matrix(L) %*% (y_tot)
sum(x)==sum(checkX)
sum(x)-sum(checkX) 
(sum(x)-sum(checkX))/sum(x) *100 #ok
#########

#print("J'ai calcule le vecteur de la production totale x. Je vais pouvoir calculer S=Fxdiag(1/x)")

x_1 <- 1/x
x_1[is.infinite(x_1)] <- 0 
x_1 <- as.numeric(x_1)
x_1d <- diag(x_1)


S <- (as.matrix(Fe) %*% x_1d) %>% `colnames<-`(rownames(X))
S[is.nan(S)]
sum(S)

valeurs.negatives(Fe)
valeurs.negatives(S)
#3 valeurs négatives dans S (car 3 dans Fe, et 0 dans X)

# Exportation des résultats sous forme de fichier Rds des différentes matrices calculées
saveRDS( A.alternative, str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/A.aggrege.rds"))
saveRDS( x, str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/x.aggrege.rds"))
saveRDS( S, str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/S.aggrege.rds"))
saveRDS( L, str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/L.aggrege.rds"))




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
#Suggestion plus courte: 
for (ges in glist){GES_list[[ges]] <- GHGToCO2eq(GES_list[[ges]])}

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


#Impact de la demande
M <- as.matrix(S) %*% L 
#interprétation : impacts des inputs d'un secteur-pays
View(M)
valeurs.negatives(M)

dim(M)
#dimensions de M.calc: impacts * pays_secteurs
#interprétation: impacts de tous les inputs des produits donnés en colonnes

#pour avoir une matrice impacts * DF:
M.alternative <- as.matrix(S) %*% as.matrix(Y)
#pour avoir une matrice impacts * secteurs (impacts par unité demandée):
M.alternative2 <- sweep( Fe , 
                          MARGIN = 2 , 
                          STATS=y_tot , 
                          FUN='/' ,
                          check.margin = TRUE)
M.alternative2[is.na(as.data.frame(M.alternative2))] <- 0 
sum(M.alternative2)
sum(S)


#Calcul de M (impact demande) à partir de Fy et pas S (donc Fe)
##colonne F_Y / demandes totales du pays (vecteur de taille 84)


sum(colSums(M==0))
sum(colSums(M.alternative==0))
sum(colSums(M.alternative2==0))
#meme nombre de 0 dans M et M.calc2


#IGNORER CE BLOC
######################
#contribution de chaque secteur à la DF: 
#colonne Y divisée par somme colonne (total de chaque DF)
Y_sectors.tot<-colSums(Y)
y_2 <- 1/Y_sectors.tot
y_2[is.infinite(y_2)] <- 0 
y_2 <- as.numeric(y_2)
y_2d <- diag(y_2)
Y_sectors.share <- as.matrix(Y) %*% y_2d
#contribution de chaque secteur à chaque impact: 
#impact*case du secteur correspondant (pour un type de DF donné)
#(remplacer les 0 par des 1 pour n'avoir que des 1 dans colSums(Y_sectors.share))

#pour aggréger par pays ? inutile pour le moment
M.calc <- t(M.calc) %>% as.data.frame() %>% mutate(pays_demande=rownames(.))
M.calc$demande=sub(".*?_", "",M.calc$pays_demande)
M.calc$regions=sub("_.*", "",M.calc$pays_demande)
M.calc.agg<-M.calc %>% group_by(regions) %>% 
  summarise_if(is.numeric, sum) 
regions=M.calc.agg$regions
M.calc.agg=M.calc.agg%>% `rownames<-`(regions) %>% t() %>% as.data.frame()
M.calc.agg=M.calc.agg[-1,]
M.calc=t(M.calc)%>%as.data.frame()
##########################


#Sélection des impacts GES et conversion
for (ges in c(glist,"GES")){
  M.mat=M
  M.mat <-  GES_list[[str_c(ges)]] %>% unlist %>% as.numeric %>% diag
  
  
  GES_list[[str_c("M.",ges)]]  <-  M.mat %*% L 
  GES_list[[str_c("M.",ges)]][is.nan(GES_list[[str_c("M.",ges)]])]
  GES_list[[str_c("M.",ges)]] <-   GES_list[[str_c("M.",ges)]] %>% as.data.frame() %>% `rownames<-`(rownames(L))

  
  
  saveRDS(GES_list[[str_c("M.",ges)]], str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/M.",ges,".rds"))
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
path_IOpays_tables <- str_c(path_out, "/IO_pays")

checklist_demande <- list()

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
  #check: sum(Y[,str_which(colnames(Y),as.character(pays))])==sum(DF_tot)
  #...TRUE (Russie presque True)
  checklist_demande[pays] <- sum(Y[,str_which(colnames(Y),as.character(pays))])==sum(DF_tot)
  
  #Vecteur production (identique au vecteur monde)
  ##x (production totale (pour CI + pour DF))
  production <- (L %*% DF_tot) %>% as.numeric
  #interprétation : quantité de chaque input (produits en ligne) nécessaire pour produire (dans le monde) une unité d'output demandée dans ce pays
  
  #test autre façon de calculer la production
  #(au lieu de prendre L* DF pays, prendre les outputs du pays dans L et multiplier par la demande mondiale: L_select*y_tot)
  L_select=L
  L_select[,-str_which(colnames(L_select),as.character(pays))]<-0
  production_2 <- (as.matrix(L_select) %*% y_tot) %>% as.numeric
  #interprétation : quantité de chaque input (produits en ligne) nécessaire pour produire dans ce pays une unité d'output consommée dans le monde
  
  production_3 <- X
  production_3[-str_which(rownames(production_3),as.character(pays)),]<-0
  production_3=as.numeric(unlist(production_3))
  
  Fe_select <- Fe
  Fe_select[,-str_which(colnames(Fe_select),as.character(pays))]<-0
  
  #Matrice S ("impact producteur") : impact environnemental (uniquement demande pays)
  x_1_select <- 1/production_2
  x_1_select[is.infinite(x_1_select)] <- 0 
  x_1_select <- as.numeric(x_1_select)
  x_1d_select <- diag(x_1_select)
  S_select <- as.matrix(Fe_select) %*% x_1d_select
  #interprétation : impact de la production de ce pays (par input)
  impact_prod <- t(S_select) %*% Id(t(S_select))
  
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
  
  #Vecteur équivalent à M ("impact demande finale" du pays)
  ##(donc déjà en CO2eq)
  M_select <- sweep( Fe , 
                     MARGIN = 2 , 
                     STATS=DF_tot , 
                     FUN='/' ,
                     check.margin = TRUE)
  M_select[is.na(as.data.frame(M_select))] <- 0 
  #interprétation : impact de la demande de ces produits par le pays en question
  impact_demande <- t(M_select) %*% Id(t(M_select))
  
  #Conversion
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
  GES_impact_S=as.numeric(unlist(GES_impact_S))
  GES_impact_M=as.numeric(unlist(GES_impact_M))

  
  #Créer le tableau en assemblant les colonnes
  GES_impact_producteur=as.numeric(unlist(GES_impact_prod))
  #GES_impact_demande=as.numeric(unlist(GES_impact_DF))
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production,production_2,production_3,impact_prod,impact_demande, GES_impact_S, GES_impact_M #GES_impact_producteur,GES_impact_demande
           )
         )
  
  #Mettre à 0 la production pour les autres pays (tableau spécifique à un seul pays)
  #io_table$production[-str_which(rownames(io_table),as.character(pays)),]<-0
  #-> fait automatiquement avec production_2
  
  #Créer une colonne produits, ordonner les colonnes
  io_table$pays.produits=rownames(io_table)
  io_table$produits=sub(".*?_", "",io_table$pays.produits)
  io_table$regions=sub("_.*", "",io_table$pays.produits)
  io_table = io_table %>% 
    select(regions,nom_pays,produits,DF_tot,production,production_2,production_3,impact_prod,impact_demande,GES_impact_S, GES_impact_M)#production_2,GES_impact_producteur,GES_impact_demande)
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  IO[sapply(IO, simplify = 'matrix', is.infinite)] <- 0
  IO[sapply(IO, simplify = 'matrix', is.nan)] <- 0
  assign(str_c("IO_",pays),IO)
  
  plot=IO %>% 
    group_by(produits) %>%
    mutate(agg.demande_impact=sum(GES_impact_M),
           agg.producteur_impact=sum(GES_impact_S),
           agg.production=sum(production_3),
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
          y = log10(impact),
          fill = indicator)) +
    geom_bar(stat='identity',position = "dodge") + 
    scale_x_discrete(breaks=IO$produits,
                     labels=IO$categorie.produit)+
    theme(axis.text.x = element_text(angle = 60, vjust = 0.5, hjust=1, size=4))
  
  assign(str_c("plot.produit_",pays),plot)
  
  rm(IO,IO_all,io_table,plot)
  #Aggréger les produits?
  #Créer un graphique
  
  
}


#SERVICES EXTRA-TERRITORIAUX toujours = 0 (?)

View(checklist_demande)

#Créer grand dataframe
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
sum(IO_all$production_3)==sum(X)
sum(IO_all$DF_tot)==sum(Y)

sum(IO_all$production)-sum(IO_all$production_2)
#les deux calculs production sont équivalents au niveau mondial (filtrer y ou filtrer L)

sum(IO_all$GES_impact_S)==sum(IO_all$GES_impact_M)
#Pas additivité car inputs pas pris en compte


sum(Fe_impacts.mat$`GHG emissions AR5 (GWP100) | GWP100 (IPCC, 2010)`)
sum(Fe_impacts.mat[,103])



##Aggréger tous secteurs par pays pour graphique
IO_all_agg.pays <- IO_all %>% select(-produits) %>%
  group_by(nom_pays) %>%
  summarise(agg.demande_impact=sum(GES_impact_M),
            agg.producteur_impact=sum(GES_impact_S),
            agg.production=sum(production_3),
            agg.demande_finale=sum(DF_tot))
View(IO_all_agg.pays)

##Aggréger par un pays secteur pour graphique
IO_agg.secteur = IO_France %>% 
  mutate(categorie.produit=substr(produits, 1,3),
         categorie.facet=substr(produits, 1,1)) %>%
  group_by(categorie.produit,categorie.facet) %>%
  summarise(agg.demande_impact=sum(GES_impact_M),
            agg.producteur_impact=sum(GES_impact_S),
            agg.production=sum(production_3),
            agg.demande_finale=sum(DF_tot))
View(IO_agg.secteur)

#Aggréger niveau mondial par produit pour graphique
#(aucun intérêt si ce n'est vérifier l'égalité, à faire par pays)
IO_agg.produits = IO_all %>% 
  group_by(produits) %>%
  mutate(agg.demande_impact=sum(GES_impact_M),
         agg.producteur_impact=sum(GES_impact_S),
         agg.production=sum(production_3),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5))
View(IO_agg.produits)

#
IO_France %>% 
  group_by(produits) %>%
  mutate(agg.demande_impact=sum(impact_demande),
         agg.producteur_impact=sum(GES_impact_prod),
         agg.production=sum(production_2),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5))
  