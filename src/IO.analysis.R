#Rappel du calcul IO 
#Égalité comptable de base 
#X = CI + Y 
#X - CI = Y
#(1-a)X = Y avec (1-a)^1 = L
#X = L.Y 

br <- "ThreeMe"

# Chargement des données I-O sauvegardées par le script exio3.loader.R
A <-readRDS(str_c(path_out,"/A_",br,".rds"))
Y <-readRDS(str_c(path_out,"/Y_",br,".rds"))
Fe <-readRDS(str_c(path_out,"/Fe_",br,".rds"))

Z <-readRDS(str_c(path_out,"/Z_",br,".rds"))
X <-readRDS(str_c(path_out,"/X_",br,".rds"))

#Vérification de l'équation comptable:
(sum(Y)+sum(Z)-sum(X))/sum(X)*100 #une petite erreur (0.22%)

#Calcul de A (marche aussi avec d'autres fonctions, résultats identiques)
X_vect=X$production
A.alternative <- sweep( Z , 
                        MARGIN = 2 , 
                        STATS=X_vect , 
                        FUN='/' ,
                        check.margin = TRUE)
A.alternative[is.na(as.data.frame(A.alternative))] <- 0 
sum(A.alternative)

## Calcul de L: matrice de Leontief
L <- LeontiefInverse(A)
L.alternative <- LeontiefInverse(A.alternative)
#

print("Computation of the Leontief matrix (L) : done")


Y_pays_types_DF<-colnames(Y)
y_tot<- as.matrix(Y) %*% as.matrix(rep(1,length(Y_pays_types_DF)))
rm(Y_pays_types_DF)
rm(Y)
#y_tot : demande totale par région et secteur


## calcul de S
#x (production totale (pour CI + pour DF))
x <- (L.alternative %*% y_tot) %>% as.numeric() #%>% as.matrix() %>% t()
#X <- (L %*% as.matrix(Y)) revient au même

#####test pour la matrice X
(sum(X)-sum(x))/sum(X) * 100 #petite erreur (-0.48%)
x_abs <- x %>% as.data.frame() %>% summarise(abs(.))
(sum(x_abs)-sum(X_df$production))/sum(x_abs) * 100 #erreur moins grosse mais quand même trop grande (devrait provenir de A ou de L ?)
#####

#Checks sur les valeurs
#########
I <- diag(rep(1, dim(A)[1]))
invL=(I-A)
checkY=as.matrix((I-A.alternative))%*%as.matrix(X)
sum(checkY)==sum(Y)
sum(checkY)-sum(Y)

invL_test=(I-test)
checkY=as.matrix((I-test))%*%as.matrix(X)
sum(checkY)==sum(Y)
sum(checkY)-sum(Y)

checkX= as.matrix(L) %*% (checkY)
sum(x)==sum(checkX)
sum(x)-sum(checkX)
checkY=as.matrix((I-A))%*%as.matrix(X)

checkX= as.matrix(L) %*% (y_tot)
sum(x)==sum(checkX)
sum(x)-sum(checkX)
#########

#valeurs négatives?
mean(apply(A,2,mean))

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
  GES_list[["HFC"]] + #déjà convertis
  GES_list[["PFC"]]
#Crée un vecteur (1065x1) (de quantité de GES émise par chaque secteur ?)

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
print(str_c("Les matrices S, L et M ainsi que le vecteur x ont ete calculees et enregistrees a l'adresse suivante~:EXIOBASE/data/IOT_",year,"_",nom,"/"))


rm(list=c("L","S","x"))
rm(GES_list)




#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays

#Chemin pour exporter
#dir.create(str_c(path_out, "/IO_pays"), recursive = TRUE)
#path_IOpays_tables <- str_c(path_out, "/IO_pays")

checklist_demande <- list()
checklist_production <- list()
#Attention, il faut mettre "Europe" et non "Europe (autres)", sinon la sélection ne marche pas

#Boucle qui crée un tableau avec les indicateurs pour chaque pays
#(il faut avoir Y, Fe et L au préalable)
for (pays in c("France","EU","US","Chine","Amerique du N.","Amerique du S.","Afrique","Russie","Europe","Asie","Moyen-Orient","Oceanie")) {
  
  #Colonne nom pays (pas nécessaire si pas rbind par la suite)
  nom_pays <- c(rep(pays,1056))
  
  #Colonne demande finale
  DF=Y
  #mettre à 0 les entrées des autres pays (demande finale adressée au pays en question)
  DF[,-str_which(colnames(DF),as.character(pays))]<-0
  DF_tot <- as.matrix(DF) %*% Id(DF) #somme de toutes les composantes
  #interprétation : quantité consommée par ce pays et produite dans le monde
  #check: sum(Y[,str_which(colnames(Y),as.character(pays))])==sum(DF_tot)
  #...TRUE
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
  
  #check pour ce calcul
  checklist_production[pays] <- sum(production)==sum(production_2)
  
  #Matrice S ("impact producteur") : impact environnemental (uniquement demande pays)
  x_1_select <- 1/production_2
  x_1_select[is.infinite(x_1_select)] <- 0 
  x_1_select <- as.numeric(x_1_select)
  x_1d_select <- diag(x_1_select)
  S_select <- as.matrix(Fe) %*% x_1d_select
  #interprétation : impact de la production de ce pays (par input)
  
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
  
  #Vecteur équivalent à M ("impact demande finale" adressée au pays)
  ##(donc déjà en CO2eq)
  GES_impact_DF <- as.matrix(GES_impact_prod %>% t()) %*% L %>% t()
  #interprétation : impact de la demande de ces produits
  
  #Créer le tableau en assemblant les colonnes
  GES_impact_producteur=as.numeric(unlist(GES_impact_prod))
  GES_impact_demande=as.numeric(unlist(GES_impact_DF))
  assign("io_table",
         data.frame(nom_pays,
                    DF_tot,production,production_2,GES_impact_producteur,GES_impact_demande
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
    select(regions,nom_pays,produits,DF_tot,production,production_2,GES_impact_producteur,GES_impact_demande)
  
  #Exporter le tableau
  saveRDS(io_table, str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  
  #Charger le tableau dans l'environnement
  IO <- readRDS(str_c(path_IOpays_tables, "/IO_", pays, ".rds"))
  assign(str_c("IO_",pays),IO)
  
  rm(IO,IO_all,io_table)
  #Aggréger les produits?
  #Créer un graphique
  
}

View(checklist_demande)
View(checklist_production)

#Créer grand dataframe
IO_all <- do.call("rbind",mget(ls(pattern = "^IO_*")))
sum(IO_all$production)-sum(IO_all$DF_tot) #! 
#problème ici car production n'est pas égale à demande
sum(IO_all$production_2)-sum(IO_all$DF_tot)
#pareil avec autre calcul production

sum(IO_all$production)-sum(IO_all$production_2)
#les deux calculs production sont équivalents au niveau mondial (filtrer y ou filtrer L)

#c'est normal que production et production_2 s'équilibrent, 
#mais production et DF_tot devraient être équivalents (?). Donc problème




##Aggréger tous secteurs par pays pour graphique
IO_all_agg.pays <- IO_all %>% select(-produits) %>%
  group_by(nom_pays) %>%
  summarise(agg.demande_impact=sum(GES_impact_demande),
            agg.producteur_impact=sum(GES_impact_producteur),
            agg.production=sum(production),
            agg.demande_finale=sum(DF_tot),
            agg.production_2=sum(production_2))
View(IO_all_agg.pays)

##Aggréger par un pays secteur pour graphique
IO_agg.secteur = IO_France %>% 
  mutate(categorie.produit=substr(produits, 1,3),
         categorie.facet=substr(produits, 1,1)) %>%
  group_by(categorie.produit,categorie.facet) %>%
  summarise(agg.demande_impact=sum(GES_impact_demande),
            agg.producteur_impact=sum(GES_impact_producteur),
            agg.production=sum(production),
            agg.demande_finale=sum(DF_tot))
View(IO_agg.secteur)

#Aggréger niveau mondial par produit pour graphique
#(aucun intérêt si ce n'est vérifier l'égalité, à faire par pays)
IO_agg.produits = IO_all %>% 
  group_by(produits) %>%
  mutate(agg.demande_impact=sum(GES_impact_demande),
         agg.producteur_impact=sum(GES_impact_producteur),
         agg.production=sum(production_2),
         agg.demande_finale=sum(production)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5))
View(IO_agg.produits)

IO_France %>% 
  group_by(produits) %>%
  mutate(agg.demande_impact=sum(GES_impact_demande),
         agg.producteur_impact=sum(GES_impact_producteur),
         agg.production=sum(production_2),
         agg.demande_finale=sum(production)) %>%
  ungroup() %>%
  mutate(categorie.produit=substr(produits, 1,5))
  