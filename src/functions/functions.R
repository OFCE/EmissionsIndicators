###### Fonctions pour l'analyse entrées sorties et pour le développement #######
#
#Ce fichier comprends toutes les fonctions peu complexes appelées depuis les différents scripts et grandes fonctions
#
#

#### Partie 1. Fonctions liées au calcul de l'empreinte (liées à des aspects méthodologiques) ####


#### LeontiefInverse


# Calcul de l'inverse de Leontief
LeontiefInverse <- function(A){
  
  I <- diag(rep(1, dim(A)[1]))
  L <- solve(I - A)
  
  return(L)
}


#### ExtractCountryDemand
# Extraction de la Demande Finale d'un pays (tous types de demande confondus). Indiquer type_demande=1 pour les ménages
ExtractCountryDemand<-function(Y,Y_pays_types_DF,pays="FR",type_demande = c(1:7)){
  
  y_demand<-Y[,str_which(Y_pays_types_DF,pays)]
  y_demand[,type_demande]
}


#### ExtractImpact
# Extraction des impacts environnementaux voulus depuis une matrice d'intensités d'émissions (F,S,M ou D(CBA)) via des chaînes de caractères
ExtractImpact<-function(Env,impacts,adresse_ecriture,verbose=T){
  F_noms_extensions<-readRDS(str_c(adresse_ecriture,"F_noms_extensions.rds"))
  
  indices_tot<-str_which(F_noms_extensions,impacts[1])
  
  for(s in impacts[-1]){
    
    indices<-str_which(F_noms_extensions,s)
    indices_tot<-c(indices_tot,indices)
  }
  if(verbose==T){#Si verbose ==TRUE alors on affiche la liste des impacts extraits
    print("ExtractImpact : j'extrais les impacts suivants : ")
    print(F_noms_extensions[indices_tot])
  }
  
  return(Env[indices_tot,])
}


#### GHGToCO2eq


# Calcul des émissions de CH4, SF6 et N2O en CO2eq pour une matrice (PFC et HFC sont déjà en CO2eq dans EXIOBASE)
GHGToCO2eq<-function(GES){
  CO2<-str_which(row.names(GES),"CO2")
  CH4<-str_which(row.names(GES),"CH4")
  N2O<-str_which(row.names(GES),"N2O")
  SF6<-str_which(row.names(GES),"SF6")
  
  
  GES[CH4,]<-28*GES[CH4,]
  GES[N2O,]<-265*GES[N2O,]
  GES[SF6,]<-23500*GES[SF6,]
  return(GES)
}


#### DecompCO2eqByGHG


# Calcul de la part de chaque GES dans les émissions totales de GES
DecompCO2eqByGHG<-function(GHG){
  
  tot<-sum(GHG)
  CO2<-str_which(row.names(GHG),"CO2")
  CH4<-str_which(row.names(GHG),"CH4")
  N2O<-str_which(row.names(GHG),"N2O")
  SF6<-str_which(row.names(GHG),"SF6")
  PFC<-str_which(row.names(GHG),"PFC")
  HFC<-str_which(row.names(GHG),"HFC")
  
  CO2_tot<-sum(GHG[CO2,])
  CH4_tot<-sum(GHG[CH4,])
  N2O_tot<-sum(GHG[N2O,])
  SF6_tot<-sum(GHG[SF6,])
  PFC_tot<-sum(GHG[PFC,])
  HFC_tot<-sum(GHG[HFC,])
  
  Decomp_by_GHG<-data.frame(CO2=CO2_tot, CH4=CH4_tot,N2O=N2O_tot,SF6=SF6_tot,PFC=PFC_tot,HFC=HFC_tot,Unit="Mt eqCO2")
  Decomp_by_GHG_percentage<-data.frame(CO2=CO2_tot/tot, CH4=CH4_tot/tot,N2O=N2O_tot/tot,SF6=SF6_tot/tot,PFC=PFC_tot/tot,HFC=HFC_tot/tot,Unit="%")
  result<-list(Mt_CO2eq=Decomp_by_GHG,Percentage=Decomp_by_GHG_percentage,GHG_Emissions_tot=tot)
  return(result)
  
}


#### DemandeIndustrieHorsPaysX


#Mettre à 0 une matrice de demande Y pour les industries du pays donné en argument
DemandeIndustrieHorsPaysX<-function(Y_Pays_DF,pays="FR"){
  Y_Pays_DF[str_which(A_pays_secteurs,"FR"),]<-0
  Y_Hors_Pays_X<-Y_Pays_DF
  return(Y_Hors_Pays_X)
}


####DemandeUniquementIndustriesPaysX


#Mettre à 0 une matrice de demande Y pour les industries autres que celles du pays donné en argument
DemandeUniquementIndustriesPaysX<-function(Y_Pays_DF,pays="FR"){
  Y_Pays_DF[-str_which(A_pays_secteurs,"FR"),]<-0
  Y_Uniquement_Pays_X<-Y_Pays_DF
  return(Y_Uniquement_Pays_X)
}

#### GHG_Footprint


#calcul l'empreinte carbone de la consommation en CO2eq pour les 6GES
GHG_Footprint<-function(Demande,M,pays="FR",adresse_ecriture){
  D<-M%*%Demande #Calcul d'IOA
  E<-ExtractImpact(D,c("CO2","CH4","N2O","SF6","HFC","PFC"),adresse_ecriture)%>%GHGToCO2eq()/1000000000#on convertit en CO2eq et en Mt
  return(DecompCO2eqByGHG(E))
}


#### LoadMatrixYLSM


## Fonctions de chargement de matrices pour l'analyse entrées-sorties
LoadMatrixYLSM<-function(adresse_ecriture){
  print("Je charge les matrices Y,M,L et S")
  Y<<-readRDS(str_c(adresse_ecriture,"/Y.rds"))
  Y_pays_types_DF<<-readRDS(str_c(adresse_ecriture,"/Y_pays_types_DF.rds"))
  M<<-readRDS(str_c(adresse_ecriture,"/M.rds"))
  L<<-readRDS(str_c(adresse_ecriture,"/L.rds"))
  S<<-readRDS(str_c(adresse_ecriture,"/S.rds"))
  A_pays_secteurs<<-readRDS(str_c(adresse_ecriture,"/A_pays_secteurs.rds"))
}
LoadMatrixAYF<-function(adresse_ecriture){
  print("Bien, je charge les matrices A,Y et F")
  A<<-readRDS(str_c(adresse_ecriture,"/A.rds"))
  Y<<-readRDS(str_c(adresse_ecriture,"/Y.rds"))
  FF<<-readRDS(str_c(adresse_ecriture,"/F.rds"))
}



#### ImpactParPays


###Fonction qui calcule l'impact par pays à partir d'une matrice E=diag(s)%*%L%*%diag(y_FR_DF)
ImpactParPays<-function(E_FR){
  ## Création un tableau qui fasse correspondance nom - codes iso3166 alpha 2 des pays
  pays<-read.table(str_c(donnees_cartes,"country_codes.csv"),sep=";", header=T)
  pays # le tableau qui contiendra les données de géolocalisation des émissions
  pays$region2=as.character(pays$region2)
  
  ## Il faut ajouter à ce data.frame une colonne GES
  pays$Impact<-0
  
  #Calcul si l'argument (E_FR) est une matrice
  if(class(E_FR)=="matrix"){ 
    for(p in pays$Code){
      
      i<-str_which(pays$Code,p)# on extrait le numéro de ligne du pays en cours dans le tableau "pays"
      
      i2<-str_which(rownames(E_FR),p)# on extrait les numéro de lignes des secteurs du pays en cours dans la matrice E_FR
      
      #Emissions de la DF française dans ce pays
      e<-E_FR[i2,] # Détail des émission de la france dans les différents secteurs du pays en cours
      e<-sum(e) # Somme de toutes les émissions de la France dans le pays en cours
      
      #On affecte la valeur des émissions de la France dans le pays en cours à la bonne ligne du tableau "pays"
      pays$Impact[i]<-e
      
    }
    # Annulation du calcul si l'argument est un vecteur
  }else if(class(E_FR)=="vector"){
    print("Il me faut une matrice en argument et non pas un vecteur (j'utilise les noms de lignes)")
    return(0)
    
  }else{  print("Erreur ! Il me faut une matrice en argument  (j'utilise les noms de lignes) qui soit extraite directement d'une matrice de type E_FR")}
  return(pays)
  
}# Fin de fonction ImpactParPays


from <- "exio3"
to <- "CPA2002_Niv4"
sht <- "Products"

#### Correspondence Matrix 

loadCorrespondenceMatrix <- function(from, to, sht) {
  path <- str_c("EXIOBASE/data/table.correspondences/", from, ".to.", to, ".xlsx")
  
  # Industry names
  row.names <- read_excel(path, sht, "A1:A500", T) %>% .[[1]]
  # Keep only non-NA cells
  row.count <- sum(!is.na(row.names))
  row.names <- row.names[2:(1 + row.count)]
  
  # Product names
  col.names <- read_excel(path, sht, "A1:HP1", T) %>% t 
  # Keep only non-NA cells
  col.count <- sum(!is.na(col.names))
  col.names <- col.names[2:(1 + col.count)]
  
  # Correspondence matrix
  mat <- read_excel(path, sht, anchored("B2", c(row.count, col.count)), F) %>% as.matrix
  rownames(mat) <- row.names
  colnames(mat) <- col.names
  mat[is.na(mat)] <- 0
  
  mat
  
}



## Fonction effectuant le calcul matriciel pour mettre les contenus carbone par produit issus d'EXIOBASE en COICOP Niv4
Exiobase3VersCOICOP4<-function(Matrice_Impacts_Totaux_Par_Produit_EXIOBASE,nProducts=200){
  
  
  
  
  
  ## 0. Chargement des tables de passage
  
  EXIOBASE3_Vers_CPA2002_Niv4<-readRDS(str_c(BDD,"EXIOBASE3_Vers_CPA2002_Niv4.rds"))# pour pxp
  CPA2002_Niv4_Vers_COICOP_Niv4<-readRDS(str_c(BDD,"CPA2002_Niv4_Vers_COICOP_Niv4.rds"))# pour pxp
  EXIOBASE3_Industries_Vers_CPA2002_Niv3<-readRDS(str_c(BDD,"EXIOBASE3_Industries_Vers_CPA2002_Niv3.rds"))# pour ixi
  CPA2002_Niv3_Vers_COICOP_Niv4<-readRDS(str_c(BDD,"CPA2002_Niv3_Vers_COICOP_Niv4.rds"))# pour ixi
  
  
  if(nProducts==200){### Si produit =200 (EXIOBASE pxp)  
    
    ## 1. 200 produits d'EXIOBASE --> CPA2002 Niveau 4 (223)
    Impacts_Totaux_Par_Produits_FR_CPA2002<-Matrice_Impacts_Totaux_Par_Produit_EXIOBASE%*%t(EXIOBASE3_Vers_CPA2002_Niv4)
    
    ## 2. CPA2002 Niv4 --> COICOP Niv4 
    Impacts_Totaux_Par_Produits_FR_COICOP_Niv4<-Impacts_Totaux_Par_Produits_FR_CPA2002%*%t(CPA2002_Niv4_Vers_COICOP_Niv4)
    dim(Impacts_Totaux_Par_Produits_FR_COICOP_Niv4)
    
  }else if(nProducts==163){### Si produit = 163
    
    ## 1. Exiobase indusries --> CPA2002 Niv 3
    Impacts_Totaux_Par_Produits_FR_CPA2002<-Matrice_Impacts_Totaux_Par_Produit_EXIOBASE%*%t(EXIOBASE3_Industries_Vers_CPA2002_Niv3)
    
    ## 2. CPA2002 Niv3--> COICOP Niv 4
    Impacts_Totaux_Par_Produits_FR_COICOP_Niv4<-Impacts_Totaux_Par_Produits_FR_CPA2002%*%t(CPA2002_Niv3_Vers_COICOP_Niv4)
  }else{
    print("Le nombre de produit entre n'est pas correct. ")
  }
  return(list(CPA2002=Impacts_Totaux_Par_Produits_FR_CPA2002,
              COICOP4=Impacts_Totaux_Par_Produits_FR_COICOP_Niv4))
}




#### COICOP4VersCOICOP123



### Fonction effectuant le calcul matriciel du passage COICOP4 --> COICOP 1, 2 ou 3
COICOP4VersCOICOP123<-function(Matrice_Impacts_Totaux_Par_Produit_COICOP4){
  
  ## 0. Chargement des tables de passage
  
  COICOP_Niv_4_Vers_COICOP_Niv_3<-readRDS(str_c(BDD,"COICOP_Niv_4_Vers_COICOP_Niv_3.rds"))
  COICOP_Niv_3_Vers_COICOP_Niv_2<-readRDS(str_c(BDD,"COICOP_Niv_3_Vers_COICOP_Niv_2.rds"))
  COICOP_Niv_2_Vers_COICOP_Niv_1<-readRDS(str_c(BDD,"COICOP_Niv_2_Vers_COICOP_Niv_1.rds"))
  ## 1. 161 produits COICOP4 --> 86 produits COICOP3
  
  Matrice_Impacts_Totaux_Par_Produit_COICOP3<-Matrice_Impacts_Totaux_Par_Produit_COICOP4%*%COICOP_Niv_4_Vers_COICOP_Niv_3
  
  ##2. 86 produits COICOP3 --> 37 produits COICOP2
  Matrice_Impacts_Totaux_Par_Produit_COICOP2<-Matrice_Impacts_Totaux_Par_Produit_COICOP3%*%COICOP_Niv_3_Vers_COICOP_Niv_2
  
  ##3. 37 produits COICOP2 --> 12 produits COICOP1
  Matrice_Impacts_Totaux_Par_Produit_COICOP1<-Matrice_Impacts_Totaux_Par_Produit_COICOP2%*%COICOP_Niv_2_Vers_COICOP_Niv_1
  
  
  return(list(COICOP3=Matrice_Impacts_Totaux_Par_Produit_COICOP3,
              COICOP2=Matrice_Impacts_Totaux_Par_Produit_COICOP2,
              COICOP1=Matrice_Impacts_Totaux_Par_Produit_COICOP1))
}


#### Partie 2: Fonctions de développement (purement technique et n'ayant pas d'intérêt méthodologique en soi)  #####



# Idem mais donne un aperçu ou l'on veut dans la matrice
res2<-function(M,nrow=c(1:4),ncol=c(1:4)){
  if(is.vector(M)){return(M[nrow])}
  else{return(M[nrow,ncol])}
}

#### Id


#Crée un vecteur identité de taille appropriée par rapport à une matrice donnée en argument
Id<-function(Matrice){
  Id<-rep(1,dim(Matrice)[2])
  return(Id)
}



#### DiagnostiqueMatrice 


#Une fonction pour tester une matrice avant ou après un calcul et anticiper / résoudre les bugs
DiagnostiqueMatrice<-function(Matrice){
  
  print(str_c("Nombre de lignes : ",dim(Matrice)[1]))
  print(str_c("Nombre de colonnes : ",dim(Matrice)[2]))
  print(str_c("Cet objet est de classe : ",class(Matrice)))
  print(str_c("Les donnees qu'il contient sont de type : ",typeof(Matrice)))
  print("Structure de l'objet (affiche NULL si pas de noms de colonnes et / ou lignes) :")
  print(str(Matrice))
  print(str_c("L'objet comprend ", sum(is.na(Matrice))," NA. "))
  if(sum(is.na(Matrice))!=0){
    print("Des NAs sont detectes dans l'objet, aucun calcul matriciel possible. Les coordonnees du / des NAs sont : ") 
    NAA<-which(is.na(Matrice),arr.ind = T)#Pour localiser les éléments = à NA
  }else{NAA<-"Fin de diagnostique"}
  return(NAA)
  
  
  #Exemple de diagnostic d'une matrice
  #mat<-matrix(c(1,"bleu",NA,2,4,5),2,3)
  #DiagnostiqueMatrice(mat)
  
} 



#### DiagnostiquePassage


#Fonction de diagnostique des tables de passage
DiagnostiquePassage<-function(Table_Passage){
  cols<-apply(Table_Passage,MARGIN=2,FUN="sum")
  print("Colonnes oubliées (somme = à 0) dans la table de passage :")
  print(which(cols==0))
  rows<-apply(Table_Passage,MARGIN=1,FUN="sum")
  print("Lignes oubliées (somme = à 0) dans les table de passage :")
  print(which(rows==0))
  print("**************************************************")
  
  print("Colonnes incompletes (la somme n'est pas égale à 1) : utile si la nomenclature de DEPART est en colonnes :")
  
  cols2<-which(cols!=1)
  print(cols[cols2])
  print("**************************************************")
  print("Lignes incompletes (la somme n'est pas égale à 1) : utile si la nomenclature de DEPART est en ligne")
  rows2<-which(rows!=1)
  print(rows[rows2])
  
}


#### SankeyTablePassage


#Fonction pour générer un diagramme de sankey à partir d'une table de passage
SankeyTablePassage<-function(Nom_Table,Transposee=T,width=2000, height=3000){
  ## Chargement d'une table de passage
  table<-readRDS(str_c(BDD,Nom_Table,".rds"))#lecture de la table de passage
  
  if(Transposee==T){#Argument à faire varier selon si la nomenclaure de départ est en ligne ou en colonne (ce qui implique le besoin de transposer la matrice pour avoir le sankey ds le bon sens)
    data <- t(table)
  }else{
    data <- table
  }
  
  
  ## Décomposition de la table (on produit un tableau à 3 cols : source, destination, et valeur)
  links <- data %>% 
    as.data.frame() %>% #On créé un df à partir de la matrice
    rownames_to_column(var="source") %>% 
    gather(key="target", value="value", -1) %>%
    filter(value != 0)
  
  
  # On créé ensuite un df comprenant la lite de tous les flux et de tous les noeuds impliqués ds ces flux
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Generation du sankey
  p <- sankeyNetwork(Links = links, Nodes = nodes,  # ?sankeyNetwork pour plus d'information sur les arguments de la fonction
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", nodePadding = TRUE,
                     sinksRight=TRUE,fontSize = 11, width = width, height = height )
  
  #Sauvegarde dans une page html directement à la racine du reprtoire PEB empreinte
  save_html(p, str_c("C:/OEB_Empreinte/",Nom_Table,".html"), background = "white", libdir = "repertoire sankeys")
  
  
  
}



#### SankeyDF



#Cette fonction crée un sankey à partir d'un dataframe comportant trois colonnes : source (origine du flux), arget (destination) et 
#value (la valeur du flux en question). cahque ligne doit correpondre à un flux.
SankeyDF<-function(DF, Unite="Indiquer ici l'unite",Nom_DF="Test Sankey DF",largeur=200,hauteur=300){
  
  
  links<-DF
  
  
  # On créé ensuite un df comprenant la lite de tous les flux et de tous les noeuds impliqués ds ces flux
  nodes <- data.frame(
    name=c(as.character(links$source), 
           as.character(links$target)) %>% unique()
  )
  
  # With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
  links$IDsource <- match(links$source, nodes$name)-1 
  links$IDtarget <- match(links$target, nodes$name)-1
  
  # Generation du sankey
  p <- sankeyNetwork(Links = links, Nodes = nodes,
                     Source = "IDsource", Target = "IDtarget",
                     Value = "value", NodeID = "name", nodePadding = FALSE,
                     sinksRight=TRUE,fontSize = 11, width = 200, height = 300 )
  
  print(p)
  
  #Sauvegarde dans une page html
  save_html(p, str_c("C:/OEB_Empreinte/",Nom_DF,".html"), background = "white", libdir = "lib2")
  
  return(p)
  
  #Exemple avec sankey DF
  #DF<-data.frame(source=c("FR","DE"),target=c("BZH","BZH"),value=c(0.2,0.8))
  #SankeyDF(DF)
  
}

