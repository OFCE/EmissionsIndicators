#Fonction qui donne la VA à partir des matrices production (X) et consommations intermédiares (IO.Z)
ValeurAjoutee.calcul <- function(prod,IO.Z){
  CI= (t(IO.Z) %*% Id(IO.Z)) %>% as.data.frame()
  VA= prod-CI
  colnames(VA) <- "valeur_ajoutee"
  return(VA)
}

#Fonction qui récupère les composantes de la VA à partir de la matrice impacts Fe
Composantes.VA <- function(impacts){
  #Sélection des variables correspondant à la VA, et somme des composantes
  Fe_VA = t(impacts) %>% as.data.frame()
  #VA brutes
  Fe_VA$gross.VA <- apply(Fe_VA[,c(1:9)], 1, sum)
  #VA nette (sans consumption of fixed capital)
  Fe_VA$net.VA <- apply(Fe_VA[,c(1:5)], 1, sum)+apply(Fe_VA[,c(7:9)], 1, sum)
  Fe_VA$Etat <- apply(Fe_VA[,c(1:2)], 1, sum)
  Fe_VA$Travail <- apply(Fe_VA[,c(3:5)], 1, sum)
  Fe_VA$Capital <- apply(Fe_VA[,c(7:9)], 1, sum)
  #Toutes les composantes
  VA_decompo = Fe_VA[,c(1:9,1114:1118)] %>% as.data.frame()
  return(VA_decompo)
}
