
# Calcul de l'inverse de Leontief
LeontiefInverse <- function(data,
                            coef = NULL)
  {
  
  # Conditions on technical coefficients
  if (is.null(coef)){coef = TRUE}
  
  if (coef == FALSE){
    data <- sweep(data, 
             MARGIN = 2,
             STATS=X[,"production"],
             FUN='/',
             check.margin = TRUE)
    
    # Change NA in 0 
    data[is.na(as.data.frame(data))] <- 0
  }
  
  I <- diag(rep(1, dim(data)[1]))
  L <- solve(I - data)
  
  return(L)
}

#Crée un vecteur identité de taille appropriée par rapport à une matrice donnée en argument
Id<-function(Matrice){
  Id<-rep(1,dim(Matrice)[2])
  return(Id)
}


