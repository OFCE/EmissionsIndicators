
# Calcul de l'inverse de Leontief
LeontiefInverse <- function(data,
                            X = NULL,
                            coef = NULL,
                            direct = NULL)
{
  if (is.null(direct)){direct = FALSE}
  if (is.null(coef)){coef = TRUE}
  
  
  # Conditions on technical coefficients
  if (coef == FALSE){
    data <- coef_techniques(data,X)
    #data <- data %*% diag(1/X[,1])
    
    # Change NA in 0 
    data[is.na(as.data.frame(data))] <- 0
  }
  
  if (direct == FALSE){
    I <- diag(nrow = nrow(data))
    I_A <- I-data
    L  <- solve(I_A)
  } 
  if (direct == TRUE){
    
    I <- diag(nrow = nrow(data))
    I_A <- I-data
    L = I + data^2
  }
  return(L)
}

#Crée un vecteur identité de taille appropriée par rapport à une matrice donnée en argument
Id<-function(Matrice){
  Id<-rep(1,dim(Matrice)[2])
  return(Id)
}

coef_techniques <- function (Z,X)
{
  tei <- as.matrix(Z)
  prod <- as.matrix(X)
  ctec <- matrix(nrow = nrow(tei), ncol = ncol(tei))
  if (ncol(tei) == length(prod)) {
    for (i in 1:nrow(tei)) {
      for (j in 1:ncol(tei)) {
        if (prod[j]==0) {
          ctec[i,j] <- 0
        }else{
          ctec[i,j] <- tei[i,j]/prod[j]
        }
      }
    }
    return (ctec)
  }else {
    cat("Error : dimensions of Z and X are not the same")
  }
}
