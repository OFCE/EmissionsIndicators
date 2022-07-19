


#Colonne nom pays (pas nécessaire si pas rbind par la suite)
shock.demand <- function(data,
                         iso = NULL, 
                         aggregate = NULL){
  
  if(is.null(aggregate)){aggregate = FALSE}
  
  
  
  name.iso <- c(rep(iso,ncol(Z))) #length()=204
  
  #Colonne demande finale
  
  #mettre à 0 les entrées des autres pays (demande finale du pays en question adressée aux autres pays)
  if(!is.null(iso)){
    
    data[,-str_which(colnames(data),as.character(iso))] <- 0
  }
  
  if (aggregate == TRUE){
    
    data <- rowSums(data) %>% as.matrix()
  }
  
  return(data)
}


#Matrice S (impact producteur)
Env.multiplier <- function(Y.mat, S.mat, L.mat,volume = NULL){
  
  if (is.null(volume)){volume = FALSE}
  
  
  x <- L.mat %*% Y.mat
  
  x_1 <- 1/x        
  x_1[is.infinite(x_1)] <- 0 
  
  x_1d <- as.numeric(x_1) %>% diag()
  
  S <- (t(S.mat)%*% x_1d) %>% `colnames<-`(rownames(X))
  S[is.nan(S)]
  
  ## 
  if (volume == TRUE){
    
    S <- S %*% diag(as.numeric(x)) %>% `colnames<-`(rownames(x))
  }
  
  return(S)
}