


#Colonne nom pays (pas nécessaire si pas rbind par la suite)
shock.demand <- function(data,
                         iso = NULL, 
                         aggregate = NULL){
  
  if(is.null(aggregate)){aggregate = FALSE}
  
  
  
  name.iso <- c(rep(iso,ncol(Z))) #length()=204
  
  #Colonne demande finale
  
  #mettre à 0 les entrées des autres pays (demande finale du pays en question adressée aux autres pays)
  if(!is.null(iso)){
    
    data[-str_which(rownames(data),as.character(iso)),] <- 0
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

#Une version plus générique de la fonction
divide <- function(Vect,Fe.mat,Y.mat,L.mat,demand=FALSE,volume=FALSE){
  if(class(Vect)!="numeric"){
    print("Convert to numeric")
    Vect=as.numeric(unlist(Vect))
  }
  vect_1=1/Vect
  vect_1[is.infinite(vect_1)] <- 0 
  vect_1d <- as.numeric(vect_1) %>% diag
  Mat.imp <- (as.matrix(Fe.mat) %*% vect_1d) %>% #S
    `colnames<-`(rownames(Vect))
  
  if(demand==FALSE & volume==TRUE){
    Mat.imp[is.nan(Mat.imp)]
    Mat.imp <- Mat.imp %*% diag(unlist(Vect)) #S_volume
    colnames(Mat.imp)<-colnames(Fe.mat)
  }
  
  if(demand==TRUE){
    Mat.imp <- Mat.imp %*% L.mat #M
    
    if(volume==TRUE){
      Y_comp.tot<-colSums(Y.mat)
      y_1 <- 1/Y_comp.tot
      y_1[is.infinite(y_1)] <- 0 
      y_1d <- as.numeric(y_1) %>% diag
      Y.share <- as.matrix(Y.mat) %*% y_1d
      demand=Y_comp.tot%*%t(Y.share)
      Mat.imp <- Mat.imp %*% diag(as.data.frame(demand)) #M_volume
      colnames(Mat.imp)<-colnames(demand)
    }
  }
  return(Mat.imp)
}
