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

# Extraction des indicateurs GES depuis les matrices environementales (S & M )
GHG.extraction <- function(data, 
                           GES,
                           unit.co2e = NULL)
{
  
  if (is.null(unit.co2e)){unit.co2e = TRUE}
  
  GES_list <- list()
  
  
  if (GES == "GES"){
    
    GES_list[["GES.raw"]] <- data %>% 
      as.data.frame %>% 
      filter(str_detect(row.names(.), "CO2") | 
               str_detect(row.names(.), "CH4") | 
               str_detect(row.names(.), "N2O") | 
               str_detect(row.names(.), "SF6") | 
               str_detect(row.names(.), "PFC") | 
               str_detect(row.names(.), "HFC") )
    
    
    GES_list[["GES.raw"]] 
    
    for (g in glist){
      
      #Row number for each GES in the S matrix
      id_row <- str_which(row.names(GES_list[["GES.raw"]]),str_c(g))
      if  (ncol(GES_list[["GES.raw"]]) == 1 ){
        
        GES_list[[str_c(g)]] <- GES_list[["GES.raw"]][id_row,] %>%  sum() %>% as.data.frame()
      } else 
      {
        GES_list[[str_c(g)]] <- GES_list[["GES.raw"]][id_row,] %>%  colSums() %>% as.data.frame()
      }
      # Conversion to equivalent CO2 unit
      GES_list[[g]] <- GHGToCO2eq(GES_list[[g]])
    }
    
    GES_list[["GES"]] <- GES_list[["CO2"]] +
      GES_list[["CH4"]] +
      GES_list[["N2O"]] +
      GES_list[["SF6"]] +
      GES_list[["HFC"]] +
      GES_list[["PFC"]]
    
    
    #assign(str_c("GES_impact_",names(listdf)[index]), GES_list[["GES"]])
    #index=index+1
  } else 
  {
    
    GES_list[["GES.raw"]] <- data %>% 
      as.data.frame %>% 
      filter(str_detect(row.names(.), GES))
    
    
    #Row number for each GES in the S matrix
    id_row <- str_which(row.names(GES_list[["GES.raw"]]),str_c(GES))
    
    if  (ncol(GES_list[["GES.raw"]]) == 1 ){
      GES_list[[str_c(GES)]] <- GES_list[["GES.raw"]][id_row,] %>%  sum() %>% as.data.frame()
    } else 
    {
      GES_list[[str_c(GES)]] <- GES_list[["GES.raw"]][id_row,] %>%  colSums() %>% as.data.frame()
    } 
    
    
    
    if (unit.co2e == TRUE){
      GES_list[[GES]] <- GHGToCO2eq(GES_list[[GES]])
    }
  }
  
  colnames(GES_list[[GES]]) <-  "value"
  
 
  return( GES_list[[GES]] )
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