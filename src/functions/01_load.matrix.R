
load.matrix <- function(mat,
                        year, 
                        nom = NULL,
                        path = path_data.source,
                        satellite = NULL,
                        col_index = NULL, 
                        row_index = NULL, 
                        export = NULL, 
                        path_out = path_out)
{
  
  if (is.null(export)){ export = FALSE} 
  
  if (is.null(nom)){ nom = "pxp"} 
  
  if (is.null(col_index)){ col_index = ""} 
  
  if (is.null(row_index)){ row_index = ""} 
  
  if (is.null(satellite)){ satellite = FALSE} 
  
  # Col names 
  if (satellite == TRUE){
    
    df_mat <- fread(file = str_c(path,"IOT_",year,"_",nom,"/satellite/",mat,".txt"),
                    sep = "\t",
                    header = FALSE) %>%
      as.data.frame()
    
    # Col names
    df_mat_col.iso <- df_mat[1,][-1] %>% as.character()#extraction du nom des pays de A
    df_mat_col.sec <- df_mat[2,][-1] %>% as.character()#extraction des noms de secteur de A
    df_mat_col.iso_sec <- str_c(col_index,df_mat_col.iso,"_",df_mat_col.sec)
    
    
    # Row names
    df_mat_row.iso <- df_mat[,1][-1:-2] %>% as.character()#extraction du nom des pays de A
    df_mat_row.sec <- df_mat[,2][-1:-2]  %>% as.character()#extraction des noms de secteur de A
    df_mat_row.iso_sec <- str_c(row_index,df_mat_row.iso,"_",df_mat_row.sec)
    
    
    df_mat <- as.matrix(df_mat)[-1:-2,-1] 
    
    
  } else {
    df_mat <- fread(file = str_c(path,"IOT_",year,"_",nom,"/",mat,".txt"),
                    sep = "\t",
                    header = FALSE) %>%
      as.data.frame()
    
  
  # Col names
  df_mat_col.iso <- df_mat[1,][-1:-2] %>% as.character()#extraction du nom des pays de A
  df_mat_col.sec <- df_mat[2,][-1:-2] %>% as.character()#extraction des noms de secteur de A
  df_mat_col.iso_sec <- str_c(col_index,df_mat_col.iso,"_",df_mat_col.sec)
  
  
  # Row names
  df_mat_row.iso <- df_mat[,1][-1:-3] %>% as.character()#extraction du nom des pays de A
  df_mat_row.sec <- df_mat[,2][-1:-3]  %>% as.character()#extraction des noms de secteur de A
  df_mat_row.iso_sec <- str_c(row_index,df_mat_row.iso,"_",df_mat_row.sec)
  
 
  
  # Extraction des valeurs du df et mise sous forme d'une matrice de type "numeric"
  df_mat <- as.matrix(df_mat)[-1:-3,-1:-2] # on ne garde que les valeurs numeriques de A et plus les noms de ligne et colonnes
  
  }
  
  df_mat <- as.numeric(unlist(df_mat))# on transforme les valeur du df en valeurs numeriques et non plus en chaines de caracteres
  df_mat <- matrix(df_mat,length(df_mat_row.iso_sec),length(df_mat_col.iso_sec),
                   dimnames=list(df_mat_row.iso_sec,df_mat_col.iso_sec)) 
  
  
  if (export == TRUE){
    saveRDS(df_mat, str_c(path_data.source,"IOT_",year,"_",nom,"/",mat,".rds"))
  }
  
  df_mat
}
