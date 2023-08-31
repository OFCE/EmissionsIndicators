
table.import <- function(data,
                         iso = NULL, 
                         transpose = NULL){
  
  if(is.null(iso)){iso == iso}
  if(is.null(transpose)){transpose = FALSE}
  
  if(transpose == TRUE){
    
    products_labels <- data[1:2]
    
    data <- t(data[-1:-4]) %>% as.data.frame() %>%
      `colnames<-`(colnames(data[-1:-4])) %>%
      mutate(countries = str_sub(rownames(data),1,2),
             products = str_sub(rownames(data),4)) %>%
      left_join(products_labels, by = "products") %>% select(colnames(EMS.M))
    
  }
  
  df <- data  %>% group_by("countries", "products", "products.fr", "codes")  %>%
    summarise(across(colnames(data[,-1:-4]), ~ sum(.)), .groups = "drop_last")  %>% ungroup() %>%
    .[,-1:-4] %>% 
    t %>% as.data.frame() %>% 
    mutate(countries = str_sub(rownames(.),1,2),
           products = str_sub(rownames(.),4)) %>%
    pivot_wider(names_from = countries,
                values_from = V1)  %>% select(-all_of(iso))
  return(df)
}
# 
# (data[,-1:-3]/10^9) %>% sum()
# (df[-1]/10^9) %>% sum()

