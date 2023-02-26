n <- 0
products_labels <- EMS.M[1:2] |> distinct()

df <- t(EMS.M[-1:-3]) %>% as.data.frame() %>%
  `colnames<-`(colnames(EMS.M[-1:-3])) %>%
  mutate(countries = str_sub(rownames(.),1,2),
         products = str_sub(rownames(.),4)) %>%
  left_join(products_labels, by = "products") %>% select(colnames(EMS.M))

table.import <- function(data,
                         iso = NULL, 
                         transpose = NULL){
  
  if(is.null(iso)){iso == iso}
  if(is.null(transpose)){transpose = FALSE}
  
  if(transpose == TRUE){
    
    data <- t(data[-1:-3]) %>% as.data.frame() %>%
      `colnames<-`(colnames(data[-1:-3])) %>%
      mutate(countries = str_sub(rownames(data),1,2),
             products = str_sub(rownames(data),4)) %>%
      left_join(products_labels, by = "products") %>% select(colnames(EMS.M))
    
  }
  
  df <- (data)  %>% group_by("countries","code", "products")  %>%
    summarise(across(all_of(colnames(data[,-1:-3])), ~ sum(.)))  %>% ungroup() %>%
    .[,-1:-3] %>% 
    t %>% as.data.frame() %>% 
    mutate(countries = str_sub(rownames(.),1,2),
           products = str_sub(rownames(.),4)) %>%
    pivot_wider(names_from = countries,
                values_from = V1)  %>% select(-iso)
  return(df)
}