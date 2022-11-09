

table.import <- function(data){
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