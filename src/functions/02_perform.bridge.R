perform.bridge <- function(data, 
                           country_in, country_out, country_sht, 
                           sec_in, sec_out, sec_sht, 
                           sq_mat = NULL, 
                           format_data = NULL)
{
  
  
  if(is.null(sq_mat)){sq_mat = FALSE}
  
  if(is.null(format_data)){format_data = "data.frame"}
  
  
  df <- data %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                            products.in = str_sub(rownames(.),4))
  
  id.in = str_c(df$countries.in,"_",df$products.in)
  
  br_lg <-  loadBridge(country_in, country_out, country_sht) %>%
    data.frame() %>% mutate(countries.out = rownames(.)) %>%
    pivot_longer(cols = exio3.desc$countries, names_to = "countries.in", values_to = "value") %>%
    filter(value ==1) %>% select(-value)
  
  
  ### Bridge in long format for products
  br.2_lg <-  loadBridge(sec_in, sec_out, sec_sht) %>% 
    as.data.frame() %>% mutate(products.out = rownames(.)) %>%
    pivot_longer(cols = exio3.desc$products, names_to = "products.in", values_to = "weight") %>% 
    filter(weight > 0)
  
  
  ## Merge and aggregation
  df.1 <- merge(df, br_lg, by = "countries.in" , all.x = TRUE) %>%
    merge(., br.2_lg, by = "products.in") %>%
    group_by(countries.out, products.out) %>%
    # Somme pondérée par weight pour les produits (0>p>1)
    summarise(across(all_of(colnames(data)), ~ sum(. * weight)))  %>% ungroup() 
  
  
  id_out = str_c(df.1$countries.out,"_",df.1$products.out)
  
  
  
  # Step 2 avec la transpose (que pour countries, car produits ici composante demande)
  df.2 <- df.1 %>% select(-countries.out,- products.out) %>%
    as.matrix(nrow(df.1),ncol(data),
              dimnames = list(id_out,data)) %>%
    t() %>% as.data.frame() %>% `colnames<-`(id) %>%
    mutate(countries.in = str_sub(rownames(.),1,2),
           products.in = str_sub(rownames(.),4)) %>%
    merge(br_lg, by = "countries.in" , all = T) %>%
    group_by(countries.out, products.in) %>%
    # Somme pondérée par weight pour les produits (0>p>1)
    summarise(across(all_of(id.out), ~ sum(.)))  %>% ungroup() 
  
  
  
  
  
  if (sq_mat == TRUE){
    
    df.2 <- df.2 %>% 
      merge(br.2_lg, by = "products.in", all = T) %>%
      group_by(countries.out, products.out) %>%
      # Somme pondérée par weight pour les produits (0>p>1)
      summarise(across(all_of(id.out), ~ sum(.)))  %>% ungroup() %>%
      rename(countries = "countries.out", 
             products = "products.out")
    
    id_out.col <- str_c(df.2$countries,"_",df.2$products)
    id_out.row <- colnames(select(df.2, - products, -countries))
    
  } else { df.2 <- df.2 %>%
    rename(countries = "countries.out", 
           products = "products.in")}
  
  id_out.col <- str_c(df.2$countries,"_",df.2$products)
  id_out.row <- colnames(select(df.2, - products, -countries))
  
  
  #Choice of the format of export of the datatable (either data.frame or matrix)
  if (format_data == "matrix"){
    
    df.2 <- df.2 %>% select(-countries, - products) %>% 
      as.matrix(nrow(.),ncol(.),
                dimnames = list(id_out.row,id_out.col))
    
    
    
  } else{ 
    # Transpose and datawrangling
    df.2 <- df.2 %>% select(all_of(id_out)) %>% 
      t() %>% as.data.frame() %>% `colnames<-`(id_out.row) %>% 
      mutate(countries = str_extract(rownames(.),"^.+?(?=_)"),
             products = str_extract(rownames(.),"(?<=_)(.*)")) %>% 
      select(countries, products, all_of(id_out.col))
  }
  df.2
}