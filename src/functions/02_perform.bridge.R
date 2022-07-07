perform.bridge <- function(data, 
                           country_in, country_out, country_sht, 
                           sec_in, sec_out, sec_sht, 
                           sq_mat = NULL, 
                           format_data = NULL, 
                           transpose = NULL, 
                           satellite = NULL,
                           vector = NULL)
{
  
  
  if(is.null(sq_mat)){sq_mat = FALSE}
  
  if(is.null(format_data)){format_data = "data.frame"}
  
  if(is.null(transpose)){transpose = FALSE}
  
  if(is.null(satellite)){satellite = FALSE}
  
  if(is.null(vector)){vector = FALSE}
  
  
  df <- data %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                            products.in = str_sub(rownames(.),4))

  if (transpose == TRUE){
 
  df_0 <-   t(data)
     df <- df_0%>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                            products.in = str_sub(rownames(.),4))
  
  } else { 
    df_0 <- data
    df <- df_0 %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                              products.in = str_sub(rownames(.),4))
  }
  
  
  br_lg <-  loadBridge(country_in, country_out, country_sht) %>%
    data.frame() %>% mutate(countries.out = rownames(.)) %>%
    pivot_longer(cols = colnames(.)[-ncol(.)], names_to = "countries.in", values_to = "value") %>%
    filter(value ==1) %>% select(-value)
  
  
  ### Bridge in long format for products
  br.2_lg <-  loadBridge(sec_in, sec_out, sec_sht) %>% 
    as.data.frame() %>% mutate(products.out = rownames(.)) %>%
    pivot_longer(cols = colnames(.)[-ncol(.)], names_to = "products.in", values_to = "weight") %>% 
    filter(weight > 0)
  

  ## Merge and aggregation
  if (satellite == TRUE){
  df.1 <- merge(df, br_lg, by = "countries.in" , all.x = TRUE) %>%
    merge(., br.2_lg, by = "products.in") %>%
    group_by(countries.out, products.out) %>%
    # Somme pondérée par weight pour les produits (0>p>1)
    summarise(across(all_of(colnames(df_0)), ~ sum(. * weight)))  %>% ungroup() %>%
    rename_with(~ sub("^countries.*", "countries", .x), starts_with("countries")) %>%
    rename_with(~ sub("^products.*", "products", .x), starts_with("products"))
  
  id_out <-  str_c(df.1$countries,"_",df.1$products)
  
  } else {

    df.1 <- merge(df, br_lg, by = "countries.in" , all.x = TRUE) %>%
      merge(., br.2_lg, by = "products.in") %>%
      group_by(countries.out, products.out) %>%
      # Somme pondérée par weight pour les produits (0>p>1)
      summarise(across(all_of(colnames(df_0)), ~ sum(. * weight)))  %>% ungroup() 
    
    id_out = str_c(df.1$countries.out,"_",df.1$products.out)
    
  # Step 2 avec la transpose (que pour countries, car produits ici composante demande)
  if (vector == FALSE){
    df.1 <- df.1 %>% select(-countries.out,- products.out) %>%
      as.matrix(nrow(df.1),ncol(data),
                dimnames = list(id_out,data)) %>%
      t() %>% as.data.frame() %>% `colnames<-`(id_out) %>%
      mutate(countries.in = str_sub(rownames(.),1,2),
             products.in = str_sub(rownames(.),4)) %>%
      merge(br_lg, by = "countries.in" , all = T) %>%
      group_by(countries.out, products.in) %>%
      # Somme pondérée par weight pour les produits (0>p>1)
      summarise(across(all_of(id_out), ~ sum(.)))  %>% ungroup()
  } 
  
 
  
  if (sq_mat == TRUE){
    
    df.1 <- df.1 %>% 
      merge(br.2_lg, by = "products.in", all = T) %>%
      group_by(countries.out, products.out) %>%
      # Somme pondérée par weight pour les produits (0>p>1)
      summarise(across(all_of(id_out), ~ sum(.)))  %>% ungroup() %>%
      rename_with(~ sub("^countries.*", "countries", .x), starts_with("countries")) %>%
      rename_with(~ sub("^products.*", "products", .x), starts_with("products"))
      #rename(countries = "countries.out", 
      #       products = "products.out")
    
    
    id_out.col <- str_c(df.1$countries,"_",df.1$products)
    id_out.row <- colnames(select(df.1, - products, -countries))
    
  } 
  else { 
    df.1 <- df.1 %>% 
      rename_with(~ sub("^countries.*", "countries", .x), starts_with("countries")) %>%
      rename_with(~ sub("^products.*", "products", .x), starts_with("products"))
    
    

    }
  
  id_out.col <- str_c(df.1$countries,"_",df.1$products)
  id_out.row <- colnames(select(df.1, - products, -countries))
  }
  
  #Choice of the format of export of the datatable (either data.frame or matrix)
  if (format_data == "matrix"){

    
  df.1 <- df.1 %>% select(-countries, - products) %>% 
      as.matrix(nrow(.),ncol(.)) %>% `rownames<-`(id_out) 
  
  }
  
  return(df.1)
}