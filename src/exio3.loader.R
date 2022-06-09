### data loading

A <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/A.rds"))
Y <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y.rds"))
#Fe <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Fe.rds"))

Y_d <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y_types_DF.rds"))
A_cd <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/A_pays_secteurs.rds"))
Y_cd <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y_pays_types_DF.rds"))
Y_c <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y_pays.rds"))

Y_df <- Y %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))

### Bridge in long format for countries
br_lg <-  loadBridge("Countries_1", "Countries_2.WD1", "Countries") %>%
  data.frame() %>% mutate(countries.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$countries, names_to = "countries.in", values_to = "value") %>%
  filter(value == 1) %>% select(-value)
# 
# br_lg %>% arrange(match(countries.in, countries_data)) %>% select(countries.in) %>% as.vector()
# countries_data

### Bridge in long format for products
br.2_lg <-  loadBridge("exio3", "threeMeEurostat", "Products", transverse = T) %>%
  as.data.frame() %>% mutate(products.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$products, names_to = "products.in", values_to = "value") %>%
  filter(value == 1) %>% select(-value)

### Merge and summarize of rows

Y_dff <- merge(Y_df, br_lg, by = "countries.in" , all = T) %>%
  group_by(countries.out) %>%
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out) %>% ungroup()

Y_dff_c <-as.vector(Y_dff$countries.out)

Y_dfff <- merge(Y_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out, products.out) %>% ungroup()


tY_dff <- as.matrix(Y_dff[-1], length(Y_dff_c),length(Y_cd),
                    dimnames = list(Y_dff_c,Y_cd)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(Y_dff_c) %>%
  mutate(countries.in = str_sub(rownames(.),1,2),
         products.in = str_sub(rownames(.),4)) %>%
  merge(br_lg, by = "countries.in" , all = T) %>%
  group_by(countries.out) %>%
  summarise_at(Y_dff_c,~sum(.x)) %>%
  arrange(countries.out) %>% ungroup()


group_by(countries.out, products.out) %>% 
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out, products.out) %>% ungroup()
pivot_longer(cols = Y_dff_c, names_to = "products.in", values_to = "value") %>%
  filter(value == 1) %>% select(-value)

# Check for NA
Y_dff[is.na(Y_dff)]
Y_dfff[is.na(Y_dfff)]


#difference in value from the original database Y and the transformed Y_dff. Should be equal to 0 
sum(Y_dff[,-1]) - sum(Y)
sum(tY_dff[,-1]) - sum(Y)
sum(Y_dfff[,-1:-2]) - sum(Y)

Y[,1] %>% sum()
# 9002674 tot sum
Y_dff %>% select(-countries.out, -products.out) %>% sum()
Y_dff %>% select(-countries.out) %>% sum()

#67492247 tot sum
Y %>% sum()

##Columns 
### Merge and summarize of rows
#En cours (pas du tout le résultat attendu pour l'instant)
Y_dffinal <- Y_dff %>% pivot_longer(cols = starts_with(exio3.desc$countries), names_to = "countries.in", values_to = "value")
Y_dffinal <- merge(Y_dff, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out, products.out) %>% ungroup()

>>>>>>> f28f67748d00173adc32cae5b155a174d779d774
