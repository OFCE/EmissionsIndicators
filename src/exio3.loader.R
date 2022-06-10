### data loading

A <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/A.rds"))
Y <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/Y.rds"))
#Fe <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Fe.rds"))

Y_d <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/Y_types_DF.rds"))
A_cd <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/A_pays_secteurs.rds"))
Y_cd <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/Y_pays_types_DF.rds"))
Y_c <- readRDS(str_c("data.out/IOT_",year,"_",nom,"/Y_pays.rds"))

Y_df <- Y %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))

### Bridge in long format for countries
br_lg <-  loadBridge("Countries_1", "Countries_2.WD1", "Countries") %>%
  data.frame() %>% mutate(countries.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$countries, names_to = "countries.in", values_to = "value") %>%
  filter(value ==1) %>% select(-value)


### Bridge in long format for products
br.2_lg <-  loadBridge("exio3", "threeMeEurostat", "Products", transverse = T) %>% 
  as.data.frame() %>% mutate(products.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$products, names_to = "products.in", values_to = "weight") %>% 
filter(weight > 0 )

## Merge and aggregation
Y_dfff <- merge(Y_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(Y_cd), ~ sum(. * weight)))  %>% ungroup() 


### Merge and summarize of rows
# TEST SUR AGGREGATION PAYS
 Y_dff <- merge(Y_df, br_lg, by = "countries.in" , all = T) %>%
   group_by(countries.out) %>%
   summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out) %>% ungroup()
 
 Y_dff_c <-as.vector(Y_dff$countries.out)

 
 # Taille finale de la matrice Y (hors colonnes indicateurs nom)
# Y 12 * 88 (1056) X  12 * 7 (84)
# A 12 * 88 (1056) X  12 * 88 (1056)
 
 
 # Nom nouvelles colonnes 
col.Y_dfff <- str_c(Y_dfff$countries.out,"_",Y_dfff$products.out)


# Step 2 avec la transpose (que pour countries, car produits ici composante demande)
tY_dff <- Y_dfff %>% select(-countries.out,- products.out) %>% as.matrix(length(col.Y_dfff),length(Y_cd),
                    dimnames = list(col.Y_dfff,Y_cd)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.Y_dfff) %>%
  mutate(countries.in = str_sub(rownames(.),1,2),
         products.in = str_sub(rownames(.),4)) %>%
  merge(br_lg, by = "countries.in" , all = T) %>%
  group_by(countries.out, products.in) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(col.Y_dfff), ~ sum(.)))  %>% ungroup() 


# Check for NA
Y_dff[is.na(Y_dff)]
Y_dfff[is.na(Y_dfff)]
tY_dff[is.na(tY_dff)]

#difference in value from the original database Y and the transformed Y_dff. Should be equal to 0 
sum(Y_dff[,-1]) - sum(Y)

# Encore une petite erreur mais qui est négligable (0.032%)
(sum(tY_dff[,-1:-2]) - sum(Y)) #/sum(Y) *100

#Changer noms des lignes
#rownames(tY_dff) <- str_c(tY_dff$countries.out,"_",tY_dff$products.in)

####A
A_df <- A %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))
A_dfff <- merge(A_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(A_cd), ~ sum(. * weight)))  %>% ungroup()

A_dff <- merge(A_df, br_lg, by = "countries.in" , all = T) %>%
  group_by(countries.out) %>%
  summarise_at(A_cd,~sum(.x)) %>% arrange(countries.out) %>% ungroup()

Y_dff_c <-as.vector(Y_dff$countries.out)
