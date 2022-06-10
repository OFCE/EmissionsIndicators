### data loading

#A mettre dans header si besoin
#path_codedata <- str_c(path_user,".../src/")

A <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/A.rds"))
Y <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y.rds"))
Fe <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/F.rds"))

Y_d <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y_types_DF.rds"))
A_cd <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/A_pays_secteurs.rds"))
Y_cd <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y_pays_types_DF.rds"))
Y_c <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y_pays.rds"))
F_cd <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/F_pays_secteurs.rds"))
F_noms_extensions <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/F_noms_extensions.rds"))

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
row.Y_dfff <- str_c(tY_dff$countries.out,"_",tY_dff$products.in)
tY_dfff <- tY_dff %>% select(-countries.out,- products.in) %>% 
  as.matrix(length(nrow(tY_dff)),length(ncol(tY_dff)),
            dimnames = list(row.Y_dff,col.Y_dfff)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(row.Y_dfff)
View(tY_dfff)
(sum(tY_dfff[,-1:-2]) - sum(Y)) /sum(Y) *100 #Erreur: 0.027%

####Mêmes étapes pour A
A_df <- A %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))

## Merge and aggregation
A_dfff <- merge(A_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(A_cd), ~ sum(. * weight)))  %>% ungroup()

#A_dff <- merge(A_df, br_lg, by = "countries.in" , all = T) %>%
#  group_by(countries.out) %>%
#  summarise_at(A_cd,~sum(.x)) %>% arrange(countries.out) %>% ungroup()

#A_dff_c <-as.vector(A_dff$countries.out)

col.A_dfff <- str_c(A_dfff$countries.out,"_",A_dfff$products.out)

# Step 2 avec la transpose
tA_dff <- A_dfff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(col.A_dfff),length(A_cd),
            dimnames = list(col.A_dfff,A_cd)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.A_dfff) %>%
  #transposée, maintenant bridge dans l'autre sens
  mutate(countries.in = str_sub(rownames(.),1,2),
         products.in = str_sub(rownames(.),4)) %>%
  merge(br_lg, by = "countries.in" ,all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in",all.x = TRUE) %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(col.A_dfff), ~ sum(.)))  %>% ungroup()

# Check for NA
#A_dff[is.na(A_dff)]
A_dfff[is.na(A_dfff)]
tA_dff[is.na(tA_dff)]

#difference in value from the original database Y and the transformed Y_dff. Should be equal to 0 
sum(A_dff[,-1]) - sum(A)
(sum(tA_dff[,-1:-2]) - sum(A)) /sum(A) *100

#Cleaning (noms des lignes et colonnes,...)
tA_dfff <- tA_dff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(nrow(tA_dff)),length(ncol(tA_dff)),
            dimnames = list(col.A_dfff,col.A_dfff)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.A_dfff)
View(tA_dfff)

(sum(tA_dfff[,-1:-2]) - sum(A)) /sum(A) *100 #Erreur trop importante: 8,67%

####Mêmes étapes pour Fe (plus court)

F_df <- Fe %>% as.data.frame() %>% t() %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))

## Merge and aggregation
F_dfff <- merge(F_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(F_noms_extensions), ~ sum(. * weight)))  %>% ungroup()

# Check for NA
F_dfff[is.na(F_dfff)]
tF_dfff[is.na(tF_dfff)]

#Cleaning (noms des lignes et colonnes,...)
row.F_dfff <- str_c(F_dfff$countries.out,"_",F_dfff$products.out)
tF_dfff <- F_dfff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(row.F_dfff),length(F_noms_extensions),
            dimnames = list(row.F_dfff,F_noms_extensions)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(row.F_dfff)
View(tF_dfff)

#difference in value from the original database Y and the transformed Y_dff. Should be equal to 0 
(sum(tF_dfff[,-1:-2]) - sum(Fe))  /sum(Fe) *100 #Erreur de 0.95%

### Save and export
saveRDS(tA_dfff, str_c(path_out, "A_ThreeMe.rds"))

saveRDS(tY_dfff, str_c(path_out, "Y_ThreeMe.rds"))

saveRDS(tF_dfff, str_c(path_out, "Fe_ThreeMe.rds"))
