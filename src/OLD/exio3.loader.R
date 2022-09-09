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
br_pays <- "monde.12"
br_lg <-  loadBridge("Countries_1", "Countries_2.WD1", "Countries") %>%
  data.frame() %>% mutate(countries.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$countries, names_to = "countries.in", values_to = "value") %>%
  filter(value ==1) %>% select(-value)
regions<-unique(br_lg[,1])
saveRDS(regions, str_c(path_loader,br_pays, "_aggregate_regions.rds"))

### Bridge in long format for products
br <- "CPA2002_Niv1"
br.2_lg <-  loadBridge("exio3", br, "Products") %>% 
  as.data.frame() %>% mutate(products.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$products, names_to = "products.in", values_to = "weight") %>% 
filter(weight > 0)

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
  t() %>% as.data.frame() %>% `colnames<-`(col.Y_dfff) %>%
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

# Encore une petite erreur mais qui est négligable (0.032%) (0.074% avec les nouvelles données) (et 0% sans pondération)
(sum(tY_dff[,-1:-2]) - sum(Y)) /sum(Y) *100

#Changer noms des lignes
row.Y_dfff <- str_c(tY_dff$countries.out,"_",tY_dff$products.in)
tY_dfff <- tY_dff %>% select(-countries.out,- products.in) %>% 
  as.matrix(length(nrow(tY_dff)),length(ncol(tY_dff)),
            dimnames = list(row.Y_dff,col.Y_dfff)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(row.Y_dfff)
View(tY_dfff)
(sum(tY_dfff[,-1:-2]) - sum(Y)) /sum(Y) *100 #Erreur: 0.067%

#Y sans pondération
#en effet erreur plus petite (-0.0071141%)


####Mêmes étapes pour A (mais inutile car on ne peut pas aggréger A, pas de sens)
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
  summarise(across(all_of(col.A_dfff), ~ sum(. * weight)))  %>% ungroup()
#Ajout weight ici


# Check for NA
#A_dff[is.na(A_dff)]
A_dfff[is.na(A_dfff)]
tA_dff[is.na(tA_dff)]

#difference in value from the original database A and the transformed tA_dff. Should be equal to 0 
(sum(A_dff[,-1]) - sum(A))#/sum(A) *100 #très petite erreur
(sum(tA_dff[,-1:-2]) - sum(A)) /sum(A) *100 #1.956342e-14%

#Cleaning (noms des lignes et colonnes,...)
tA_dfff <- tA_dff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(nrow(tA_dff)),length(ncol(tA_dff)),
            dimnames = list(col.A_dfff,col.A_dfff)) %>%
  t() %>% #remettre dans le bon sens
  as.data.frame()  %>% `colnames<-`(col.A_dfff)
View(tA_dfff)

(sum(tA_dfff[,-1:-2]) - sum(A)) /sum(A) *100 #Erreur trop importante: 8,67% (5.88% avec les nouvelles données)

#A sans pondération: pas save
#erreur: -0.02663822% !!!

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

#Cleaning (noms des lignes et colonnes,...)
row.F_dfff <- str_c(F_dfff$countries.out,"_",F_dfff$products.out)
tF_dfff <- F_dfff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(row.F_dfff),length(F_noms_extensions),
            dimnames = list(row.F_dfff,F_noms_extensions)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(row.F_dfff)
View(tF_dfff)

#difference in value from the original database F and the transformed tF_dfff Should be equal to 0 
(sum(tF_dfff) - sum(Fe))  /sum(Fe) *100 #Erreur de 0.95% (1.52% avec les nouvelles données)
#sans pondération (nouveau bridge: erreur négligeable, de 1.268088e-14%)

### Save and export
br_pays <- "monde.12"
dir.create(str_c(path_out,br_pays, "_", br, "/"), recursive = TRUE)
path_loader <- str_c(path_out, br_pays,"_", br, "/")

saveRDS(tA_dfff, str_c(path_loader, "A_",br_pays,"_",br,".rds"))

saveRDS(tY_dfff, str_c(path_loader, "Y_",br_pays,"_",br,".rds"))

saveRDS(tF_dfff, str_c(path_loader, "Fe_",br_pays,"_",br,".rds"))

####Pour X et Z
#X
X <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/x.rds"))
X_df <- X %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                             products.in = str_sub(rownames(.),4))
X_df <- merge(X_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(production, ~ sum(. * weight)))  %>% ungroup() %>%
  select(production) %>% as.data.frame %>% `rownames<-`(col.A_dfff)
(sum(X_df) - sum(X)) /sum(X) *100 #petite erreur (0.79%) (0% sans pondération)
saveRDS(X_df, str_c(path_loader, "X_",br_pays,"_",br,".rds"))

#Z
Z <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Z.rds"))
Z_df <- Z %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))
Z_dfff <- merge(Z_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(A_cd), ~ sum(. * weight)))  %>% ungroup()
col.Z_dfff <- str_c(Z_dfff$countries.out,"_",Z_dfff$products.out)
#autre sens
tZ_dff <- Z_dfff %>% select(-countries.out,-products.out) %>% 
  as.matrix(length(col.Z_dfff),length(A_cd),
            dimnames = list(col.Z_dfff,A_cd)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.Z_dfff) %>%
  #transposée, maintenant bridge dans l'autre sens
  mutate(countries.in = str_sub(rownames(.),1,2),
         products.in = str_sub(rownames(.),4)) %>%
  merge(br_lg, by = "countries.in" ,all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in",all.x = TRUE) %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(col.Z_dfff), ~ sum(. * weight)))  %>% ungroup()
# Check for NA
#A_dff[is.na(A_dff)]
Z_dfff[is.na(Z_dfff)]
tZ_dff[is.na(tZ_dff)]
tZ_dfff <- tZ_dff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(nrow(tZ_dff)),length(ncol(tZ_dff)),
            dimnames = list(col.Z_dfff,col.Z_dfff)) %>%
  t() %>% #remettre dans le bon sens
  as.data.frame()  %>% `colnames<-`(col.Z_dfff)
View(tZ_dfff)
(sum(tZ_dfff) - sum(Z)) /sum(Z) *100 #erreur 1.96% ... (4.416829e-14% sans pondération)
saveRDS(tZ_dfff, str_c(path_loader, "Z_",br_pays,"_",br,".rds"))

####Pour S et M
#S
S <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/S.rds"))

S_df <- t(S) %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in = str_sub(rownames(.),4))

S_dfff <- merge(S_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(F_noms_extensions), ~ sum(. * weight)))  %>% ungroup()
View(S_dfff)

col.S_dfff <- str_c(S_dfff$countries.out,"_",S_dfff$products.out)

tS_dfff <- S_dfff %>% select(-countries.out,- products.out) %>% 
  as.matrix(length(nrow(S_dfff)),length(ncol(S_dfff)),
            dimnames = list(col.S_dfff,S_noms_extensions)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.S_dfff)
View(tS_dfff)
(sum(tS_dfff) - sum(S)) /sum(S) *100 #petite erreur (1.314899e-05 %) (3.246356e-14% sans pondération)
saveRDS(tS_dfff, str_c(path_loader, "S_",br_pays,"_",br,".rds"))

#M
M <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/M.rds"))
M_df <- t(M) %>% as.data.frame() %>% 
  mutate(countries.in = str_sub(rownames(.),1,2),
         types.df = str_sub(rownames(.),4)) %>%
  merge(., br_lg, by = "countries.in" , all.x = TRUE) 
M_dfff <- M_df %>% 
  group_by(countries.out,types.df) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(F_noms_extensions), ~ sum(.)))  %>% 
  ungroup() 
col.M_dfff = str_c(M_dfff$countries.out,"_",M_dfff$types.df)
tM_dff <- M_dfff %>% select(-countries.out,- types.df) %>% 
  as.matrix(length(col.M_dfff),length(M_noms_extensions),
            dimnames = list(col.Y_dfff,M_noms_extensions)) %>% 
  t() %>% as.data.frame()  %>% `colnames<-`(col.M_dfff)
(sum(tM_dff) - sum(M)) /sum(M) *100 #0% erreur
saveRDS(tM_dff, str_c(path_loader, "M_",br_pays,"_",br,".rds"))

#F_Y
Fy <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Fy.rds"))

Fy_df <- t(Fy) %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                            demand = str_sub(rownames(.),4))

Fy_dfff <- merge(Fy_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  group_by(countries.out, demand) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(F_noms_extensions), ~ sum(.)))  %>% ungroup()
col.Fy_dfff = str_c(Fy_dfff$countries.out,"_",Fy_dfff$demand)

tFy_dfff <- Fy_dfff %>% select(-countries.out,-demand) %>% 
  as.matrix(length(nrow(Fy_dfff)),length(col.Fy_dfff),
            dimnames = list(nrow(Fy_dfff),col.Fy_dfff)) %>%
  t() %>% as.data.frame()  %>% `colnames<-`(col.Fy_dfff)
(sum(tFy_dfff) - sum(Fy)) /sum(Fy) *100 #0% erreur
saveRDS(tFy_dfff, str_c(path_loader, "Fy_",br_pays,"_",br,".rds"))

