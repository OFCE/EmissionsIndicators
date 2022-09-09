
# Choix du pays considéré pour calcul empreinte carbone 
br <- "ThreeME"
br.pays <- "OG"
year=2015

# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_loader,"Y_",br.pays,"_",br,".rds"))
Fe <-readRDS(str_c(path_loader,"Fe_",br.pays,"_",br,".rds")) %>% t
Z <-readRDS(str_c(path_loader,"Z_",br.pays,"_",br,".rds"))
X <-readRDS(str_c(path_loader,"X_",br.pays,"_",br,".rds"))

## Mutate with string 
names_y <- rownames(Y) %>% as.data.frame() %>%
  mutate(id = seq(1:length(.)), 
         countries = str_extract(.,"^.+?(?=_)"),
         products = str_extract(.,"(?<=_)(.*)")) %>% 
  select(id, countries, products)

names_io <-rownames(Z) %>% as.data.frame() %>%
  mutate(id = seq(1:length(.)), 
         countries = str_extract(.,"^.+?(?=_)"),
         products = str_extract(.,"(?<=_)(.*)")) %>% 
  select(id, countries, products)

# Extraction of the emissions from production activities



rm(list = ls()[grep("^Fe.", ls())])

for(ghg in glist){
  
  Fe.ghg <- GHG.extraction(Fe,ghg) %>% as.matrix()
  
  Fe.CO2eq <- as.data.frame(unlist(GHGToCO2eq(Fe.ghg)))
  Fe.CO2eq$id <- seq(1,1323)
  Fe.CO2eq$ghg <- str_c(ghg)
  assign(str_c("Fe.CO2eq.",ghg),Fe.CO2eq)
  print(sum(Fe.CO2eq$value))
  rm(Fe.ghg,Fe.CO2eq)
}


Fe.ghg <- do.call("rbind",mget(ls(pattern = "^Fe._*")))
Fe.ghg  <- pivot_wider(Fe.ghg,
                       names_from = ghg,
                       values_from = value) %>% select(-id)

Fe.ghg$GES <- rowSums(Fe.ghg)
Fe.ghg$name <- rownames(Z)




##Inverse de Leontief
L <- LeontiefInverse(t(Z), coef = FALSE)

##Vecteur de demande pour un pays iso
Y.vec <- shock.demand(Y, iso, aggregate = TRUE) 

##Vecteur de demande mondiale
Y.vec <- shock.demand(Y, aggregate = TRUE) 

#Matrice S (impact producteur/ million €)
S <- Env.multiplier(Y.vec, Fe.ghg["GES"], L)

#Matrice M (impact demande et CI kg emissions/ million €)
M <- diag(as.numeric(S)) %*% L 

# Volume d'émissions producteur
EMS_S_0 <- S %*% diag(as.numeric(unlist(X))) %>% `colnames<-`(rownames(X))
EMS_S <- Env.multiplier(Y.vec, Fe.ghg["GES"], L, volume = T)

# Volume d'émissions consommateur pays iso
EMS_M_0 <- M %*% diag(as.numeric(Y.vec)) %>%
  `colnames<-`(colnames(Z)) %>% `rownames<-`(rownames(Y)) 

EMS_M <- M %*% (as.numeric(Y.vec))   %>%
  `rownames<-`(rownames(Y))  %>% as.data.frame() %>%
  mutate(id = seq(1:length(.)))
# EMS_M %>% as.data.frame() %>%
#   mutate(id = seq(1:length(.)), 
#          countries = str_extract(rownames(.),"^.+?(?=_)"),
#          products = str_extract(rownames(.),"(?<=_)(.*)")) %>% 
#   select(id, countries, products) %>% group_by()

#  Verif comptabilité carbone source et output
sum(Fe.ghg["GES"]) / 10^12
sum(EMS_M) / 10^12
sum(EMS_S) / 10^12

# PIB monde ou pays si iso TRUE 
sum(Y.vec)/10^3

# Mise en forme des résultats pour export
Y_export <-  (Y.vec) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.)))  %>% 
  merge(names_y, ., by = "id") %>% select(-id) %>% 
  pivot_wider(names_from = countries, values_from = "V1") 


factor_M_export <-  (M) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  `rownames<-`(rownames(Y))

factor_S_export <-  t(S) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_io, by = "id") %>% select(-id)  %>% 
  `colnames<-`(c("value", "countries", "products")) %>%
  pivot_wider(names_from = countries, values_from = "value")


GES_S_export <-  t(EMS_S) %>% as.data.frame() %>% mutate(id = seq(1:nrow(.))) %>% 
  merge(names_io, by = "id") %>% select(-id) %>%
  `colnames<-`(c("value", "countries", "products")) %>%
  pivot_wider(names_from = countries, values_from = "value")


GES_M_export <-  (EMS_M) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  pivot_wider(names_from = countries, values_from = "V1") 

GES_M.TOT <- (GES_M_export[-1]/10^9) %>% rowSums %>% bind_cols(GES_M_export[1],.)
GES_S.TOT <- (GES_S_export[-1]/10^9) %>% rowSums %>% bind_cols(GES_S_export[1],.)


# Export results

# factors of emissions
saveRDS(factor_M_export, str_c(path_loader,"fac.M_",ghg,".rds"))
saveRDS(factor_S_export, str_c(path_loader,"fac.S_",ghg,".rds"))


saveRDS(EMS_M, str_c(path_loader,"M.mat_",ghg,".rds"))
saveRDS(EMS_S, str_c(path_loader,"S.mat_",ghg,".rds"))


saveRDS(GES_M_export, str_c(path_loader,"M_",ghg,".rds"))
saveRDS(GES_S_export, str_c(path_loader,"S_",ghg,".rds"))