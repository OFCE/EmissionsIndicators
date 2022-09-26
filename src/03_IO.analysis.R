
# Choix du pays considéré pour calcul empreinte carbone 
br      <- "ThreeME"
br.pays <- "OG"
year    <- 2015
path_loader <- str_c(path_out, br, "/",br.pays,"/")

# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y  <- readRDS(str_c(path_loader,"Y_",br.pays,"_",br,".rds"))
Fe <- readRDS(str_c(path_loader,"Fe_",br.pays,"_",br,".rds"))
Z  <- readRDS(str_c(path_loader,"Z_",br.pays,"_",br,".rds"))
X  <- readRDS(str_c(path_loader,"X_",br.pays,"_",br,".rds"))

# Error in percentage on the supply-use balance (from data source)
100 * (rowSums(Y) + rowSums(Z) - X[,1]) /X[,1]



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
  Fe.CO2eq$id <- seq(1,  nrow(Fe.CO2eq))
  Fe.CO2eq$ghg <- str_c(ghg)
  assign(str_c("Fe.CO2eq.",ghg),Fe.CO2eq)
  print(str_c(round(sum(Fe.CO2eq$value)/10^12,3), " MtCO2e of ",ghg))
  rm(Fe.ghg,Fe.CO2eq)
}

Fe.ghg <- do.call("rbind",mget(ls(pattern = "^Fe._*"))) 
#Fe.CO2eq.CH4.AT_Products of Forestry

#4.104604e+06
#4.104604e+06

Fe.ghg <- pivot_wider(Fe.ghg,
                      names_from = ghg,
                      values_from = value) %>% select(-id)

Fe.ghg$GES <- rowSums(Fe.ghg)
Fe.ghg$name <- rownames(Z)
 # AT_Air transport 4152130239     28*1.867419e+10



#29.96622 GtCO2e /  GtCO2e  7.951345 CH4 / N2O 2.065128


# 41.18379 Gt CO2e worldwide
sum(Fe.ghg$GES)/10^12

### I-O Calculus
##Inverse de Leontief
L <- LeontiefInverse(Z, X, coef = FALSE)

L_1 <- LeontiefInverse(Z,X, coef = F, direct = TRUE)

x <- L %*% Y.vec
##Vecteur de demande pour un pays iso
Y.vec <- shock.demand(Y, iso, aggregate = TRUE) 

##Vecteur de demande mondiale
#Y.vec <- shock.demand(Y, aggregate = TRUE) 



#### Impacts factors ####
for(ghg in glist){
  
# Coefficient d'intensité carbone S =  F / x
S <- (Fe.ghg[[str_c(ghg)]]/x) %>% as.vector() %>% `names<-`(rownames(X))
S[is.infinite(S)] <- 0 
S[is.na(S)] <- 0 

# Coefficient d'intensité carbone M  =  S %*% L / M_1 = S %*% L_1
M <- diag(S) %*% L 
M_1 <- diag(S) %*% L_1 

# Volume d'émissions =  S %*% L /\ M_1 = S %*% L_1
S_volume <- S %*% diag(as.vector(x)) %>% `colnames<-`(rownames(Y)) %>% `rownames<-`("EMS_S")
M_volume <- (M %*% diag(as.vector(Y.vec))) %>% `colnames<-`(rownames(Y)) %>% `rownames<-`(rownames(Y))
M_1_volume <- (M_1 %*% diag(as.vector(Y.vec))) %>% `colnames<-`(rownames(Y)) %>% `rownames<-`(rownames(Y))

# CIM_volume <- (S %*% t(Z)) %>% `colnames<-`(rownames(Y)) %>% `rownames<-`(rownames(Y))
# CIM_1_volume <- (M_1 %*% Z) %>% `colnames<-`(rownames(Y)) %>% `rownames<-`(rownames(Y))


print(str_c("for country ",iso,": ",round(sum(M_volume/10^9),2)," MtCO2e of ",ghg," emissions (consumer-based approach)"))  






# Mise en forme des résultats pour export
factor_M_export <-  M %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  `colnames<-`(c(rownames(Z), "countries", "products")) %>%
  select("countries","products", rownames(Z))

factor_M.dir_export <-  M_1 %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  `colnames<-`(c(rownames(Z), "countries", "products")) %>%
  select("countries","products", rownames(Z))

factor_S_export <-  S %>% as.data.frame() %>%mutate(id = seq(1:length(S))) %>% 
  merge(names_io, by = "id") %>% select(-id)  %>% 
  `colnames<-`(c("value", "countries", "products")) %>%
  pivot_wider(names_from = countries, values_from = "value")

M.dir_export <-  M_1_volume %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  `colnames<-`(c(rownames(Z), "countries", "products")) %>%
  select("countries","products", rownames(Z))

M_export <-  M_volume %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_y, by = "id") %>% select(-id) %>%
  `colnames<-`(c(rownames(Z), "countries", "products")) %>%
  select("countries","products", rownames(Z))

S_export <-  t(S_volume) %>% as.data.frame() %>%mutate(id = seq(1:length(S_volume))) %>% 
  merge(names_io, by = "id") %>% select(-id)  %>% 
  `colnames<-`(c("value", "countries", "products")) %>%
  pivot_wider(names_from = countries, values_from = "value")

Y_export <-  (Y.vec) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.)))  %>% 
  merge(names_y, ., by = "id") %>% select(-id) %>% 
  pivot_wider(names_from = countries, values_from = "V1") 


#GES_M.TOT <- (GES_M_export[-1]/10^9) %>% rowSums %>% bind_cols(GES_M_export[1],.)
#GES_S.TOT <- (GES_S_export[-1]/10^9) %>% rowSums %>% bind_cols(GES_S_export[1],.)


# Export results

# factors of emissions
saveRDS(M, str_c(path_loader,"fac.M_",ghg,"_",iso,".rds"))
saveRDS(S, str_c(path_loader,"fac.S_",ghg,"_",iso,".rds"))

#  Emissions volume
saveRDS(M_volume, str_c(path_loader,"M.mat_",ghg,"_",iso,".rds"))
saveRDS(S_volume, str_c(path_loader,"S.mat_",ghg,".rds"))


saveRDS(M_export, str_c(path_loader,"M_",ghg,"_",iso,".rds"))
saveRDS(S_export, str_c(path_loader,"S_",ghg,".rds"))
}
