
# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y  <- readRDS(str_c(path_loader,"Y_",br.pays,"_",br,".rds"))
Fe <- readRDS(str_c(path_loader,"Fe_",br.pays,"_",br,".rds"))
Z  <- readRDS(str_c(path_loader,"Z_",br.pays,"_",br,".rds"))
X  <- readRDS(str_c(path_loader,"X_",br.pays,"_",br,".rds"))

if (nom == "ixi"){
  Fy <- readRDS(str_c(path_loader,"Fy_",br.pays,"_",br,".rds"))  
}
# Error in percentage on the supply-use balance (from data source)
100 * (rowSums(Y) + rowSums(Z) - X[,1]) /X[,1]



## Mutate with string 
names_y <- rownames(Y) |> as.data.frame() %>%
  mutate(id = seq(1:length(.)), 
         countries = str_extract(.,"^.+?(?=_)"),
         products = str_extract(.,"(?<=_)(.*)")) |> 
  select(id, countries, products)

names_io <-rownames(Z) |> as.data.frame() %>%
  mutate(id = seq(1:length(.)), 
         countries = str_extract(.,"^.+?(?=_)"),
         products = str_extract(.,"(?<=_)(.*)")) |> 
  select(id, countries, products)

# Extraction of the emissions from production activities
rm(list = ls()[grep("^Fe.", ls())])

# indirect emissions
for(ghg in glist){
  Fe.ghg <- GHG.extraction(Fe,ghg) |> as.matrix()
  
  Fe.CO2eq <- as.data.frame(unlist(GHGToCO2eq(Fe.ghg)))
  Fe.CO2eq$id <- seq(1,  nrow(Fe.CO2eq))
  Fe.CO2eq$ghg <- str_c(ghg)
  assign(str_c("Fe.CO2eq.",ghg),Fe.CO2eq)
  print(str_c(round(sum(Fe.CO2eq$value)/10^12,3), " GtCO2e of ",ghg," (indirect emissions)"))
  rm(Fe.ghg,Fe.CO2eq)
}

Fe.ghg <- do.call("rbind",mget(ls(pattern = "^Fe._*"))) |> 
  pivot_wider(names_from = ghg,
              values_from = value) |>
  select(-id) %>% 
  mutate(GES = rowSums(.), 
         name = colnames(Z)) 

# direct emissions 
if (nom == "ixi"){
  rm(list = ls()[grep("^Fy.", ls())])
  for(ghg in glist){
    
    Fy.ghg <- GHG.extraction(Fy,ghg) |> as.matrix()
    
    Fy.CO2eq <- as.data.frame(unlist(GHGToCO2eq(Fy.ghg))) %>%
      mutate(id = seq(1,  nrow(.)),
             ghg = str_c(ghg)) 
    
    assign(str_c("Fy.CO2eq.",ghg),Fy.CO2eq)
    print(str_c(round(sum(Fy.CO2eq$value)/10^12,3), " GtCO2e of ",ghg," (direct emissions)"))
    rm(Fy.ghg,Fy.CO2eq)
  }
  
  Fy.ghg <- do.call("rbind",mget(ls(pattern = "^Fy._*"))) |> 
    pivot_wider(names_from = ghg,
                values_from = value) |>
    select(-id) %>% 
    mutate(GES = rowSums(.), 
           name = colnames(Y), 
           country = str_sub(name,1,2), 
           item = str_sub(name,4))
  
  9.341933e+07 #US_Final consumption expenditure by government CO2
  #select(Fy.ghg, name, all_of(glist), GES)
  #BR_Final consumption expenditure… 1.72e 8 CO2
  
  
  sum(Fy.ghg$GES)/10^12
  
  ED.tot_1_volume <- Fy.ghg |> filter(country == iso) |> select(country, item, glist)
  print(str_c("for country ",iso,": ",round(sum(ED.tot_1_volume[[str_c(ghg)]]/10^9),2)," MtCO2e of ",ghg," emissions (direct emissions)")) 
}

# 41.18379 Gt CO2e worldwide
sum(Fe.ghg$GES)/10^12


### I-O Calculus


##Vecteur de demande pour un pays iso
Y_all.vec <- shock.demand(Y, aggregate = TRUE) 
482000.2/(17119.8     + 0.0   +   0.0 +523195.6 +974376.1 + 213329.9 + 482000.2)
4111406.0/(178897.8     +   0.0   +    0.0 +1609343.7 + 2715372.9 +1032941.8 +4111406.0)
##Inverse de Leontief
L   <- LeontiefInverse(Z, X, coef = FALSE)
L_1 <- LeontiefInverse(Z, X, coef = FALSE, direct = TRUE)

x <- L %*% Y_all.vec

##Vecteur de demande pays iso
Y.vec <- shock.demand(Y,iso, aggregate = TRUE) 

#### Impacts factors ####
for(ghg in glist){
  
  # Coefficient d'intensité carbone S =  F / x
  S <- (Fe.ghg[[str_c(ghg)]]/x) |> as.vector() |> `names<-`(rownames(X))
  S[is.infinite(S)] <- 0 
  S[is.na(S)] <- 0 
  
  # Coefficient d'intensité carbone M  =  S %*% L / M_1 = S %*% L_1
  M <- diag(S) %*% L 
  M_1 <- diag(S) %*% L_1 
  dY <- diag(as.vector(Y.vec))
  
  M * Z
  
  # Volume d'émissions =  S %*% L /\ M_1 = S %*% L_1
  S_volume <- S %*% diag(as.vector(x)) |> `colnames<-`(rownames(Y)) |> `rownames<-`("EMS_S")
  M_volume <- (M %*% diag(as.vector(Y.vec))) |> `colnames<-`(rownames(Y)) |> `rownames<-`(rownames(Y))
  M_1_volume <- (M_1 %*% diag(as.vector(Y.vec)))|> `rownames<-`(rownames(Y))
  M.tot_volume <- (M  %*% as.matrix(Y.vec))  |> `rownames<-`(rownames(Y))
  M.tot_1_volume <- (M_1  %*% as.matrix(Y.vec))   |> `rownames<-`(rownames(Y))
  
  
  Z.iso <- Z
  Z.iso[-str_which(rownames(Z.iso),as.character(iso)),] <- 0
  CI.M.tot_volume <- (M %*% Z.iso) |> `colnames<-`(rownames(Y)) |> `rownames<-`(rownames(Y))
  CI.M.tot_1_volume <- (M_1  %*% Z.iso)   |> `rownames<-`(rownames(Y))
  
  sum(M %*% (Z))/10^9
  # CIM_volume <- (S %*% t(Z)) |> `colnames<-`(rownames(Y)) |> `rownames<-`(rownames(Y))
  # CIM_1_volume <- (M_1 %*% Z) |> `colnames<-`(rownames(Y)) |> `rownames<-`(rownames(Y))
  
  
  print(str_c("for country ",iso,": ",round(sum(M_volume/10^9),2)," MtCO2e of ",ghg," emissions (indirect emissions)")) 
  
  
  
  # Mise en forme des résultats pour export
  factor_M_export <-  M |> as.data.frame() %>%
    mutate(id = seq(1:nrow(.))) |> 
    merge(names_y, by = "id") |> select(-id) |>
    `colnames<-`(c(rownames(Z), "countries", "products")) |>
    select("countries","products", rownames(Z))
  
  factor_M.dir_export <-  M_1 |> as.data.frame() %>%
    mutate(id = seq(1:nrow(.))) |> 
    merge(names_y, by = "id") |> select(-id) |>
    `colnames<-`(c(rownames(Z), "countries", "products")) |>
    select("countries","products", rownames(Z))
  
  factor_S_export <-  S |> as.data.frame() |>
    mutate(id = seq(1:length(S))) |> 
    merge(names_io, by = "id") |> select(-id)  |> 
    `colnames<-`(c("value", "countries", "products")) |>
    pivot_wider(names_from = countries, values_from = "value")
  
  M.dir_export <-  M_1_volume |> as.data.frame() %>%
    mutate(id = seq(1:nrow(.))) |> 
    merge(names_y, by = "id") |> select(-id) |>
    `colnames<-`(c(rownames(Z), "countries", "products")) |>
    select("countries","products", rownames(Z))
  
  M_export <-  M_volume |> as.data.frame() %>% 
    mutate(id = seq(1:nrow(.))) %>% 
    merge(names_y, by = "id") |> select(-id) |>
    `colnames<-`(c(rownames(Z), "countries", "products")) |>
    select("countries","products", rownames(Z))
  
  S_export <-  t(S_volume) |> as.data.frame() %>%
    mutate(id = seq(1:length(S_volume))) |> 
    merge(names_io, by = "id") |> select(-id)  |> 
    `colnames<-`(c("value", "countries", "products")) |>
    pivot_wider(names_from = countries, values_from = "value")
  
  Y_export <-  (Y.vec) |> as.data.frame() %>%
    mutate(id = seq(1:nrow(.)))  %>% 
    merge(names_y, ., by = "id") |> select(-id) |> 
    pivot_wider(names_from = countries, values_from = "V1") 
  
  ### Export results
  # factors of emissions
  saveRDS(factor_M_export, str_c(path_loader,"fac.M_",ghg,"_",iso,".rds"))
  saveRDS(factor_M.dir_export, str_c(path_loader,"fac.M_dir_",ghg,"_",iso,".rds"))
  saveRDS(factor_S_export, str_c(path_loader,"fac.S",ghg,"_",iso,".rds"))
  
  #  Emissions volume
  saveRDS(M_volume, str_c(path_loader,"M.mat_",ghg,"_",iso,".rds"))
  saveRDS(M_1_volume, str_c(path_loader,"M_dir.mat_",ghg,"_",iso,".rds"))
  saveRDS(S_volume, str_c(path_loader,"S.mat_",ghg,".rds"))
  
  
  saveRDS(M_export, str_c(path_loader,"M_",ghg,"_",iso,".rds"))
  saveRDS(M.dir_export, str_c(path_loader,"M_dir_",ghg,"_",iso,".rds"))
  saveRDS(S_export, str_c(path_loader,"S_",ghg,".rds"))
  
  # IF SECTORS BASED
  if (nom == "ixi"){
    saveRDS(ED.tot_1_volume, str_c(path_loader,"ED.mat_",ghg,"_",iso,".rds"))
  }
  
}

saveRDS(Y_export, str_c(path_loader,"Y_",iso,".rds"))
