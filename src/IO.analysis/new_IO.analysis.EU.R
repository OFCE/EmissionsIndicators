#Obtenir un dataframe avec les données pour un pays:
###Généralisation pour n'importe quel pays


br <- "CPA2002_Niv1"
br.pays <- "EU1"
# 
ghg <- "GES"
# Choix du pays considéré pour calcul empreinte carbone 
iso <- "FR"




path_loader <- str_c(path_out,br,"/",br.pays,"/")

# Chargement des données I-O sauvegardées par le script exio3.loader.R
Y <-readRDS(str_c(path_loader,"/Y_",br.pays,"_",br,".rds"))
Fe <-readRDS(str_c(path_loader,"/Fe_",br.pays,"_",br,".rds")) %>% t
Z <-readRDS(str_c(path_loader,"/Z_",br.pays,"_",br,".rds"))
X <-readRDS(str_c(path_loader,"/X_",br.pays,"_",br,".rds"))

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
Fe.ghg <- GHG.extraction(Fe ,ghg) %>% as.matrix()

##Inverse de Leontief
L <- LeontiefInverse(t(Z), coef = FALSE)

##Vecteur de demande pour un pays iso
Y.vec <- shock.demand(Y, iso, aggregate = TRUE) 

##Vecteur de demande pour un pays iso
Y.vec <- shock.demand(Y, aggregate = TRUE) 

#Matrice S (impact producteur/ million €)
S <- Env.multiplier(Y.vec, Fe.ghg, L)

#Matrice M (impact demande et CI kg emissions/ million €)
M <- diag(as.numeric(S)) %*% L 

# Volume d'émissions producteur
EMS_S <- S %*% diag(as.numeric(x)) %>% `colnames<-`(rownames(X))
# Volume d'émissions consommateur pays iso
EMS_M <- M %*% diag(as.numeric(Y.vec)) %>%
  `colnames<-`(rownames(X)) %>% `colnames<-`(rownames(Y))

#  Verif comptabilité carbone source et output
sum(Fe.ghg) / 10^12
sum(EMS_M) / 10^12
sum(EMS_S) / 10^12

# PIB monde ou pays si iso TRUE 
sum(Y.vec)/10^3

# Mise en forme des résultats pour export
Y_export <-  (Y.vec) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.)))  %>% 
  merge(names_y, ., by = "id") %>% select(-id) %>% 
  pivot_wider(names_from = countries, values_from = "V1") 

# GES_M_export <-  t(EMS_M) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
#   merge(names_y, by = "id") %>% select(-id) %>%
#   pivot_wider(names_from = countries, values_from = "value") 

GES_S_export <-  t(EMS_S) %>% as.data.frame() %>%mutate(id = seq(1:nrow(.))) %>% 
  merge(names_io, by = "id") %>% select(-id) %>%
  pivot_wider(names_from = countries, values_from = "value")


# Export results
saveRDS(Y_export, str_c(path_loader,"Imports.Y_",iso,".rds"))
saveRDS(GES_M_export, str_c(path_loader,"M_",iso,"_",ghg,".rds"))
saveRDS(GES_S_export, str_c(path_loader,"S_",iso,"_",ghg,".rds"))
