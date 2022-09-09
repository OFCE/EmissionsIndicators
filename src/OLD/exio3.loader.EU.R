Z <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Z.rds"))
Y <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y.rds"))
Fe <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/F.rds"))
X <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/x.rds"))

br <- "CPA2002_Niv1"
br_pays <- "EU1"
dir.create(str_c(path_out,br_pays, "_", br, "/"), recursive = TRUE)
path_loader <- str_c(path_out, br_pays,"_", br, "/")

#Y et Z ne marchent pas
Y_EU = perform.bridge(Y,
                      country_in = "Countries_1", country_out = "Countries_2.EU1", country_sht = "Countries", 
                      sec_in="exio3", sec_out="CPA2002_Niv1", sec_sht =  "Products",
                      sq_mat = FALSE, format_data = "data.frame")
Y.row <- str_c(Y_EU$countries,"_",Y_EU$products)
Y_EU <- Y_EU %>% select(-countries,-products) %>% `rownames<-`(Y.row) %>% t()

Z_EU = perform.bridge(Z,
                      country_in = "Countries_1", country_out = "Countries_2.EU1", country_sht = "Countries", 
                      sec_in="exio3", sec_out="CPA2002_Niv1", sec_sht =  "Products",
                      sq_mat = TRUE, satellite = FALSE, format_data = "data.frame")
Z.row <- str_c(Z_EU$countries,"_",Z_EU$products)
Z_EU <- Z_EU %>% select(-countries,-products) %>% `rownames<-`(Z.row) %>% t()

Fe_EU <- perform.bridge(Fe,country_in = "Countries_1", country_out = "Countries_2.EU1", country_sht = "Countries", 
                        sec_in = "exio3",sec_out =  "CPA2002_Niv1",sec_sht =  "Products", transpose = T,
                        sq_mat = F, satellite = T, format_data = "data.frame")
Fe.row <- str_c(Fe_EU$countries,"_",Fe_EU$products)
Fe_EU <- Fe_EU %>% select(-countries,-products) %>% `rownames<-`(Fe.row) %>% t() %>% as.data.frame()

X_EU <- perform.bridge(X,country_in = "Countries_1", country_out = "Countries_2.EU1", country_sht = "Countries", 
                       sec_in = "exio3",sec_out =  "CPA2002_Niv4",sec_sht =  "Products",
                       transpose = FALSE,vector=TRUE, format_data = "data.frame")
X.row <- str_c(X_EU$countries,"_",X_EU$products)
X_EU <- X_EU %>% select(-countries,-products) %>% `rownames<-`(X.row)

saveRDS(Z_EU, str_c(path_loader, "Z_",br_pays,"_",br,".rds"))

saveRDS(Y_EU, str_c(path_loader, "Y_",br_pays,"_",br,".rds"))

saveRDS(Fe_EU, str_c(path_loader, "Fe_",br_pays,"_",br,".rds"))

saveRDS(X_EU, str_c(path_loader, "X_",br_pays,"_",br,".rds"))
