### data loading

#A mettre dans header si besoin
#path_codedata <- str_c(path_user,".../src/")
path_codedata <- ""
Z <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Z.rds"))
Y <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/Y.rds"))
Fe <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/F.rds"))
X <- readRDS(str_c(path_codedata,"data_out/IOT_",year,"_",nom,"/x.rds"))

sec_in = "exio3"
sec_out = "CPA2002_Niv1"


Y.df <- perform.bridge(Y,country_in = "Countries_1", country_out = "Countries_2.WD1", country_sht = "Countries", 
                       sec_in , sec_out, sec_sht =  "Products",
                       format_data = "data.frame")

Z.df <- perform.bridge(Z,country_in = "Countries_1", country_out = "Countries_2.WD1", country_sht = "Countries", 
                       sec_in,sec_out ,sec_sht =  "Products",
                       sq_mat = T, format_data = "data.frame")

Fe.df <- perform.bridge(Fe,country_in = "Countries_1", country_out = "Countries_2.WD1", country_sht = "Countries", 
                       sec_in = "exio3",sec_out =  "CPA2002_Niv1",sec_sht =  "Products", transpose = T,
                       sq_mat = F, satellite = T, format_data = "data.frame")

X_df <- perform.bridge(X,country_in = "Countries_1", country_out = "Countries_2.WD1", country_sht = "Countries", 
                        sec_in = "exio3",sec_out =  "CPA2002_Niv1",sec_sht =  "Products", transpose =F,
                        sq_mat = F, satellite = T, format_data = "data.frame")


### Save and export
saveRDS(Z.df, str_c(path_out, "Z_",sec_out,".rds"))

saveRDS(Y.df, str_c(path_out, "Y_",sec_out,".rds"))

saveRDS(Fe.df, str_c(path_out, "Fe_",sec_out,".rds"))

saveRDS(X_df, str_c(path_out, "X_",sec_out,".rds"))

