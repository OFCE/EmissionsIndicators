### data loading

#A mettre dans header si besoin
#path_codedata <- str_c(path_user,".../src/")

Z.raw <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Z.rds"))
Y.raw <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/Y.rds"))
Fe.raw <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/F.rds"))
X.raw <- readRDS(str_c("data_out/IOT_",year,"_",nom,"/x.rds"))

sec_in = "exio3"
sec_out = "ThreeME_c28"
sec_sht =  "Products"

country_in = "Countries_1"
country_out = "Countries_1"
country_sht = "Countries"

br ="ThreeME"
br.pays ="OG"

Y.df <- perform.bridge(Y.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out, sec_sht,
                       format_data = "matrix", vector = T)


Z.df <- perform.bridge(Z.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out,sec_sht,
                       sq_mat = T, format_data = "matrix")


Fe.df <- perform.bridge(Fe.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out, sec_sht, transpose = T,
                       sq_mat = F, satellite = T, format_data = "matrix")


X.df <- perform.bridge(X.raw, country_in, country_out, country_sht, 
                        sec_in, sec_out,sec_sht,
                        sq_mat = F, satellite = T, vector = T,  format_data = "matrix")


### Save and export
dir.create(str_c(path_out,br,"/",br.pays), recursive = TRUE)
saveRDS(Z.df, str_c(path_out,br,"/",br.pays, "/Z_",br.pays,"_",br,".rds"))

saveRDS(Y.df, str_c(path_out,br,"/",br.pays, "/Y_",br.pays,"_",br,".rds"))

saveRDS(Fe.df, str_c(path_out,br,"/",br.pays, "/Fe_",br.pays,"_",br,".rds"))

saveRDS(X.df, str_c(path_out,br,"/",br.pays, "/X_",br.pays,"_",br,".rds"))
