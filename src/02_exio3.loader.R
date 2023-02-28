### data loading
source("src/00_header.R")

Z.raw <- readRDS(str_c("data_in/IOT/IOT_",year,"_",nom,"/Z.rds"))
Y.raw <- readRDS(str_c("data_in/IOT/IOT_",year,"_",nom,"/Y.rds"))
Fe.raw <- readRDS(str_c("data_in/IOT/IOT_",year,"_",nom,"/F.rds"))
X.raw <- readRDS(str_c("data_in/IOT/IOT_",year,"_",nom,"/x.rds"))


### 
sec_in = "exio3"
sec_out = br
sec_sht =  "Products"

country_in = "Countries_1"
country_out = str_c("Countries_2.",br.pays)
country_sht = "Countries"

Y.df <- perform.bridge(Y.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out, sec_sht,
                       format_data = "matrix", index = TRUE, countries.row = TRUE)
sum(Y.df) - sum(Y.raw)

Z.df <- perform.bridge(Z.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out,sec_sht,
                       sq_mat = T, format_data = "matrix", index = TRUE)
sum(Z.df) - sum(Z.raw)

Fe.df <- perform.bridge(Fe.raw,country_in, country_out, country_sht, 
                       sec_in, sec_out, sec_sht, transpose = T,
                       sq_mat = FALSE, satellite = T, format_data = "matrix", index = TRUE) %>% t 
sum(Fe.df) - sum(Fe.raw)

X.df <- perform.bridge(X.raw, country_in, country_out, country_sht, 
                        sec_in, `sec_out`,sec_sht,
                        sq_mat = FALSE,  format_data = "matrix", index = TRUE)
sum(X.df) - sum(X.raw)

#Y.raw %>% as.data.frame() %>% filter(str_detect( row.names(.), "CN_Products of forestry")) %>% View()
#Y.df %>% as.data.frame() %>% filter(str_detect( row.names(.), "CN_Products of Forestry")) %>% View()



### Save and export
dir.create(str_c(path_out,br,"/",br.pays), recursive = TRUE)
saveRDS(Z.df, str_c(path_out,br,"/",br.pays, "/Z_",br.pays,"_",br,".rds"))

saveRDS(Y.df, str_c(path_out,br,"/",br.pays, "/Y_",br.pays,"_",br,".rds"))

saveRDS(Fe.df, str_c(path_out,br,"/",br.pays, "/Fe_",br.pays,"_",br,".rds"))

saveRDS(X.df, str_c(path_out,br,"/",br.pays, "/X_",br.pays,"_",br,".rds"))

