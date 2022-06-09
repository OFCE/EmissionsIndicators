### data loading
#path_codedata <- str_c(path_user, "location of the script") 
EmissionsIndicators/data.out/IOT_2015_pxp/A.rds
A <-readRDS(str_c("data.out/IOT_",year,"_",nom,"/A.rds"))
Y <-readRDS(str_c(path_codedata,"data.out/IOT_",year,"_",nom,"/Y.rds"))
Fe <-readRDS(str_c("data.out/IOT_",year,"_",nom,"/Fe.rds"))

Y_d <- readRDS(str_c(path_codedata, "data.out/IOT_",year,"_",nom,"/Y_types_DF.rds"))
Y_cd <- readRDS(str_c(path_codedata, "data.out/IOT_",year,"_",nom,"/Y_pays_types_DF.rds"))
Y_c <- readRDS(str_c(path_codedata, "data.out/IOT_",year,"_",nom,"/Y_pays.rds"))

Y_df <- Y %>% as.data.frame() %>% mutate(countries.in = str_sub(rownames(.),1,2),
                                         products.in= str_sub(rownames(.),4))

### Bridge in long format for countries
br_lg <-  loadBridge("Countries_1", "Countries_2.WD1", "Countries") %>%
  data.frame() %>% mutate(countries.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$countries, names_to = "countries.in", values_to = "value") %>%
  filter(value == 1) %>% select(-value)

### Bridge in long format for products
br.2_lg <-  loadBridge("exio3", "threeMeEurostat", "Products", transverse = T) %>%
  as.data.frame() %>% mutate(products.out = rownames(.)) %>%
  pivot_longer(cols = exio3.desc$products, names_to = "products.in", values_to = "value") %>%
  filter(value == 1) %>% select(-value)

### Merge and summarize of rows
Y_dff <- merge(Y_df, br_lg, by = "countries.in" , all.x = TRUE) %>%
  merge(., br.2_lg, by = "products.in") %>%
  group_by(countries.out, products.out) %>%
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out, products.out) %>% ungroup()

Y_dff <- merge(Y_df, br_lg, by = "countries.in" , all = F) %>%
  group_by(countries.out) %>%
  summarise_at(Y_cd,~sum(.x)) %>% arrange(countries.out) %>% ungroup()

countries_data <- Y_c %>% unique()
countries_list<- exio3.desc$countries%>% unique()
#34780.8
Y_dff[,2] %>% sum

Y[,1] %>% sum()
# 9002674 tot sum
Y_dff %>% select(-countries.out, - products.out) %>% sum()
Y_dff %>% select(-countries.out) %>% sum()

#67492247 tot sum
Y %>% sum()
