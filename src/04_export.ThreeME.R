source("src/00_header.R")


other_countries <- get(str_c("countries_",br.pays,".desc"))["codes"][iso !=  get(str_c("countries_",br.pays,".desc"))["codes"]] |>
  unlist() |> as.vector()


#br.pays <- country_out |> str_remove("Countries_2.")
wb <- createWorkbook("ThreeME calibration")
wb_dir <- createWorkbook("ThreeME calibration.2")

# Adding the exports in monetary values into the workbook with each sheet per type of ghg

for (ghg in c(glist)){
  
  #Extracting the data and taking only the imported emissions 
  data_fac.M <- readRDS(str_c(path_loader,"fac.M_",ghg,"_",iso,".rds"))  %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts)
  
    X  <- readRDS(str_c(path_loader,"X_",br.pays,"_",br,".rds"))  |> as.data.frame() %>%
    mutate(countries = str_sub(rownames(.),1,2),
           products = str_sub(rownames(.),4)) |>
    pivot_wider(names_from = countries,
                values_from = indout_0) |> select("products", all_of(other_countries))
  
  
  EMS.M <- readRDS(str_c(path_loader,"M_",ghg,"_",iso,".rds"))  %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts)
  
  EMS.M_dir <- readRDS(str_c(path_loader,"M_dir_",ghg,"_",iso,".rds"))  %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts)
  
  # Emissions par produit et par pays d'origine
  data_EMS.M <- table.import(EMS.M,transpose = F)  %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |> select(-nProducts) |>
    arrange(match(codes,ThreeME_c28.desc$codes)) |> select("products", "codes",all_of(other_countries))
  
  
  data_EMS.M_dir <- table.import(EMS.M_dir,transpose = F)  %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts) |>
    arrange(match(codes,ThreeME_c28.desc$codes)) |> select("products", "codes", all_of(other_countries))
  
  # Intensity by products
  data_IEMS_M <- cbind("products" = data_EMS.M[,1], data_EMS.M[-1:-2]/X[,-1]) %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts) |>
    arrange(match(codes,ThreeME_c28.desc$codes)) |>
    select("products", "codes",other_countries) %>%
    mutate_if(is.numeric, ~ replace_na(., 0) %>%
                replace(., is.infinite(.), 0))
  
  data_IEMS_M_dir <- cbind("products" = data_EMS.M_dir[,1], data_EMS.M_dir[,-1:-2]/X[,-1]) %>%
    merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
    select(-nProducts) |>
    arrange(match(codes,ThreeME_c28.desc$codes)) |>
    select("products", "codes",other_countries) %>%
    mutate_if(is.numeric, ~ replace_na(., 0) %>%
                replace(., is.infinite(.), 0))
  
  
  
  
  # Export of results in rds format
  saveRDS(data_IEMS_M, str_c(path_loader,"fac.M_",ghg,"_",iso,"_imp.rds"))
  saveRDS(data_IEMS_M_dir, str_c(path_loader,"fac.M_dir_",ghg,"_",iso,"_imp.rds"))
  

  saveRDS(data_EMS.M, str_c(path_loader,"EMS.M_",ghg,"_",iso,"_imp.rds"))
  saveRDS(data_EMS.M_dir, str_c(path_loader,"EMS.M.dir_",ghg,"_",iso,"_imp.rds"))
  
  # Writing the data into a excel workbook sheet 
  addWorksheet(wb, str_c(ghg))
  writeData(wb, str_c(ghg), get(str_c("data_EMS.M")), rowNames = TRUE)
  
  addWorksheet(wb, str_c("fac_",ghg))
  writeData(wb, str_c("fac_",ghg), data_IEMS_M, rowNames = TRUE)
  
  
  # Writing the data into a excel workbook sheet 
  addWorksheet(wb_dir, str_c(ghg))
  writeData(wb_dir, str_c(ghg), get(str_c("data_EMS.M_dir")), rowNames = TRUE)
  
  addWorksheet(wb_dir, str_c("fac_",ghg))
  writeData(wb_dir, str_c("fac_",ghg), get(str_c("data_IEMS_M_dir")), rowNames = TRUE)
  
  #(select(EMS.M[-1:-3], !starts_with("FR"))/10^9) |> sum
  
  (data_EMS.M[-1:-2]/10^9)|> sum()
}

#data_EMS.M[-1:-2] |> sum()/10^9
# Adding the exports in monetary values
data_M <- readRDS(str_c(path_loader,"Y_",iso,".rds"))  %>%
  #select(-iso) |>
  merge(sec.desc[[str_c(br,".desc")]],., by = "products") |>
  arrange(match(codes,ThreeME_c28.desc$codes)) |> select("products", "codes", other_countries)

# Writing the data into a excel workbook sheet
addWorksheet(wb, "IMPORTS")
writeData(wb, "IMPORTS", data_M, rowNames = TRUE)

addWorksheet(wb_dir, "IMPORTS")
writeData(wb_dir, "IMPORTS", data_M, rowNames = TRUE)



# Exporting the results in the path_out folder
saveWorkbook(wb, file = str_c(path_user, path_export,"DATA.",iso,"_GHG_IMPORTED.xlsx"), overwrite = TRUE)
saveWorkbook(wb_dir, file = str_c(path_user, path_export,"DATA.",iso,"_GHG_DIR_IMPORTED.xlsx"), overwrite = TRUE)

