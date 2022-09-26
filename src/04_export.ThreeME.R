library(openxlsx)



# folder in which the results are saved
path_user <-  "/Users/paul/Documents/Professionnel/"
path_export <- "ThreeME_V3/data/France/"
# Choose of the year and the country (iso) for the data 
iso <- "FR"

# Choose the period
year <- 2015

brd <- "ThreeME"
ghg <- 'CO2'
# Creation of an excel workbook
wb <- createWorkbook("ThreeME calibration")

  # Adding the exports in monetary values into the workbook with each sheet per type of ghg
  
  #for (ghg in c(glist, "GES")){
    


    #Extracting the data and taking only the imported emissions 
    data_EMS.M <- readRDS(str_c(path_loader,"M_",iso,"_",ghg,".rds"))  %>%
      select(-iso) %>% merge(sec.desc[[str_c(brd,".desc")]],., by = "products") %>%
      select(-nProducts)

    #Extracting the data and taking only the imported emissions for intermediate use
    data_EMS.CIM <- readRDS(str_c(path_loader,"M_",iso,"_",ghg,".rds"))  %>%
      select(-iso) %>% merge(sec.desc[[str_c(brd,".desc")]],., by = "products") %>%
      select(-nProducts)
    
    # Writing the data into a excel workbook sheet 
    addWorksheet(wb, str_c(ghg))
    writeData(wb, str_c(ghg), get(str_c("data_EMS.M")), rowNames = TRUE)
    addWorksheet(wb, str_c("CIM_",ghg))
    writeData(wb, str_c("CIM_",ghg), get(str_c("data_EMS.CIM")), rowNames = TRUE)
    
  #}
  # Adding the exports in monetary values
  data_M <- readRDS(str_c(path_loader,"Imports.Y_",iso,".rds"))  %>%
    select(-iso) %>% merge(sec.desc[[str_c(brd,".desc")]],., by = "products") %>%
    select(-nProducts)
  
  # Writing the data into a excel workbook sheet
  addWorksheet(wb, "IMPORTS")
  writeData(wb, "IMPORTS", data_M, rowNames = TRUE)
  

  # Exporting the results in the path_out folder
  saveWorkbook(wb, file = str_c(path_user,path_export,"DATA_GHG_IMPORTED.xlsx"), overwrite = TRUE)

  
  # Creation of an excel workbook
  wb <- createWorkbook("ThreeME calibration")
  
  # Adding the exports in monetary values into the workbook with each sheet per type of ghg
  
  for (ghg in c(glist, "GES")){
  
  #Extracting the data and taking only the imported emissions 
  
  #Extracting the data and taking only the imported emissions 
  data_EMS.CIM <- readRDS(str_c(path_loader,"S_",iso,"_",ghg,".rds"))  %>%
    select(-iso) %>% merge(sec.desc[[str_c(brd,".desc")]],., by = "products") %>%
    select(-nProducts)
  
  # Writing the data into a excel workbook sheet 
  addWorksheet(wb, str_c(ghg))
  writeData(wb, str_c(ghg), get(str_c("data_EMS.M")), rowNames = TRUE)
  addWorksheet(wb, str_c(ghg))
  writeData(wb, str_c(ghg), get(str_c("data_EMS.CIM")), rowNames = TRUE)
  
  
  }
  # Adding the exports in monetary values
  data_M <- readRDS(str_c(path_loader,"Imports.Y_",iso,".rds"))  %>%
    select(-iso) %>% merge(sec.desc[[str_c(brd,".desc")]],., by = "products") %>%
    select(-nProducts)
  
  # Writing the data into a excel workbook sheet
  addWorksheet(wb, "IMPORTS")
  writeData(wb, "IMPORTS", data_M, rowNames = TRUE)
  
  
  # Exporting the results in the path_out folder
  saveWorkbook(wb, file = str_c(path_user,path_export,"DATA_GHG_IMPORTED.xlsx"), overwrite = TRUE)
  
  
