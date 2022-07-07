library(openxlsx)

# folder in which the results are saved
path_user <- "C:/Users/PMalliet/Documents/GitHub/"
path_out <- "ThreeME_V3/data/France/"
# Choose of the year and the country (iso) for the data 
iso <- "FR"

# Choose the period
year <- 2015



# Creation of an excel workbook
wb <- createWorkbook("ThreeME calibration")

# Adding the exports in monetary values into the workbook with each sheet for a type of ghg
for (brd in c("ThreeME")){
  
  
  # Creation of an excel workbook
  wb <- createWorkbook(str_c(brd," calibration"))
  
  # Adding the exports in monetary values into the workbook with each sheet per type of ghg
  
  for (ghg in c(glist, "GES")){
    
    #Extracting the data and taking only the imported emissions 
    data_EMS.M_f1 <- readRDS(str_c("EXIOBASE/Results/data.",brd,"/",ghg,".f1_",iso,".",brd,"_ctr_sec_",year,".rds"))  %>%
      as.data.frame() %>%  filter(rownames(.) != iso) %>%
      t %>% as.data.frame %>% cbind("codes" = sec.desc[[str_c(brd,".desc")]][["codes"]], . )
    
    data_EMS.M_f2 <- readRDS(str_c("EXIOBASE/Results/data.",brd,"/",ghg,".f2_",iso,".",brd,"_ctr_sec_",year,".rds"))  %>%
      as.data.frame() %>%  filter(rownames(.) != iso) %>%
      t %>% as.data.frame %>% cbind("codes" = sec.desc[[str_c(brd,".desc")]][["codes"]], . )
    
    data_EMS.M_f3 <- readRDS(str_c("EXIOBASE/Results/data.",brd,"/",ghg,".f3_",iso,".",brd,"_ctr_sec_",year,".rds"))  %>%
      as.data.frame() %>%  filter(rownames(.) != iso) %>%
      t %>% as.data.frame %>% cbind("codes" = sec.desc[[str_c(brd,".desc")]][["codes"]], . )
    
    
    # Writing the data into a excel workbook sheet 
    for (i in 1:3){
      addWorksheet(wb, str_c(ghg,"_def",i))
      writeData(wb, str_c(ghg,"_def",i), get(str_c("data_EMS.M_f",i)), rowNames = TRUE)
    }
  }
  # Adding the exports in monetary values
  data_M <- readRDS(str_c("EXIOBASE/data/IOT_",year,"_",nom,"/Y.",iso,".",brd,"_ctr_sec.rds"))  %>%
    as.data.frame() %>% filter(rownames(.) != iso) %>%
    t %>% cbind("codes" = sec.desc[[str_c(brd,".desc")]][["codes"]], . )
  
  # Writing the data into a excel workbook sheet
  addWorksheet(wb, "IMPORTS")
  writeData(wb, "IMPORTS", data_M, rowNames = TRUE)
  
  
  # Exporting the results in the path_out folder
  saveWorkbook(wb, file = str_c(path_user,path_out,"DATA_GHG_IMPORTED.xlsx"), overwrite = TRUE)
}
