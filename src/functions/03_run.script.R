run.script <- function(nom,
                      year, 
                      br, 
                      iso, 
                      PreLoader = NULL,
                      Bridging = NULL,
                      ThreeME_export = NULL){
  
  if (is.null(PreLoader)){PreLoader = FALSE}
  if (is.null(Bridging)){Bridging = FALSE}
  if (is.null(ThreeME_export)){ThreeME_export = FALSE}
  
  
  if (PreLoader == TRUE){source("src/01_exio3.Pre_loader.R")}
  if (Bridging == TRUE){source("src/02_exio3.loader.R")}
  
  source("src/03_IO.analysis.R")
  
  if (ThreeME_export == TRUE){source("src/04_export.ThreeME.R")}
}