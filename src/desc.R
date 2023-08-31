

source("data_in/desc/exio3.desc.R")
source("data_in/desc/CPA4.desc.R")
source("data_in/desc/ThreeME_c28.desc.R")
source("data_in/desc/ThreeME_c29.desc.R")
source("data_in/desc/exio3_test.desc.R")
source("data_in/desc/countries.desc.R")

sec.desc <- list("exio3.desc" = exio3.desc,
                 "exio3_test.desc" = exio3_test.desc,
                 "ThreeME_c28.desc" = ThreeME_c28.desc,
                 "ThreeME_c29.desc" = ThreeME_c29.desc)


## Functions used in the script
source("src/functions/01_load.bridge.R")
source("src/functions/01_load.matrix.R")
source("src/functions/01_Leontief.inverse.R")
source("src/functions/01_GHG.extraction.R")
source("src/functions/01_shock.demand.R")
source("src/functions/02_perform.bridge.R")
source("src/functions/02_table.import.R")
source("src/functions/03_run.script.R")
