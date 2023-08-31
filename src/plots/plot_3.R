library("WDI")
source("src/utils.R")
# Plot name
name_plot <- "Plot_3"
format_img <- "png"
police <- "Arial"
#_________________________________________
for (frmt in c(format_img,"pdf", "data")){
  dir.create(path = str_c(path_ResultsPlot,name_plot,"/",frmt),recursive = TRUE)
}
#_________________________________________
iso_list <- c("US", "FR", "DE","ES", "IT","PL", "CN", "ID", "IN","BR")
reg <- "EU2"

rep_data_tot <-rep_data.EI2 <- rep_data.ED2 <- list()
for (iso in iso_list){
  rep_data.EI <- rep_data.ED <- list()
  for (ghg in c("CO2" ,"CH4", "N2O")){
    
    rep_data.EI[[str_c(ghg)]] <- readRDS(str_c("data_out/IOT_",year,"_pxp/ThreeME_c28/",reg,"/M_",ghg,"_",iso,".rds"))  |>
      table.import()  |>
      merge(sec.desc[[str_c(br,".desc")]],., by = "products") |> select(-nProducts) |>
      arrange(match(codes,ThreeME_c28.desc$codes)) |>  mutate(GHG = ghg)
    
    
    rep_data.ED[[str_c("ED_",ghg)]] <- readRDS(str_c("data_out/IOT_",year,"_ixi/ThreeME_c28/",reg,"/ED.mat_",ghg,"_",iso,".rds")) |>
      summarise(across(all_of(ghg), ~ sum(.)))
  }

  
  #cbind(sec.desc[[str_c(br,".desc")]]$products,rep_data.EI[[str_c(ghg)]]$products)
  rep_data.EI[[str_c("CO2")]][-1:-3] |> select(-GHG) |> na.omit() |> sum()/10^9 
   rep_data.EI2[[str_c(iso)]] <- bind_rows(rep_data.EI, .id = "GHG")  |> 
    group_by(products, codes)  |> 
    # Somme pondérée par weight pour les produits (0>p>1)
    summarise(across(all_of(iso_list), ~ sum(.)), .groups = "drop_last")  |> ungroup() 
 
  rep_data.EI2[[str_c(iso)]] <- rep_data.EI2[[str_c(iso)]][-1:-2] |> na.omit() |> sum()
  
  rep_data.ED2[[str_c(iso)]] <-  bind_cols(rep_data.ED) |> mutate(GHG = rowSums(.))  |>
    select(GHG)
  
  # Messages
  cat(str_c("EMISSIONS DE GES INDIRECTES EN ",iso," de ", round(rep_data.EI2[[str_c(iso)]] |> na.omit() |> sum()/10^9,2), " MtCO2e\n"))
  
  cat(str_c("EMISSIONS DE GES DIRECTES EN ",iso," de ", round(rep_data.ED2[[str_c(iso)]][["GHG"]]/10^9,2)," MtCO2e\n"))
}


# Pop des pays de la list iso
data_pop <- WDI(country=iso_list, indicator="SP.POP.TOTL", start=2015, end=2020,
                extra=TRUE, cache=NULL) |> select("iso2c","year", "SP.POP.TOTL") |> `colnames<-`(c("iso", "years", "value"))  |> filter(years == year) |> select(-years) |> 
  arrange(match(iso, iso_list))


data.ED2 <-  unlist(rep_data.ED2) |> data.frame() |>
  rename(value_tot = ".") |>
  mutate( "countries" = str_sub(rownames(.),1,2), 
           "source" = "direct", 
           "value" = value_tot/(data_pop$value)) |> select(value, countries, source)

data.EI2 <- as.data.frame(unlist(rep_data.EI2) / (data_pop$value)) |>
  mutate("countries" = row.names(.), 
         "source" = "indirect") |>
  `colnames<-`(c("value", "countries", "source"))

data_plot <- rbind(data.EI2, data.ED2) 

desc.1 <- data.frame("labels" = c(countries_EU2.desc[["labels_fr"]]),
                     "codes"  = c(countries_EU2.desc[["codes"]])) |>
  filter(codes %in%  iso_list) 

desc.2 <- data.frame("labels" = c(countries_EU3.desc[["labels_fr"]]),
                     "codes"  = c(countries_EU3.desc[["codes"]])) |>
  filter(codes %in%  iso_list) 
desc <- rbind(desc.1, desc.2) |> unique()

### plot
plot <- ggplot(data_plot,
               aes(x= countries, 
                   y = round(value/10^3,3), fill = source)) + 
  
  geom_bar(stat='identity', position = position_stack())  + 
  scale_x_discrete(limits = desc$codes,
                   labels = desc$labels) +
  scale_fill_manual(values = c("#3e5e8d", "#8597b1"), 
                    breaks = c("direct","indirect" ), 
                    labels = c("Émissions directes", "Émissions indirectes")) +
  #geom_hline(aes(yintercept = 2 ),color = "darkred", linetype= 5, linewidth = 0.3) +
  ofce::theme_ofce(base_family = "Arial") +
  theme(
    plot.title = element_text(size= 10, family = police),
    plot.subtitle = element_text(size = 10, family = police),
    legend.text = element_text(size = 11, family = police),
    axis.title.x =element_blank(),
    axis.title.y = element_text(size = 10, family = police), 
    axis.text.x = element_text(size = 9 ,family = police),
    axis.text.y = element_text(size = 8, family = police, hjust=0),   
    panel.grid  = element_blank(), 
    legend.title = element_blank()
  )  +
  labs(
    #title= str_c("  Empreinte carbone par habitant en ",year," (en tCO2e/hab)"), 
    #subtitle =  str_c() ,
    #caption="Source: Exiobase 3.8.2, calculs OFCE\n Gaz à effet de serre inclus: CO2, CH4, N2O, Gaz fluorés",
    x= "",
    y=" en tCO2e/hab"
  )

for (frmt in format_img){
  ggsave(str_c(name_plot,".",frmt), plot  , device = str_c(frmt),
         path = str_c(path_ResultsPlot,name_plot,"/",frmt), width = 240 , height = 120 , units = "mm", dpi = 600) 
 }

write_csv(data_plot,file = "data_graphique1.csv")
