# Plot name
name_plot <- "Plot_2"
frmt <- "png"
police <- "Arial"
#_________________________________________
for (frmt in c(format_img,"pdf", "data")){
  dir.create(path = str_c(path_ResultsPlot,name_plot,"/",frmt),recursive = TRUE)
}
#_________________________________________
rep_data <- list()
for (ghg in c("CO2" ,"CH4", "N2O")){
  rep_data[[str_c(ghg)]] <- readRDS(str_c("data_out/IOT_2015_pxp/",sec_out,"/",br.pays,"/EMS.M.dir_",ghg,"_",iso,"_imp.rds")) |> mutate(GHG = ghg)
}

df <- readRDS(str_c("data_out/IOT_2015_pxp/",sec_out,"/",br.pays,"/EMS.M_CO2_",iso,"_imp.rds")) 
df%>% select(!starts_with(c("products", "codes", "GHG"))) %>% sum()/10^9


data <- bind_rows(rep_data, .id = "GHG")  |> 
  group_by(products, codes) %>%
  # Somme pondérée par weight pour les produits (0>p>1)
  summarise(across(all_of(other_countries), ~ sum(.)))  %>% ungroup() 

data[-1:-2] %>% sum()/10^9


### FEATURES
label <- data[1:2]
desc <-data.frame("labels" = c(countries_EU3.desc[["labels_fr"]]),
                  "codes"  = c(countries_EU3.desc[["codes"]])) |> filter(codes != iso)

desc.pr <-data.frame("labels.pr" = c(ThreeME_c28.desc[["products.fr"]]),
                     "codes"  = c(ThreeME_c28.desc[["codes"]])) 

pal <- c("#2a5572", "#98504a","#fb8072","#bc80bd","#bebada", "#fdb462","#b3de69")

data[-1:-2] %>% sum()/10^9
data_eu
### DATA
data_plot <- data |> merge(desc.pr, by = "codes") |>
  select(-codes, -products) |>
  pivot_longer(cols = colnames(data[-1:-2]), names_to = "countries", values_to = "value") %>%
  merge(desc, by.x = "countries", by.y = "codes") |> 
  arrange(match(labels,countries_EU3.desc[["labels_fr"]])) |> na.omit()

sum(data_plot$value)/10^9

### PLOT
rep_plot <- list() 
rep_plot[[str_c("plot.",iso,"_",br.pays,"_",sec_out)]] <- ggplot(data = data_plot ,
                                                                 aes(x = factor(labels.pr, levels = rev(c(ThreeME_c28.desc[["products.fr"]]))), 
                                                                     y = value/10^9,
                                                                     fill = countries)) +
  geom_hline(aes(yintercept = 150 ),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 125),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 100),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 75 ),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 50),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 25),color = "darkgray", linetype= 5, linewidth = 0.3) +
  geom_hline(aes(yintercept = 0),color = "black",  linewidth = 0.4) +
  geom_bar(stat="identity", width = 0.95, position = position_stack(reverse = TRUE)) + 
  
  scale_color_manual(values = pal , aesthetics = "fill",
                     limits = desc$codes,
                     labels = desc$labels) + 
  scale_y_continuous(breaks = seq(0, 150, by = 25)) + 
  
  #scale_fill_brewer(palette = "Reds") +
  theme_void() +
  theme(
    plot.title = element_text(size= 10, family = police),
    plot.subtitle = element_text(size = 10, family = police),
    legend.text = element_text(size = 11, family = police),
    axis.title.x = element_text(size = 10, family = police),
    axis.title.y = element_text(size = 10, family = police), 
    axis.text.x = element_text(size = 9 ,family = police),
    axis.text.y = element_text(size = 8, family = police, hjust=0),   
    panel.grid  = element_blank(), 
    legend.title = element_blank()
  ) +
  labs(
    title= str_c("           Émissions de GES importées par l'Union Européenne - données ",year), 
   #subtitle =  str_c() ,
    caption="Source: Exiobase 3.8.2, calculs OFCE\n Gaz à effet de serre inclus: CO2, CH4, N2O, Gaz fluorés",
    x= "",
    y=" en Mt CO2e"
  ) + 
  coord_flip()

rep_plot[[str_c("plot.",iso,"_",br.pays,"_",sec_out)]]
# EXPORT
for (frmt in format_img){
  ggsave(str_c(name_plot,".",iso,"_",br.pays,"_",sec_out,".",frmt), rep_plot[[str_c("plot.",iso,"_",br.pays,"_",sec_out)]]  , device = str_c(frmt),
         path = str_c(path_ResultsPlot,name_plot,"/",frmt), width = 360 , height = 120 , units = "mm", dpi = 600) 
}

