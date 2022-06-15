#associé IOanalysis brouillon
# Paths
source("src/results/plots/Plots_PHD/Plot_PHD_main.R")
name_plot <- "Plot_CHIII.2"

for (frmt in c(format_img,"pdf", "data")){
  dir.create(path = str_c(path_res,name_plot,"/",frmt),recursive = TRUE)
}




rep_plot <- list()

for (ela in c("dElas", "dNElas", "Elasd", "dElas.1")){
  
  df.1 <- read.csv(str_c("Results/data/simulations/Dc.choc_TC.CO2.EI.EXP_COICOP1_",ela,".csv")) %>%
    cbind("Tax" = "Carbon tax on \n indirect emissions",.) %>%
    select(Tax, Choc_tot, Cov.rate, Decile_UC)
  df.2 <- read.csv(str_c("Results/data/simulations/Dc.choc_TC.CO2.ED.EXP_COICOP1_",ela,".csv")) %>%
    cbind("Tax" = "Carbon tax on \n direct emissions",.) %>%
    select(Tax, Choc_tot, Cov.rate, Decile_UC) 
  
  df.3 <- read.csv(str_c("Results/data/simulations/Dc.choc_TC.CO2.ET.EXP_COICOP1_",ela,".csv")) %>%
    cbind("Tax" = "Carbon tax on \n total emissions",.) %>%
    select(Tax, Choc_tot, Cov.rate, Decile_UC) 
  
  df.1$Decile_UC <- factor(dc$Decile, levels = levels(dc$Decile))
  df.2$Decile_UC <- factor(dc$Decile, levels = levels(dc$Decile))
  df.3$Decile_UC <- factor(dc$Decile, levels = levels(dc$Decile))
  
  ##A PARTIR D ICI (bind tous les tableaux ?)
  data_plot <- rbind(df.1,df.2, df.3) 
  
  data_plot.1 <- data_plot %>% select(-Choc_tot) 
  
  #gather(key = Decile, value = value, - Cov.rate, -Tax, -Decile_UC)
  data_plot.2 <- data_plot %>% select(-Cov.rate) #%>%gather(key = Decile, value = value, - Choc_tot, -Tax, -Decile_UC)
  
  rep_plot[[str_c("plot.1_",ela)]] <- ggplot(data = data_plot.1 , aes(x= Decile_UC, y = Cov.rate * 100, fill = Tax)) +
    geom_hline(aes(yintercept = 3),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 2),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    #geom_hline(aes(yintercept = 1.5),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 1),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    #geom_hline(aes(yintercept = 0.5),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0),color = "#8b8b8b", linetype= "solid", size = 0.4) +
    geom_bar(stat="identity", width = 0.85, position = "dodge") +
    scale_color_manual(values = rep_pal[[str_c("cb_palette_p8")]], aesthetics = "fill") +
    ggtitle("In % of income") +
    
    #xlim(0,120) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, family =police, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), 
      axis.text.x = element_text( size =10, family =police),
      axis.text.y = element_blank(),
      legend.title = element_blank(),
      legend.position = "none",
      plot.background=element_blank(), 
      panel.grid.minor=element_blank(),
      panel.grid.major.y=element_blank(),
      panel.grid.major.x=element_blank()
    ) +
    scale_y_reverse() +
    coord_flip()
  # 
  # theme_PHD() <- theme_minimal() +
  # theme(
  # plot.title = element_text(size = 12, family =police, hjust = 0.5),
  # axis.title.x = element_blank(),
  # axis.title.y = element_blank(), 
  # axis.text.x = element_text( size =10, family =police),
  # axis.text.y = element_blank(),
  # legend.title = element_blank(),
  # legend.position = "none",
  # plot.background=element_blank(), 
  # panel.grid.minor=element_blank(),
  # panel.grid.major.y=element_blank(),
  # panel.grid.major.x=element_blank()
  # ) +
  # scale_y_reverse() +
  # coord_flip()
  
  rep_plot[[str_c("plot.2_",ela)]] <- ggplot(data = data_plot.2 , aes(x= Decile_UC, y = Choc_tot, fill = Tax)) + 
    #geom_hline(aes(yintercept = 100),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 200),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 400),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 600),color = "#8b8b8b", linetype= "dashed", size = 0.3) +
    geom_hline(aes(yintercept = 0),color = "#8b8b8b", linetype= "solid", size = 0.4) +
    geom_bar(stat="identity", width = 0.85, position = "dodge") +
    scale_color_manual(values = rep_pal[[str_c("cb_palette_p8")]], aesthetics = "fill") +
    ggtitle("In EUR per household") +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, family =police, hjust = 0.5),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(), 
      axis.text.x = element_text( size = 10, family =police),
      axis.text.y = element_blank(),
      legend.title = element_blank(),
      legend.text = element_text(size = 10, family =police),
      legend.position="right",
      plot.background=element_blank(), 
      panel.grid.minor=element_blank(),
      panel.grid.major.y=element_blank(),
      panel.grid.major.x=element_blank()
    ) +
    coord_flip()
  
  
  plot.mid <- ggplot(df.1 ,aes(x = 1, y = Decile_UC)) +
    geom_text(aes(label = Decile), size = 3, family = police) +
    ggtitle("") +
    ylab(NULL) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      panel.background = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_line(color=NA),
      plot.margin = unit(c(0,-10,5,-10), "mm")
    )
  
  
  #rep_plot[[str_c("plot_",ela)]] <- grid.arrange(gg1, gg.mid, gg2,ncol=3,widths=c(3/10,1/10,6/10))
  rep_plot[[str_c("plot_",ela)]] <- ggarrange( rep_plot[[str_c("plot.1_",ela)]], plot.mid, rep_plot[[str_c("plot.2_",ela)]],
                                               ncol=3, widths=c(4.5/10,1/10,4.5/10),
                                               nrow=1, common.legend = TRUE, legend = "bottom")
  
  
  #### EXPORTS
  write.table(data_plot, str_c(path_res.plot,name_plot,"/data/data_",name_plot,"_",ela,".csv"), sep = ",", row.names = F)
  
  for (frmt in format_img){
    ggsave(str_c(name_plot,"_",ela,".",frmt), rep_plot[[str_c("plot_",ela)]], device = str_c(frmt), path = str_c(path_res.plot,name_plot,"/",frmt), width = 280 , height = 200 , units = "mm", dpi = 600)  
  }
  ggsave(str_c(name_plot,"_",ela,".pdf"), rep_plot[[str_c("plot_",ela)]], device = cairo_pdf, path = str_c(path_res.plot,name_plot,"/pdf"), width = 280 , height = 200 , units = "mm", dpi = 600)
}

rep_plot[[str_c("plot")]] <- ggarrange(plot.mid,
                                       rep_plot[[str_c("plot.1_dNElas")]] ,rep_plot[[str_c("plot.2_dNElas")]] ,
                                       #rep_plot[[str_c("plot.1_dElas")]] ,rep_plot[[str_c("plot.2_dElas")]] ,
                                       #rep_plot[[str_c("plot.1_Elasd")]] ,rep_plot[[str_c("plot.2_Elasd")]] ,
                                       ncol = 3, widths = c(1/10, 4.5/10, 4.5/10),
                                       nrow=1, common.legend = TRUE, legend = "right")
for (frmt in format_img){
  ggsave(str_c(name_plot,".",frmt), rep_plot[[str_c("plot.dNElas")]], device = str_c(frmt), path = str_c(path_res.plot,name_plot,"/",frmt), width = 280 , height = 140 , units = "mm", dpi = 600)  
}
ggsave(str_c(name_plot,".pdf"), rep_plot[[str_c("plot.dNElas")]], device = cairo_pdf, path = str_c(path_res.plot,name_plot,"/pdf"), width = 280 , height = 140 , units = "mm", dpi = 600)

##IO
View(IO_France)
IO_France %>% 
  pivot_longer(cols = c("GES_impact_producteur","GES_impact_demande"), 
                                  names_to = "indicator",
                                  values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot(aes(x= produits,
             y = impact,
             fill=indicator)) +
  geom_bar(stat='identity')

#graph impact niveau mondial par secteurs
IO_agg.secteur %>% pivot_longer(cols = c("agg.producteur_impact","agg.demande_impact"), 
                           names_to = "indicator",
                           values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot(aes(x= categorie.produit,
             y = impact,
             fill=indicator)) +
  geom_bar(stat='identity', position = "dodge")


#graph production et demande tous secteurs par pays
IO_all_agg.pays %>% 
  pivot_longer(
    cols = c("agg.production","agg.demande_finale"),
    names_to = "econ_multiplier",
    values_to = "variable") %>%
  as.data.frame() %>%
  ggplot(aes(x = nom_pays, 
             y = variable,
             fill = econ_multiplier)) +
  geom_bar(stat = "identity",position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
#graph impact tous secteurs par pays
IO_all_agg.pays %>% pivot_longer(
  cols = c("agg.producteur_impact","agg.demande_impact"),
  names_to = "indicator",
  values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= nom_pays, 
        y = impact,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#graph niveau mondial par produit
IO_agg.produits %>% pivot_longer(
  cols = c("agg.producteur_impact","agg.demande_impact"),
  names_to = "indicator",
  values_to = "impact") %>%
  as.data.frame() %>% 
  ggplot( 
    aes(x= categorie.produit, 
        y = impact,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") 

#idées: facet par secteur?