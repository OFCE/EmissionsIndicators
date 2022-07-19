format <- "png"

#Charger le tableau dans l'environnement
for (iso in c("FR", "DE", "IT", "PL", "SP", "GB")) {
IO <- readRDS(str_c(path_ResultsTable, "/Summary_EMS.",ghg,"_", iso, ".rds"))
assign(str_c("IO_",iso),IO)

#Créer un graphique avec les trois indicateurs
data_plot <- IO %>% 
  #par produits
  group_by(produits) %>%
  #filter(produits != "SERVICES EXTRA-TERRITORIAUX") %>% #toujours=0
  mutate(agg.demande_impact=sum(GES_M),
         agg.producteur_impact=sum(GES_S),
         agg.production=sum(production_pays),
         agg.demande_finale=sum(DF_tot)) %>%
  ungroup() %>%
  #format long pour afficher les deux indicateurs
  pivot_longer(
    cols = c("agg.producteur_impact","agg.demande_impact"),
    names_to = "indicator",
    values_to = "impact") 


 plot <-  ggplot(data_plot,
    aes(x= produits, 
        y = impact/ 10^9,
        fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production"),
                    values = c("indianred1", "cornflowerblue"))
 
 
ggsave(filename = str_c("plot.secteurs_", iso, ".",format), 
       plot=plot, 
       path=path_ResultsPlot,
       width = 280 , height = 200 , units = "mm", dpi = 600)


plot <-  ggplot(data_plot,
                aes(x= regions, 
                    y = impact/ 10^9,
                    fill = indicator)) +
  geom_bar(stat='identity',position = "dodge") +
  theme(axis.text.x = element_text(angle = 25, size=4, vjust = 1, hjust=1),
        plot.title =element_text(size=12, face='bold', hjust=0.5),
        panel.background = element_blank(),
        panel.grid.major.y=element_line(color="gray",size=0.5,linetype = 2),
        plot.margin = unit(c(10,5,5,5), "mm"))+
  labs(title="Impacts",
       x ="Secteurs", y = "Impact GES (CO2eq)",
       fill="Indicateur") +
  scale_fill_manual(labels = c("Demande", "Production"),
                    values = c("indianred1", "cornflowerblue"))


ggsave(filename = str_c("plot.regions_", iso, ".",format), 
       plot=plot, 
       path=path_ResultsPlot,
       width = 280 , height = 200 , units = "mm", dpi = 600)
}