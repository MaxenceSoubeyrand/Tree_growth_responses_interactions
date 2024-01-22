#script to displayed the species conditions in the last RESEF inventory
rm(list=ls())

library(tidyverse)
library(ggplot2)
library(RODBC)

#growth
con <- odbcConnectAccess2007("C:/Users/max_d/OneDrive - UQAT/Documents/PhD/Data/RESEF/RESEF_30-novembre-2020_UQAT.accdb")
dendro <- sqlFetch(con, "t_Dendrometrie") 
site_car <- sqlFetch(con, "t_Azimuts_XY") %>% 
  select(`Site in RESEF dataset`=PLACE, Locality=LOCALITE, Longitude=LONGCALGPS, Latitude=LATCALGPS )

place_growth <- unique(readRDS(file="../../../model/growth.rds")$PLACE)

sp_corr <- data.frame(
  ESS = c("BOJ", "BOP", "EPN", "ERS", "ERR", "SAB"),
  Species = c("Yellow birch", "White birch", "Black spruce", "Sugar maple", "Red maple", "Balsam fir")
)

dendro2 <- dendro %>% 
  select(PLACE, ANNEE, ARBRE,DHP, ESS) %>% 
  filter(PLACE %in% place_growth) %>% 
  group_by(PLACE) %>%
  mutate(`First inventory year`= min(ANNEE),
         `Last inventory year`= max(ANNEE)) %>% 
  slice_max(order_by = ANNEE) %>% 
  mutate(Site = cur_group_id()) %>% 
  filter(ESS %in% c("BOJ", "BOP", "EPN", "ERS", "ERR", "SAB")) %>% 
  left_join(sp_corr) %>% 
  rename(`Site in RESEF dataset`=PLACE)

plot <- ggplot(dendro2, aes(x = DHP, color = Species, fill = Species)) +
  geom_histogram() +
  facet_wrap(~Site, scales = "free_y", ncol=3) +
  labs( x = "DBH (in mm)", y = "Frequence")+
  theme_bw()

pdf(file = "freq_DBH.pdf", width=6, height=8.5)
plot
dev.off()

png(filename = "freq_DBH.png", units="in", width=6, height=8.5, res=500)
plot
dev.off()


tab_SI <- dendro2 %>% 
  left_join(site_car) %>% 
  select(Site, `Site in RESEF dataset`, Locality, Longitude, Latitude,
         `First inventory year`, `Last inventory year`) %>% 
  unique() %>% 
  write.csv("tab_SI.csv")
  
  


