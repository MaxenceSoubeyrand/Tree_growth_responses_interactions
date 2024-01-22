rm(list=ls())

library(tidyverse)
library(sf)
library(maps)
library(ggspatial)
library(ggnewscale)


data <- readRDS("data_map.rds")

#Bioclimatic domain
dom_bio_map<- readRDS("dom_bio_map.rds")

canada <- sf::st_as_sf(map('world', regions="canada", plot = FALSE, fill = TRUE))
world <- sf::st_as_sf(map('world', plot = FALSE, fill = TRUE))

lim_bio <- dom_bio_map@bbox

dom_bio_map <- st_as_sf(dom_bio_map) 

dom_bio_map  = st_sf(
  aggregate(
    dom_bio_map,
    by=list(DOM_BIO=dom_bio_map$DOM_BIO),
    FUN=function(vals){vals[1]}))

color_blind <- c('black','#EE7733', '#0077BB', "#009E73", "#F0E442", '#33BBEE', '#EE3377', '#CC3311', '#009988', '#AA4499')


main <- ggplot()+ geom_sf(data = canada) +
  geom_sf(data=dom_bio_map, aes(fill=DOM_BIO.1), alpha=0.6) +
  scale_fill_manual(values=c(color_blind[5], color_blind[4], color_blind[10], color_blind[2],color_blind[3], color_blind[6], color_blind[7]),
                    breaks = c("2", "3","4", "5", "6","7"),
                    labels = c("Basswood -\nsugar maple", "Yellow birch -\nsugar maple ","Balsam fir -\nyellow birch", "Balsam fir -\nwhite birch", "Spruce -\nmoss", "7")) +
  labs(fill="Bioclimatic domain") +
  new_scale_color() + 
  geom_point(data=data, aes(x=LONG, y=LAT, color=` `), size=7)+
  geom_text(data=data, aes(x=LONG, y=LAT, label=Site), size=5, color="white")+
  scale_color_manual(values="black") +
  coord_sf(xlim = c(lim_bio[1,1]+0.45, lim_bio[1,2]+lim_bio[1,2]/7), ylim = c(lim_bio[2,1], lim_bio[2,2]-0.4), expand=T) +
  theme_bw() +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin()) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.05, "in"), pad_y = unit(0.15, "in"),
                         style = north_arrow_fancy_orienteering) +
  annotation_scale(location = "bl", width_hint = 0.35, height = unit(0.1, "cm")) +
  xlab("Latitude") +
  ylab("Longitude")+
  theme(legend.title.align = 0.5,
        panel.background = element_rect(fill = "dodgerblue3"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 16)
  ) + guides(fill=guide_legend(nrow=2,byrow=TRUE))

countries <- data.frame(lat=c(51.66781288717857, 38.99658150626441),
                        long=c(-90.42096831020812, -88.50786675658247),
                        country=c("Canada", "USA"))


emprise <- ggplot(data=world) +
  geom_sf() +
  geom_text(data=countries, aes(x=long, y=lat, label=country), size=2.5)+
  coord_sf(xlim = c(-99, -51), ylim = c(30, 62), expand = FALSE) +
  theme_void() +
  annotate("rect", xmin = lim_bio[1,1]+0.45, xmax = lim_bio[1,2]+lim_bio[1,2]/7, ymin = lim_bio[2,1], ymax = lim_bio[2,2]-0.4,
           alpha = .6, fill = "grey50", color="black",linewidth=0.5) +
  theme(
    panel.background = element_rect(fill = "dodgerblue3"),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black",
                                fill = NA,
                                linewidth = 1))

library(cowplot)

map <-
  ggdraw() +
  draw_plot(main) +
  draw_plot(emprise, x=0.83, y=0.8, width=.15, height=.15)


pdf(file="map_ggplot.pdf")
map
dev.off()

png(filename="map_ggplot.png", unit="in", height=7, width=7, res=700)
map
dev.off()
