#Figure 4
rm(list=ls())

setwd("~/PhD/Chap2/Tree_growth_responses_interactions/model_analysis")
library(raster)
library(tidyverse)
theme_set(theme_bw())
library(brms)
library(shinystan)
library(rstan)
library(sjPlot)
library(sjmisc)
library(ggpubr)
library(ggplot2)
library(ggh4x)
library(qpdf)
library(posterior)
library(distributional)
library(tidybayes)
library(ggnewscale)

res <- list.files("../model/res_mod_bayes", pattern="best_model_loo")

#Find the futur climate according climate scenarios
clim_resef_futur <- read_csv("clim_resef_12 GCMsMSY.csv", show_col_types = FALSE)

clim_resef_futur <- clim_resef_futur %>% 
  separate(Year, sep="_",into=c("model", "model2", "scenario", "period")) %>% 
  dplyr::select(-model, -model2, ID2) %>% 
  mutate(period=str_remove_all(period, ".gcm")) %>%  
  mutate(TAVE=rowMeans(clim_resef_futur[c("Tave05", "Tave06",	"Tave07",	"Tave08")]), #Average temperature during season growth
         PPT=rowSums(clim_resef_futur[c("PPT05",	"PPT06",	"PPT07",	"PPT08")]), #Total precipitation during season growth
         RH=rowMeans(clim_resef_futur[c("RH05", "RH06",	"RH07",	"RH08")]), #Mean humidity during season growth to compute VPD
         DD5=rowSums(clim_resef_futur[c("DD5_05",	"DD5_06",	"DD5_07",	"DD5_08")])) %>% #DD5 sum during season growth
  mutate(es=0.6108*exp(17.27*TAVE/(TAVE+237.3)), 
         ea=RH/100*es,
         VPD=ea-es) %>% 
  dplyr::select(ID1, scenario, period, Latitude, Longitude, TAVE, PPT, VPD, DD5) %>% 
  filter(period=="2071-2100")

color_scenario <- c("red", "blue", "orange", "chartreuse3")



graph_sel <- data.frame(species=c(rep("EPN",2),rep("ERR",2),rep("ERS",2),rep("SAB",2),rep("BOP",2)),
                        effects=c("TAVE:NCI_intra", "VPD:NCI_intra",
                                  "DD5",  "PPT",
                                  "DD5:NCI_intra", "VPD:NCI_intra",
                                  "TAVE:NCI_intra",  "VPD:NCI_intra",
                                  "TAVE", "VPD"))

plot_sp <- list()



for(i in res){
  mod <- readRDS(file = paste0("../model/res_mod_bayes/",i))
  
  site_esp <- names(table(mod$data$PLACE))[table(mod$data$PLACE)>100]
  

  clim_resef_futur_mean <- clim_resef_futur %>% 
    filter(ID1 %in% site_esp) %>% 
    group_by(period, scenario) %>% 
    summarize(TAVE=mean(TAVE),
              PPT=mean(PPT),
              VPD=mean(VPD),
              DD5=mean(DD5)) %>% 
    pivot_longer(cols=c("TAVE", "PPT", "VPD", "DD5"), names_to=c("effect")) 
  
  sp <- str_split(i, pattern="_")[[1]][3]
  
  ###Get responses curves
  coef_sign <- data.frame(fixef(mod)) %>%
    mutate(species=sp, 
           effect=rownames(.)) 
  coef_sign$effect <- factor(coef_sign$effect, levels = factor(coef_sign$effect))
  
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IldbhE2", "ldbh")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "ITAVEE2", "TAVE")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IPPTE2", "PPT")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IDD5E2", "DD5")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IVPDE2", "VPD")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IpHE2", "pH")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "ICECE2", "CEC")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IclayE2", "clay")
  
  me <- conditional_effects(mod, re_formula=NA, ask=FALSE, plot=F, surface=F) #surface=T pour plot 2d interaction, int_conditions=conditions
  
  cond_ef_res <- NULL
  cond_ef_res_inter <- NULL
  eff_inter <- list()
  
  
  graph_sel_sp <- filter(graph_sel, species==sp) %>% na.omit()
  
  for(j in graph_sel_sp$effects){
    sub_cond <- get("me")[[j]] %>% 
      mutate(readRDS(file="../model/coef_clim.rds"),
             readRDS(file="../model/coef_soil.rds")) %>% 
      rename(sd_pH=sd_pH_eau,
             m_pH=m_pH_eau,
             sd_clay=sd_PC_ARGIL,
             m_clay=m_PC_ARGIL)
    
    if(!str_detect(j, ":")){ #Pas d'interactions
      
      if(j %in% c("VPD", "PPT", "DD5", "TAVE", "pH", "clay", "CEC")){
        sub_cond$effect1__ = (sub_cond$effect1__ * dplyr::select(sub_cond,all_of(paste0("sd_",j)))[,1])+dplyr::select(sub_cond,all_of(paste0("m_",j)))[,1]
      }
      
      cond_ef_res <- bind_rows(data.frame(species=sp, effect=j, value=sub_cond$effect1__, 
                                                          growth=exp(sub_cond$estimate__), lower = exp(sub_cond$lower__), upper = exp(sub_cond$upper__)))
      
      clim_resef_futur_mean_eff_simple <- filter(clim_resef_futur_mean, effect%in%j)
      
      eff_inter[[j]] <- ggplot(cond_ef_res, aes(x=value, y=growth, ymin = lower, ymax = upper)) +
        geom_ribbon(alpha=0.5) +
        geom_line() + ylab(NULL) +
        theme(strip.background = element_blank(),
              strip.placement = "outside") + xlab(j) +
        theme(axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=9)) +
        geom_vline(data=clim_resef_futur_mean_eff_simple, aes(xintercept=value, color=scenario), show.legend=F)+
        scale_color_manual(values=color_scenario) +
        coord_cartesian(ylim = c(0, 6)) +
        coord_cartesian(ylim = c(0, min(10, max(cond_ef_res$upper))), xlim=c(NA, max(cond_ef_res$value)))

      
    }else{#Si interactions
      var1 <- str_split(j, ":")[[1]][1]
      var2 <- str_split(j, ":")[[1]][2]
      
      if(var1 %in% c("VPD", "PPT", "DD5", "TAVE", "pH", "clay", "CEC")){
        sub_cond$effect1__ <- (sub_cond$effect1__ * dplyr::select(sub_cond,all_of(paste0("sd_",var1)))[,1])+dplyr::select(sub_cond,all_of(paste0("m_",var1)))[,1]
      }
      if(var2 %in% c("VPD", "PPT", "DD5", "TAVE", "pH", "clay", "CEC")){
        sub_cond$effect2__ <- as.factor(round((as.numeric(as.character(sub_cond$effect2__ ))* dplyr::select(sub_cond,all_of(paste0("sd_",var2)))[,1])+dplyr::select(sub_cond,all_of(paste0("m_",var2)))[,1],2))
      }
      
      cond_ef_res_inter <- data.frame(species=sp, effect=j, 
                                      effect1= str_split(j, ":")[[1]][1], effect2=str_replace_all(str_split(j, ":")[[1]][2], "_", "\n"),
                                      value1=sub_cond$effect1__,
                                      value2=sub_cond$effect2__,
                                      growth=exp(sub_cond$estimate__), lower = exp(sub_cond$lower__), upper = exp(sub_cond$upper__))
      
      colnames(cond_ef_res_inter) <- str_replace_all(colnames(cond_ef_res_inter), "_", "\n")
      

      
      eff_inter[[j]] <- 
        ggplot() + 
        {if(unique(cond_ef_res_inter$effect1) %in% c("pH" )){
          geom_histogram(data=get(unique(cond_ef_res_inter$effect1)), aes(x=value1,y = ..density..), color="grey70", fill="grey70")
        }}+
        {if(unique(cond_ef_res_inter$effect1) %in% c("CEC", "clay")){
          geom_histogram(data=get(unique(cond_ef_res_inter$effect1)), aes(x=value1,y = 6*..density..), color="grey70", fill="grey70")
        }}+
        geom_line(data=cond_ef_res_inter, aes(x=value1, y=growth, color=value2, group=value2, fill=value2, ymin = lower, ymax = upper),linewidth=1)  +
        geom_ribbon(data=cond_ef_res_inter, aes(x=value1, y=growth, color=value2, group=value2, fill=value2, ymin = lower, ymax = upper), alpha=0.3, colour=NA)+
        labs(color = unique(cond_ef_res_inter$effect2), 
             fill = unique(cond_ef_res_inter$effect2)) +
        ylab(NULL) +
        xlab(unique(cond_ef_res_inter$effect1)) +
        theme(legend.position = "right") + 
        theme(legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              legend.key.size = unit(0.35, 'cm'), 
              axis.text.y=element_text(size=10),
              axis.text.x=element_text(size=9),
              axis.title=element_text(size=10)) +
        new_scale_color() +
        geom_vline(data=filter(clim_resef_futur_mean, effect==var1), aes(xintercept=value, color=scenario), show.legend=F) +
        scale_color_manual(values=color_scenario) + 
        coord_cartesian(ylim = c(0, min(10, max(cond_ef_res_inter$upper))),
                        xlim=c(min(cond_ef_res_inter$value1), max(cond_ef_res_inter$value1)))
      
      
    }
  }

  esp <- data.frame(species=c("BOJ", "BOP", "EPN", "ERR", "ERS", "PET", "SAB"),
                    species2=c("Yellow birch", "White birch", "Black spruce",
                               "Red maple", "Sugar maple", "Trembling aspen", "Balsam fir")) %>% 
    filter(species==sp)
  
  plot <- annotate_figure(ggarrange(plotlist = eff_inter, nrow=1, common.legend = T, legend= "right"), left = text_grob("Growth (mm.y-1)", size = 11, rot = 90))
  plot <- annotate_figure(plot, left = text_grob(esp$species2, face = "bold", size = 15, rot = 90))
  
  plot_sp[[i]] <- plot
}

colnames(clim_resef_futur_mean)[2] <- "Climate scenario" 

plot_legend <- ggplot() + 
geom_vline(data=clim_resef_futur_mean, aes(xintercept=value, color=`Climate scenario`)) + 
  scale_color_manual(values=color_scenario) +
  theme(legend.position="bottom")

library(ggplot2)
library(cowplot)

legend <- cowplot::plot_grid(NULL, cowplot::get_legend(plot_legend), ncol=1)

all_plot<- ggarrange(plotlist = plot_sp, ncol=1)

pdf(file = "interaction_plot_loo.pdf", width = 9, height = 12)
ggarrange(all_plot, legend, ncol=1, heights = c(0.95,0.05))
dev.off()

png(filename = "interaction_plot_loo.png", width = 9, height = 12, units="in", res=500) 
  ggarrange(ggarrange(plotlist = plot_sp, ncol=1), legend, ncol=1, heights = c(0.95,0.05))
dev.off()
