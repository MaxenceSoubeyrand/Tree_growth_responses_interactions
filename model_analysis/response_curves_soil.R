#Figure 5
rm(list=ls())

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
library(RODBC)
library(readxl)

res <- list.files("../model/res_mod_bayes", pattern="best_model_loo")


#Get the soil variables of PEP
sol_esp <- readRDS("soil_quebec.rds")


#We selected the soil interactions
graph_sel <- data.frame(species=c(rep("BOJ",3),rep("BOP",3),rep("EPN",3),rep("ERR",3),rep("ERS",3),rep("SAB",3)),
                        effects=c("CEC:NCI_conifer", "CEC:NCI_deciduous", "CEC:NCI_intra",
                                  "CEC:TAVE", "CEC:VPD", NA,
                                  "CEC:NCI_conifer", "pH:NCI_conifer", "clay:NCI_conifer",
                                  "CEC:DD5","CEC:PPT","clay:DD5",
                                  "CEC:NCI_intra", "clay:NCI_conifer", "clay:NCI_intra", 
                                  "CEC:NCI_intra",  "pH:NCI_intra", "clay:NCI_intra"))

plot_sp <- list()

for(i in res){ 
  #i=res[4]
  mod <- readRDS(file = paste0("../model/res_mod_bayes/",i))
  
  site_esp <- names(table(mod$data$PLACE))[table(mod$data$PLACE)>100]
  
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
  
  
  cond_ef_res <- NULL
  cond_ef_res_inter <- NULL
  eff_inter <- list()
  
  
  graph_sel_sp <- filter(graph_sel, species==sp) %>% na.omit()
  
  me <- conditional_effects(mod, re_formula=NA, ask=FALSE, plot=F, 
                            surface=F, effects = graph_sel_sp$effects) #surface=T pour plot 2d interaction, int_conditions=conditions
  
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
      
      cond_ef_res <- cond_ef_res %>% bind_rows(data.frame(species=sp, effect=j, value=sub_cond$effect1__, 
                                                          growth=exp(sub_cond$estimate__), lower = exp(sub_cond$lower__), upper = exp(sub_cond$upper__)))
      
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
      
      
      sol_hist <- sol_esp %>% filter(ESSENCE==sp) %>% dplyr::select(unique(cond_ef_res_inter$effect1))
      colnames(sol_hist)[1] <- "value1" 
      
      eff_inter[[j]] <- 
        ggplot() + 
        {if(unique(cond_ef_res_inter$effect1) %in% c("CEC", "clay")){
          geom_histogram(data=sol_hist, 
                         aes(x=value1,y = 6*..density..), color="grey70", fill="grey70")
        }}+
        {if(unique(cond_ef_res_inter$effect1) %in% c("pH")){
          geom_histogram(data=sol_hist, 
                         aes(x=value1,y = 1*..density..), color="grey70", fill="grey70")
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
        coord_cartesian(ylim = c(0, min(10, max(cond_ef_res_inter$upper))), xlim=c(NA, max(cond_ef_res_inter$value1)))
      
      
    }
  }
  
  
  esp <- data.frame(species=c("BOJ", "BOP", "EPN", "ERR", "ERS", "PET", "SAB"),
                    species2=c("Yellow birch", "White birch", "Black spruce",
                               "Red maple", "Sugar maple", "Trembling aspen", "Balsam fir")) %>% 
    filter(species==sp)
  
  plot <- annotate_figure(ggarrange(plotlist = eff_inter, nrow=1, common.legend = F, legend= "right"), left = text_grob("Growth (mm.y-1)", size = 11, rot = 90))
  plot <- annotate_figure(plot, left = text_grob(esp$species2, face = "bold", size = 15, rot = 90))
  
  plot_sp[[i]] <- plot
}

colnames(clim_resef_futur_mean)[2] <- "Climate scenario" 

library(ggplot2)
library(cowplot)

pdf(file = "~/PhD/chap2/article/figures/model_analysis/interaction_plot_soil_loo.pdf", width = 9, height = 12)
  ggarrange(plotlist = plot_sp, ncol=1)
dev.off()

png(filename = "~/PhD/chap2/article/figures/model_analysis/interaction_plot_soil_loo.png", width = 9, height = 12, units="in", res=500) 
  ggarrange(plotlist = plot_sp, ncol=1)
dev.off()
