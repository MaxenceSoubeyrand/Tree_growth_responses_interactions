#Figure 2
rm(list=ls())

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

res <- list.files("../model/res_mod_bayes", pattern="sel_loo")

sel <- NULL

#For each species
for(i in res){ #i=res[2]
  sp <- str_remove(str_split(i, pattern="_")[[1]][4], ".rds")
  
  sel_sp <- readRDS(paste0("../model/res_mod_bayes/",i))$ic_diffs__ %>% 
    as.data.frame() %>% 
    mutate(model_diff=row.names(.))
  
  rownames(sel_sp) <- NULL
  
  #Get the differences between full model and climate, soil, NCI and shading
  #More the LOO IC is high more the block is important.
  mod <- data.frame(diff_mod=c("Full-climate", "Full-soil", "Full-NCI", "Full-shading"),
                    model_diff=c("res_mod[[8]] - res_mod[[34]]", "res_mod[[14]] - res_mod[[34]]",
                                 "res_mod[[18]] - res_mod[[34]]", "res_mod[[26]] - res_mod[[34]]")) 
  
  sel_sp <- sel_sp %>% 
    right_join(mod, by="model_diff") %>% 
    mutate(species=sp)
  
  sel <- bind_rows(sel, sel_sp)
}

sel$diff_mod <- factor(sel$diff_mod, levels = c("Full-climate", "Full-soil" ,
                                                "Full-NCI",   "Full-shading"))

esp <- data.frame(species=c("BOJ", "BOP", "EPN", "ERR", "ERS", "SAB"),
                  species2=c("Yellow birch", "White birch", "Black spruce",
                             "Red maple", "Sugar maple",  "Balsam fir"))

sel <- sel %>% 
  right_join(esp)

pdf(file = "model_selection_loo.pdf", width = 10, height = 8)
ggplot(sel, aes(x=diff_mod, y=LOOIC, ymin=LOOIC-SE, ymax=LOOIC+SE)) + 
     geom_errorbar() + geom_point() +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  facet_wrap2(~species2, axes="y", scales = "free_y") +
  xlab("Models") + ylab("LOO Information Criterion") + 
  theme(axis.text = element_text(size = 11.5),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12))
dev.off()

png(filename = "model_selection_loo.png", width=10, height=8, units="in", res=500) 
ggplot(sel, aes(x=diff_mod, y=LOOIC, ymin=LOOIC-SE, ymax=LOOIC+SE)) + 
  geom_errorbar() + geom_point() +
  scale_x_discrete(guide = guide_axis(n.dodge=2))+
  facet_wrap2(~species2, axes="y", scales = "free_y") +
  xlab("Models") + ylab("LOO Information Criterion") + 
  theme(axis.text = element_text(size = 11.5),
        axis.title = element_text(size = 17),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        strip.text.x = element_text(size = 12))
dev.off()