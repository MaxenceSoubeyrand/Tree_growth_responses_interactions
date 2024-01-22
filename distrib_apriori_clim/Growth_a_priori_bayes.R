#Script that give the growth of the species with the climate of the FERLD:
#-Open tree growth and climate data
#-Model tree growth with climate data
#-Compute distribution a posteriori of climate variables, it will be a priori distribution of the main model

rm(list=ls())

library(tidyverse)
library(quantreg)
library(brms)

#read tree growth and climate data
clim_growth <- readRDS("climate_growth_data.rds") 


#Centered reduce climatic variables, dbh is species-centered reduced.
clim_growth_norm <- clim_growth  %>%
  mutate(TAVE=scale(TAVE),
         PPT=scale(PPT),
         VPD=scale(VPD),
         DD5=scale(DD5)) 

#Save the coefficient to rescale for the figure
clim_growth %>%
  summarise(m_TAVE=mean(TAVE),
         m_PPT=mean(PPT),
         m_VPD=mean(VPD),
         m_DD5=mean(DD5),
         sd_TAVE=sd(TAVE),
         sd_PPT=sd(PPT),
         sd_VPD=sd(VPD),
         sd_DD5=sd(DD5)) %>% 
  saveRDS(file="../model/coef_clim.rds")


#initialize table to receive the results of the model and the scaling coefficient for DBH variables.
summary_tab <- data.frame()

ldbh_scale <- data.frame()

for(i in unique(clim_growth_norm$species)){ #i="Yellow_birch"
  #filter with the species i
  clim_growth_norm_spe <- filter(clim_growth_norm, species==i) %>% 
    mutate(lDBHI=log(DBHI))
  
  #get scale coefficient of DBH and scaling the DBH variable
  m <- mean(log(clim_growth_norm_spe$DBH))
  s <- sd(log(clim_growth_norm_spe$DBH))
  clim_growth_norm_spe$ldbh <- (log(clim_growth_norm_spe$DBH) - m) / s
  
  #Store coefficient
  ldbh_scale <- bind_rows(ldbh_scale, data.frame(mean=m, sd=s, species=i))
  
  # #Bayesian quantile regression model
  # #Run the 25 models
  # source("models.R")
  # 
  # #Model selection using Leave One Out process
  # mod_sel <- loo(brq_null,
  #                #TAVE et PPT
  #                brq1, brq2, brq3, brq4, brq5, brq6, brq7, brq8,
  #                #TAVE et VPD
  #                brq9, brq10, brq11, brq12, brq13, brq14,
  #                #DD5 et PPT
  #                brq15, brq16, brq17, brq18, brq19, brq20,
  #                #DD5 et VPD
  #                brq21, brq22, brq23, brq24)
  # 
  # #Save the best model
  # best_model <- get(rownames(mod_sel[["diffs"]])[1])
  # saveRDS(best_model, paste0("res_mod_bayes/brq_res_", i,"_best_model.rds"))
  
  #Load the model
  brq <- readRDS(paste0("res_mod_bayes/brq_res_", i,"_best_model.rds"))
  
  #Extract the a posteriori distributions
  summary_tab_sp <- mutate(data.frame(fixef(brq)), species=i, effect=rownames(data.frame(fixef(brq))))
  summary_tab <- bind_rows(summary_tab, summary_tab_sp)
}

#Save dbh coefficient
saveRDS(ldbh_scale, "../model/coef_ldbh.rds")

#save the a posteriori distributions that will be a priori distributions for the RESEF tree growth
saveRDS(summary_tab, "../model/distribution_apriori_clim.rds")