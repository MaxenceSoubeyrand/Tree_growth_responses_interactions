#Script that give the growth of the species with the climate of the FERLD:
#-Open tree growth and soil data
#-Model tree growth with soil data
#-Compute distribution a posteriori of soil variables, it will be a priori distribution of the main model
rm(list=ls())

library(tidyverse)
library(quantreg)
library(brms)

#Open tree growth and soil data
soil_growth <- readRDS("soil_growth_data.rds")

#Normalize the soil variables
soil_growth_norm <- soil_growth  %>%
  mutate(lgrowth_rate=log(growth_rate),
         pH_eau=as.vector(scale(pH_eau_B)),
         CEC=as.vector(scale(CEC_B)),
         PC_ARGIL=as.vector(scale(PC_ARGIL))) %>% 
  select(species,lgrowth_rate, DHP, PC_ARGIL, pH_eau, CEC)

#Save the coefficient of normalization
soil_growth  %>%
  summarise(m_PC_ARGIL=mean(PC_ARGIL),
            m_pH_eau=mean(pH_eau_B),
            m_CEC=mean(CEC_B),
            sd_PC_ARGIL=sd(PC_ARGIL),
            sd_pH_eau=sd(pH_eau_B),
            sd_CEC=sd(CEC_B)) %>% 
  saveRDS(file="../model/coef_soil.rds")

#initialize table to receive the results of the model and the scaling coefficient for DBH variables.
summary_tab <- data.frame()

#Transform the DBH variable with DBH coefficient find in the climate growth data to be consistent
coef_ldbh <- readRDS(file="../model/coef_ldbh.rds")

for(i in unique(soil_growth_norm$species)){ #i="yellow_birch"
  soil_growth_norm_esp <- filter(soil_growth_norm, species==i) %>% 
  #Transform DBH variable
    mutate(ldbh=(log(DHP/10)-filter(coef_ldbh, species==i)$mean)/filter(coef_ldbh, species==i)$sd)
  
  # #Run all models
  # source("models.R")
  # 
  # #Model selection
  # mod_sel <- loo(brq_full, brq_1, brq_2, brq_3, brq_4, brq_5, brq_6, brq_7, brq_8,
  #                brq_9, brq_10, brq_11, brq_12, brq_13, brq_14,
  #                brq_15, brq_16, brq_17, brq_18, brq_19, brq_20, brq_21, brq_null)
  # 
  # best_model <- get(rownames(mod_sel[["diffs"]])[1])
  # #Save the model
  # saveRDS(best_model, paste0("res_mod_bayes/brq_res_", i,"_best_model.rds"))

  #Load the model
  brq <- readRDS(paste0("res_mod_bayes/brq_res_", i,"_best_model.rds"))
  
  #Extract the a posteriori distributions
  summary_tab_sp <- mutate(data.frame(fixef(brq)), species=i, effect=rownames(data.frame(fixef(brq))))
  summary_tab <- bind_rows(summary_tab, summary_tab_sp)
}

#save the a posteriori distributions that will be a priori distributions for the RESEF tree growth
saveRDS(summary_tab, "../model/distribution_apriori_soil.rds")
