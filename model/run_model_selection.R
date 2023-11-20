#Script that model tree growth as a relation to DHP, soils, climate, competition and interactions, for each species.
rm(list=ls())

setwd("~/PhD/Chap2/Tree_growth_responses_interactions/model")
library(tidyverse)
library(brms)

#Load functions that buil formulas
source("function_built_models_priors.R")

growth <- readRDS(file="growth.rds")


saveRDS(growth, file="growth.rds") 



coef_ldbh <- readRDS(file="coef_ldbh.rds")
coef_clim <- readRDS(file="coef_clim.rds")
coef_sol <- readRDS(file="coef_soil.rds")

#Nomrmalized the climate and soil variables to be relevant with climate and soils models 
growth <- growth %>%
  mutate(lgrowth_rate=log(growth_rate)) %>%
  left_join(coef_ldbh) %>%
  mutate(coef_clim) %>%
  mutate(coef_sol) %>%
  mutate(ldbh=(log(DHP/10) - mean) / sd) %>%
  dplyr::select(-mean, -sd) %>%
  mutate(TAVE=(TAVE-m_TAVE)/sd_TAVE,
         PPT=(PPT-m_PPT)/sd_PPT,
         VPD=(VPD-m_VPD)/sd_VPD,
         DD5=(DD5-m_DD5)/sd_DD5,
         clay=(argile-m_PC_ARGIL)/sd_PC_ARGIL,
         pH=(pH_eau-m_pH_eau)/sd_pH_eau,
         CEC=(CEC-m_CEC)/sd_CEC) %>%
  dplyr::select(-m_TAVE, -m_PPT, -m_VPD, -m_DD5, -sd_TAVE, -sd_PPT, -sd_VPD, -sd_DD5,
         -m_PC_ARGIL, -m_pH_eau, -m_CEC, -sd_PC_ARGIL, -sd_pH_eau, -sd_CEC, -pH_eau, -argile)

#get prior distribution for climatic and soil data.
apriori_clim <- readRDS("distribution_apriori_clim.rds") %>% 
  dplyr::select(species, Estimate, sd=Est.Error, effect) %>% 
  separate(effect, into=c("effect", "variable"))

#a priori dbh reduced centered normal be cause we have enough data on DBH to have a good estimation
apriori_clim[apriori_clim$variable=="IldbhE2" | apriori_clim$variable=="ldbh", "Estimate"] <- 0
apriori_clim[apriori_clim$variable=="IldbhE2" | apriori_clim$variable=="ldbh", "sd"] <- 1

apriori_soil <- readRDS("distribution_apriori_soil.rds") %>% 
  mutate(effect=str_replace_all(effect, "PC_ARGIL", "clay"),
         effect=str_replace_all(effect, "pH_eau", "pH")) %>% 
  dplyr::select(species, Estimate, sd=Est.Error, effect) %>% 
  separate(effect, into=c("effect", "variable"))

rownames(apriori_soil) <- NULL

#For each species
for(sp in unique(growth$species)){ #sp="Balsam_fir"
  
  
  print(sp)
  
  #Filtering the growth data with species
  growth_sp <- filter(growth, species==sp)
  
  #get prior distribution for climatic and soil data.
  apriori_clim_sp <- apriori_clim %>% 
    filter(species==sp,
           variable != "Intercept") 

  
  apriori_sol_sp <- apriori_soil %>% 
    filter(species==sp,
           variable != "Intercept",
           str_detect(variable, "ldbh", negate=T)) 

  #No a priori on competition, shading, interactions and  random effects
  apriori_NCI_sp <- data.frame(ESS=sp, Estimate=0, effect="lin", variable=c("NCI_conifer", "NCI_intra", "NCI_deciduous"), sd=2*sd(growth_sp$lgrowth_rate))
  apriori_shade_sp <- data.frame(ESS=sp, Estimate=0, effect="lin", variable="Shading", sd=2*sd(growth_sp$lgrowth_rate))
  apriori_inter_sp <- data.frame(ESS=sp, Estimate=0, effect="lin",
                                 variable=inter(c(str_subset(apriori_clim_sp$variable, "ldbh", negate=T), apriori_sol_sp$variable, apriori_NCI_sp$variable)), 
                                 sd=2*sd(growth_sp$lgrowth_rate))
  apriori_randef_sp <- data.frame(ESS=sp, Estimate=0, effect="lin", variable="sd", sd=2*sd(growth_sp$lgrowth_rate))
  apriori_sp <- bind_rows(apriori_clim_sp, apriori_sol_sp,apriori_NCI_sp,  apriori_shade_sp, apriori_randef_sp, apriori_inter_sp)
  
  #get climatic, soil and competition effect
  clim <- str_subset(apriori_clim_sp$variable, pattern="ldb", negate=T)
  sol <- str_subset(apriori_sol_sp$variable, pattern="ldb", negate=T)
  NCI <- apriori_NCI_sp$variable
  
  #function in the file function_built_models_priors.R to built all formulas.
  models <- model(clim=clim, sol=sol, NCI=NCI)
  
  res_mod <- list()
  
  #for each model of the species
  for(mod in 1:length(models)){ #mod=9
    print(mod)
    #get the a priori distribution according to the variables tested
    apriori_sp_mod <- apriori_sp %>% 
      filter(variable %in% models[[mod]])

    #Add prior for the random effect
    full_prior <- c(create_prior(apriori_sp_mod), eval(call("prior", call("normal", 0, 1), class = "sd")))
  
    #curve bell on the quadratic effects
    if(any(str_detect(full_prior$coef, "E2"))) full_prior[str_detect(full_prior$coef, "E2"),]$ub <- 0 
    if(any(str_detect(full_prior$coef, ":"))) full_prior[str_detect(full_prior$coef, ":"),]$ub <- NA
    
    #Create the formula
    effect <- paste(full_prior$coef[-length(full_prior$coef)], collapse = " + ") #On enlève tout ce qui vient du random effect
    
    formula <- paste0("lgrowth_rate ~", effect, "+ (1 | PLACE)") #et on rajoute les effets aléatoires
    formula <- str_replace_all(formula, "IldbhE2", "I(ldbh^2)")
    formula <- str_replace_all(formula, "ITAVEE2", "I(TAVE^2)")
    formula <- str_replace_all(formula, "IPPTE2", "I(PPT^2)")
    formula <- str_replace_all(formula, "IDD5E2", "I(DD5^2)")
    formula <- str_replace_all(formula, "IVPDE2", "I(VPD^2)")
    formula <- str_replace_all(formula, "IpHE2", "I(pH^2)")
    formula <- str_replace_all(formula, "ICECE2", "I(CEC^2)")
    formula <- str_replace_all(formula, "IclayE2", "I(clay^2)")
    formula <- bf(formula)
    
    br <- brm(formula, 
              data = growth_sp,
              family = gaussian(), 
              prior = full_prior,
              control = list(adapt_delta = 0.95), 
              chain=2, 
              cores = getOption("mc.cores", 2), 
              iter = 2000)
    
    res_mod <- c(res_mod, list(br))
  }
  
  #test a model without random effect of the full model
  formula <- bf(str_remove(as.character(formula)[1], pattern="\\+ \\(1 \\| PLACE\\)")) #on enlève l'effet aléatoire de la formule
  full_prior <- full_prior[-nrow(full_prior),]
  
  br <- brm(formula, 
            data = growth_sp,
            family = gaussian(), 
            prior = full_prior,
            control = list(adapt_delta = 0.95), 
            chain=2, 
            cores = getOption("mc.cores", 2), 
            iter = 2000)
  
  res_mod <- c(res_mod, list(br))
  
  #select the best model with leave out one process
  loo_mod_sel <- loo(res_mod[[1]], res_mod[[2]], 
                 res_mod[[3]], res_mod[[4]], res_mod[[5]], res_mod[[6]], 
                 res_mod[[7]], res_mod[[8]], res_mod[[9]], res_mod[[10]], res_mod[[11]], res_mod[[12]], 
                 res_mod[[13]], res_mod[[14]], res_mod[[15]], res_mod[[16]], res_mod[[17]], res_mod[[18]], 
                 res_mod[[19]], res_mod[[20]], res_mod[[21]], res_mod[[22]], res_mod[[23]], res_mod[[24]],
                 res_mod[[25]], res_mod[[26]], res_mod[[27]], res_mod[[28]], res_mod[[29]], res_mod[[30]],
                 res_mod[[31]],  res_mod[[32]], res_mod[[33]], res_mod[[34]], res_mod[[35]])
  #Save the model selection
  saveRDS(loo_mod_sel, paste0("res_mod_bayes/mod_sel_loo_", sp,".rds"))
  
  best_model_loo <-res_mod[[as.integer(str_extract(rownames(loo_mod_sel[["diffs"]])[1], "\\d+"))]]
  
  #Save the results of the best model
  saveRDS(best_model_loo, paste0("res_mod_bayes/br_res_", sp,"_best_model_loo.rds"))
}
