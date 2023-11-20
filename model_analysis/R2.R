rm(list=ls())

setwd("~/PhD/Chap2/model_analysis")
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
library(performance)

res <- list.files("../model/res_mod_bayes", pattern="best_model_loo")

res <- str_subset(res, pattern="PET", negate=T)

sim_obs <- NULL
R2 <- NULL

for(i in res){ #i=res[5]
  sp <- str_split(i, pattern="_")[[1]][3]
  mod <- readRDS(file = paste0("../model/res_mod_bayes/",i))
  R2 <- c(R2, r2_bayes(mod)$R2_Bayes)
  sim_obs_sp <- data.frame(predict(mod), observations=mod$data$lgrowth_rate, species=sp)
  sim_obs <- bind_rows(sim_obs, sim_obs_sp)
}

colnames(sim_obs)[1] <- "simulations"

esp <- data.frame(species=c("BOJ", "BOP", "EPN", "ERR", "ERS", "PET", "SAB"),
                  species2=c("Yellow birch", "White birch", "Black spruce",
                             "Red maple", "Sugar maple", "Trembling aspen", "Balsam fir"))
set.seed(2)
sim_obs_over <- sim_obs %>% 
  group_by(species) %>% 
  sample_n(size=517) %>% 
  inner_join(esp) %>% 
  arrange(species, simulations) %>% 
  mutate(id=1:n()) %>% 
  mutate(col="Simulated",
         shape="Observations")

sd(filter(sim_obs_over, species=="BOJ")$simulations)

adj <- ggplot(data=sim_obs_over, aes(x=id, y= simulations, ymin=Q2.5, ymax=Q97.5, color=col))+ 
  geom_point(aes(y=observations, shape=shape), size=1, color="black") + 
  geom_ribbon(alpha = 0.3, linetype=0)  + geom_line() +
  scale_color_discrete(name="") +
  scale_shape_discrete(name="") +
  facet_wrap(~species2, scale="free_x") +
  ylab("Log growth") +
  xlab("Position of predictions sorted in ascending order")+
  theme(legend.text = element_text(size=12),
        legend.title = element_text(size=12),
        legend.position="bottom",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        strip.text = element_text(size = 12)
        )

adj

pdf("~/PhD/chap2/article/figures/model_analysis/adjustement_quality.pdf", width =9, height = 7)
adj
dev.off()

png(filename = "~/PhD/chap2/article/figures/model_analysis/adjustement_quality.png", width=9, height=7, units="in", res=500) 
adj
dev.off()


#Prédits en fonction des simulés
ggplot(data=sim_obs, aes(x=simulations, y= observations-simulations)) + geom_point()  + facet_wrap(~species) + geom_abline(intercept = 0, slope = 1) + 
  geom_text(data=R2, aes(label=R2$Estimate))
x=1:100+runif(100)
y=1:100
plot(lm(x~y))

pp_check(mod)  # shows dens_overlay plot by default
pp_check(mod, type = "error_hist", ndraws = 11)
pp_check(mod, type = "scatter_avg", ndraws = 100)
pp_check(mod, type = "stat_2d")
pp_check(mod, type = "loo_pit")
pp_check(mod, type = "loo_pit_overlay")

launch_shinystan(mod)

sim_obs_sp <- cbind(simulations=predict(mod)[,1], mod$data, species=sp) 

ggplot(data=sim_obs_sp, aes(x=exp(lgrowth_rate), y= exp(simulations), color=NCI_intra)) + geom_point() +
  geom_abline(intercept = 0, slope = 1) + facet_wrap(~PLACE) 

library(marginaleffects)
predictions(mod)
