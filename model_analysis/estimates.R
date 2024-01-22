#Figure 3, raw estimates of dbh, shading and competition
rm(list=ls())


library(tidyverse)
theme_set(theme_bw())
library(brms)
library(shinystan)
library(rstan)
library(sjPlot)
library(sjmisc)
library(ggpubr)
require(grid)

res <- list.files("../model/res_mod_bayes", pattern = "model_loo")
formula <- NULL

coef_sign <- NULL

for(i in res){ #i=res[1]
  mod <- readRDS(file = paste0("../model/res_mod_bayes/",i)) #get the best model
  sp <- str_split(i, pattern="_")[[1]][3]
  formula <- bind_rows(formula,data.frame(species=sp, formula=as.character(mod$formula)[1]))
  #extract the estimates
  coef_sign <- data.frame(fixef(mod)) %>% 
    mutate(species=sp, 
           effect=rownames(.)) %>% 
    bind_rows(coef_sign) 
}

coef_sign$effect <- factor(coef_sign$effect, levels=unique(coef_sign$effect))

esp <- data.frame(species=c("BOJ", "BOP", "EPN", "ERR", "ERS", "SAB"),
                  species2=c("Yellow birch", "White birch", "Black spruce",
                             "Red maple", "Sugar maple",  "Balsam fir"))

coef_sign <- coef_sign %>% 
  inner_join(esp) %>% 
  filter(effect %in% c("ldbh","IldbhE2","Shading","NCI_conifer", "NCI_intra", "NCI_deciduous"))

coef_sign$effect <- fct_relevel(coef_sign$effect, c("ldbh", "IldbhE2", "NCI_intra","NCI_deciduous","NCI_conifer","Shading"))

coef_sign$effect <- str_replace_all(coef_sign$effect, "IldbhE2", "logDBH²")
coef_sign$effect <- str_replace_all(coef_sign$effect, "ldbh", "logDBH")


all_sp2 <- ggplot(coef_sign, aes(x=Estimate, y=effect, xmin=Q2.5, xmax=Q97.5)) + 
  geom_point() + geom_linerange() +
  facet_wrap(species2~., scale="free_y") +
  geom_vline(xintercept = 0) +
  scale_y_discrete(limits=rev) +
  ylab(NULL) +
  xlab("Estimates")

pdf("species_estimates_loo.pdf", width = 6, height = 5)
all_sp2
dev.off()

png(filename = "species_estimates_loo.png", width=6, height=5, units="in", res=500) 
all_sp2
dev.off()
