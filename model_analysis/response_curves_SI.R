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

res <- list.files("../model/res_mod_bayes", pattern="best_model_loo")


for(i in res){ #i=res[1]
  mod <- readRDS(file = paste0("../model/res_mod_bayes/",i))
  a=predict(mod)
  b=data.frame(sim=a[,1], obs=mod$data$lgrowth_rate)
  
  ggplot(data=b, aes(x=exp(obs), y= exp(sim))) + geom_point() + geom_abline(intercept = 0, slope = 1)
  
  
  bayes_R2(mod)
  
  sp <- str_split(i, pattern="_")[[1]][3]
  
  ###Get responses curves
  coef_sign <- data.frame(fixef(mod)) %>%
    mutate(species=sp, 
           effect=rownames(.)) 
  coef_sign$effect <- factor(coef_sign$effect, levels = factor(coef_sign$effect))
  
  coef_sign2 <- coef_sign
  
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IldbhE2", "log(DBH)²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "ldbh", "log(DBH)")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "ITAVEE2", "TAVE²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IPPTE2", "PPT²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IDD5E2", "DD5²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IVPDE2", "VPD²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IpHE2", "pH²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "ICECE2", "CEC²")
  coef_sign2$effect <- str_replace_all(coef_sign2$effect, "IclayE2", "clay²")
  
  coef_sign2$effect <- factor(coef_sign2$effect, levels = factor(coef_sign2$effect))
  
  plot_model <- ggplot(coef_sign2, aes(x=Estimate, y=effect, xmin=Q2.5, xmax=Q97.5)) + 
    geom_point() + geom_linerange() +
    geom_vline(xintercept = 0) +
    scale_y_discrete(limits=rev) +
    ylab("Effect") + xlab("Model estimate")
  
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IldbhE2", "ldbh")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "ITAVEE2", "TAVE")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IPPTE2", "PPT")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IDD5E2", "DD5")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IVPDE2", "VPD")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IpHE2", "pH")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "ICECE2", "CEC")
  coef_sign$effect <- str_replace_all(coef_sign$effect, "IclayE2", "clay")
  
  me <- conditional_effects(mod, re_formula=NA, ask=FALSE, plot=F, surface = F) #surface=T pour plot 2d interaction
  
  cond_ef_res <- NULL
  cond_ef_res_inter <- NULL
  eff_inter <- list()
  for(j in unique(str_subset(coef_sign$effect, "Intercept", negate=T))){ #j="TAVE"
    sub_cond <- get("me")[[j]]
    if(!str_detect(j, ":")){ #Pas d'interactions
      cond_ef_res <- rbind(cond_ef_res, data.frame(species=sp, effect=j, value=unname(as.vector(sub_cond[j]))[[1]], 
                                                   growth=sub_cond$estimate__, lower = sub_cond$lower__, upper = sub_cond$upper__))
      
      
      
      
    }else{#Si interactions
      cond_ef_res_inter <- data.frame(species=sp, effect=j, 
                                      effect1= str_split(j, ":")[[1]][1], effect2=str_split(j, ":")[[1]][2],
                                      value1=unname(as.vector(sub_cond[str_split(j, ":")[[1]][1]]))[[1]],
                                      value2=sub_cond$effect2__,
                                      growth=sub_cond$estimate__, lower = sub_cond$lower__, upper = sub_cond$upper__)
      
      eff_inter[[j]] <- 
        ggplot(cond_ef_res_inter, aes(x=value1, y=growth, color=value2, group=value2, fill=value2, ymin = lower, ymax = upper)) + 
        geom_line()  +
        geom_ribbon(alpha=0.1, colour=NA)+
        labs(color = unique(cond_ef_res_inter$effect2), 
             fill = unique(cond_ef_res_inter$effect2)) +
        ylab(NULL) +
        xlab(unique(cond_ef_res_inter$effect1)) +
        theme(legend.position = "right") + 
        theme(legend.text = element_text(size=8),
              legend.title = element_text(size=8),
              legend.key.size = unit(0.35, 'cm'), 
              axis.text.y=element_text(size=8),
              axis.text.x=element_text(size=7),
              axis.title=element_text(size=8))
    }
  }
  
  cond_ef_res$effect <- str_replace_all(cond_ef_res$effect, "ldbh", "log(DBH)")
  
  eff_simple <- ggplot(cond_ef_res, aes(x=value, y=growth, ymin = lower, ymax = upper)) + 
    geom_ribbon(alpha=0.5) + 
    geom_line() + ylab("log growth") +
    facet_wrap(~effect, scale="free_x", nrow=1, strip.position = "bottom") +
    theme(strip.background = element_blank(),
          strip.placement = "outside") + xlab(NULL) +
    coord_cartesian(ylim = c(-1.5, NA))+ 
    theme(axis.text.y=element_text(size=8),
          axis.text.x=element_text(size=7))
  
  ranef_sp <- mod %>% #https://discourse.mc-stan.org/t/convenience-function-for-plotting-random-group-effects/13461/4
    spread_draws(b_Intercept, r_PLACE[PLACE,]) %>%
    # add the grand mean to the group-specific deviations
    mutate(mu = b_Intercept + r_PLACE) %>%
    ungroup() %>%
    
    # plot
    ggplot(aes(x = mu, y = reorder(PLACE, mu))) +
    geom_vline(xintercept = fixef(mod)[1, 1], linewidth = 1) +
    geom_vline(xintercept = fixef(mod)[1, 3:4], linetype = 2) +
    stat_halfeye(.width = .5, linewidth = 2/3) +
    labs(x = expression("log growth "),
         y = "RESEF sites ordered by mean predicted log growth") +
    theme(panel.grid   = element_blank(),
          axis.ticks.y = element_blank(),
          axis.text.y  = element_text(hjust = 0))
  
  
  ###Get responses curves
  distribution <- NULL
  
  prior <- bind_cols(prior=mod[["prior"]]$prior, coef=mod[["prior"]]$coef)
  prior <- prior[2:(nrow(prior)-5),]
  prior <- prior %>% 
    mutate(prior=str_remove_all(prior, "normal"),
           prior=str_remove_all(prior, "[()]")) %>% 
    separate(prior, c("mean", "sd"), ",") %>% 
    mutate(species=sp,
           mean = as.numeric(mean),
           sd=as.numeric(sd)) %>% 
    filter(mean != 0) 
  
  prior2 <- NULL
  
  for(k in 1:nrow(prior)){ #k=1
    x <- seq(-.3, .3, length=100)
    prior2 <- bind_rows(prior2, 
                        data.frame(species=prior$species[k],
                                   coef=prior$coef[k],
                                   mean=prior$mean[k],
                                   sd=prior$sd[k],
                                   x=x,
                                   dnorm=dnorm(x, mean=prior$mean[k], sd=prior$sd[k])))
  }
  
  prior2 <- mutate(prior2, distribution="a priori")
  
  draws <- prepare_predictions(mod)
  mu <- draws$dpars$mu$fe$b %>% 
    as.data.frame() %>% 
    pivot_longer(everything(), names_to = "coef", values_to = "mean") %>% 
    mutate(coef=str_remove(coef, "b_")) %>% 
    filter(coef %in% unique(prior$coef)) %>% 
    mutate(distribution="a posteriori")
  
  mu$coef <- str_replace_all(mu$coef, "IldbhE2", "log(DBH)²")
  mu$coef <- str_replace_all(mu$coef, "ldbh", "log(DBH)")
  mu$coef <- str_replace_all(mu$coef, "ITAVEE2", "TAVE²")
  mu$coef <- str_replace_all(mu$coef, "IPPTE2", "PPT²")
  mu$coef <- str_replace_all(mu$coef, "IDD5E2", "DD5²")
  mu$coef <- str_replace_all(mu$coef, "IVPDE2", "VPD²")
  mu$coef <- str_replace_all(mu$coef, "IpHE2", "pH²")
  mu$coef <- str_replace_all(mu$coef, "ICECE2", "CEC²")
  mu$coef <- str_replace_all(mu$coef, "IclayE2", "clay²")
  
  prior2$coef <- str_replace_all(prior2$coef, "IldbhE2", "log(DBH)²")
  prior2$coef <- str_replace_all(prior2$coef, "ldbh", "log(DBH)")
  prior2$coef <- str_replace_all(prior2$coef, "ITAVEE2", "TAVE²")
  prior2$coef <- str_replace_all(prior2$coef, "IPPTE2", "PPT²")
  prior2$coef <- str_replace_all(prior2$coef, "IDD5E2", "DD5²")
  prior2$coef <- str_replace_all(prior2$coef, "IVPDE2", "VPD²")
  prior2$coef <- str_replace_all(prior2$coef, "IpHE2", "pH²")
  prior2$coef <- str_replace_all(prior2$coef, "ICECE2", "CEC²")
  prior2$coef <- str_replace_all(prior2$coef, "IclayE2", "clay²")
  
  distrib_plot <- ggplot(mu, aes(x=mean)) + geom_histogram(aes(y = after_stat(density), fill=distribution)) +
    scale_fill_manual(values="grey30") +
    facet_wrap(~coef, scale="free_y", ncol=2) +
    geom_line(data=prior2, aes(x=x, y=dnorm, col=distribution)) +
    xlab("Parameter value") +
    theme(legend.text = element_text(size=8),
          legend.title = element_text(size=8),
          legend.key.size = unit(0.35, 'cm'),
          legend.position = "bottom")
  
  
  
  if(length(cond_ef_res_inter)==0){
    pdf(file=paste0("response_curve/",sp,".pdf"), width=20)
    print(annotate_figure(
      ggarrange(distrib_plot, plot_model, eff_simple, ranef_sp, nrow=1, widths=c(0.45,0.55,1,0.3), label=c("A", "B", "C", "E")),
      top = text_grob(sp, face = "bold", size = 14)))
    dev.off()
  }else{
    if(length(eff_inter)>9){
      eff_inter_all <- ggarrange(plotlist=eff_inter, ncol=4, nrow=ceiling(length(eff_inter)/4))
    }else{
      eff_inter_all <- ggarrange(plotlist=eff_inter)
    }
    
    eff_inter_all <- annotate_figure(eff_inter_all, left = text_grob("log growth", size = 11, rot = 90))
    
    all_effect <- ggarrange(eff_simple, eff_inter_all, nrow=2, 
                            heights = c(1, ceiling(length(eff_inter)/8)), labels=c("C", "D"))
    
    plot_all <- ggarrange(plot_model, all_effect, nrow=1, widths=c(0.55,1), labels=c("B", NULL))  
    
    plot_effect_distrib <- ggarrange(distrib_plot, plot_all, nrow=1, widths=c(0.3,1), labels=c("A", NULL)) 
    
    ranef_sp2 <- ggarrange(ranef_sp, nrow=1, labels=c("E"))
    
    plot_effect_distrib_ranef <- ggarrange(plot_effect_distrib, ranef_sp2, nrow=1, widths=c(1,.2))
    
    species <- data.frame(species= c("Balsam fir", "Red maple", "Sugar maple", "Yellow birch", "White birch", "Black spruce"), 
                          code=c("SAB", "ERR", "ERS", "BOJ", "BOP", "EPN"))
    
    
    prior_effect_site <- ggarrange(distrib_plot, plot_model, ranef_sp, nrow=1, labels=c("A", "B", "C"))
    all_effect2 <- ggarrange(eff_simple, eff_inter_all, nrow=2, 
                             heights = c(1, ceiling(length(eff_inter)/8)), labels=c("D", "E"))
    plot_these <- ggarrange(prior_effect_site, all_effect2, ncol=1)
    
    species <- data.frame(species= c("Balsam fir", "Red maple", "Sugar maple", "Yellow birch", "White birch", "Black spruce"), 
                          code=c("SAB", "ERR", "ERS", "BOJ", "BOP", "EPN"))
    
    pdf(file=paste0(sp,"_these.pdf"), height=15, width=10)
    print(annotate_figure(plot_these, top = text_grob(filter(species, code==sp)[1,1], face = "bold", size = 14)))
    dev.off()
    
    png(filename=paste0(sp,"_these.png"), width=10, height=15, units = "in", res=1000)
    print(annotate_figure(plot_these, top = text_grob(filter(species, code==sp)[1,1], face = "bold", size = 14)))
    dev.off()
  }
}