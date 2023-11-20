#Script with all models (all combinations possible) for selection

brq_full <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                   interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                   quad ~ 0 + I(ldbh^2) + I(pH_eau^2) + I(PC_ARGIL^2) + I(CEC^2)),
                prior = c(prior(normal(0, 1), nlpar = interc),
                          prior(normal(0, 1), class = b, nlpar = lin),
                          prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
                data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_1 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2) + I(PC_ARGIL^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_2 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2) + I(PC_ARGIL^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_3 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2) + I(CEC^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_4 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + CEC,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2) + I(CEC^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_5 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                   interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                   quad ~ 0 + I(ldbh^2) + I(PC_ARGIL^2) + I(CEC^2)),
                prior = c(prior(normal(0, 1), nlpar = interc),
                          prior(normal(0, 1), class = b, nlpar = lin),
                          prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
                data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_6 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                   interc ~ 1, lin ~ 0 + ldbh + PC_ARGIL + CEC,
                   quad ~ 0 + I(ldbh^2) + I(PC_ARGIL^2) + I(CEC^2)),
                prior = c(prior(normal(0, 1), nlpar = interc),
                          prior(normal(0, 1), class = b, nlpar = lin),
                          prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
                data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_7 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2) ),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_8 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + CEC ,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))


brq_9 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + PC_ARGIL + CEC,
                quad ~ 0 + I(ldbh^2) + I(PC_ARGIL^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_10 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                quad ~ 0 + I(ldbh^2) + I(pH_eau^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_11 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                 quad ~ 0 + I(ldbh^2) + I(PC_ARGIL^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_12 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                 quad ~ 0 + I(ldbh^2) + I(CEC^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_13 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL + CEC,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_14 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau + PC_ARGIL,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_15 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau + CEC,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_16 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + PC_ARGIL + CEC,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_17 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + PC_ARGIL,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_18 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + pH_eau,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_19 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + CEC,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_20 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + CEC + pH_eau,
                 quad ~ 0 + I(ldbh^2) + I(CEC^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_21 <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh + CEC + PC_ARGIL,
                 quad ~ 0 + I(ldbh^2) + I(CEC^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

brq_null <- brm(bf(lgrowth_rate ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                 interc ~ 1, lin ~ 0 + ldbh,
                 quad ~ 0 + I(ldbh^2)),
              prior = c(prior(normal(0, 1), nlpar = interc),
                        prior(normal(0, 1), class = b, nlpar = lin),
                        prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
              data = soil_growth_norm_esp, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))
