#Script with all models (all combinations possible) for selection

#Null model
brq_null <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                   interc ~ 1, lin ~ 0 + ldbh,
                   quad ~ 0 + I(ldbh^2)),
                prior = c(prior(normal(0, 1), nlpar = interc),
                          prior(normal(0, 1), class = b, nlpar = lin),
                          prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
                data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#########TAVE and PPT##########
#TAVE+TAVE²+PPT+PPT²
brq1 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE + PPT,
               quad ~ 0 + I(ldbh^2) + I(TAVE^2) + I(PPT^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE+PPT+PPT²
brq2 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE + PPT,
               quad ~ 0 + I(ldbh^2) + I(PPT^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#PPT+PPT²
brq3 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + PPT,
               quad ~ 0 + I(ldbh^2) + I(PPT^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE+TAVE²+PPT
brq4 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE + PPT,
               quad ~ 0 + I(ldbh^2) + I(TAVE^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))


#TAVE+TAVE²
brq5 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE,
               quad ~ 0 + I(ldbh^2) + I(TAVE^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE
brq6 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE,
               quad ~ 0 + I(ldbh^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#PPT
brq7 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + PPT,
               quad ~ 0 + I(ldbh^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE + PPT
brq8 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE + PPT,
               quad ~ 0 + I(ldbh^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))



#########TAVE and VPD##########
#TAVE+TAVE²+VPD+VPD²
brq9 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
               interc ~ 1, lin ~ 0 + ldbh + TAVE + VPD,
               quad ~ 0 + I(ldbh^2) + I(TAVE^2) + I(VPD^2)),
            prior = c(prior(normal(0, 1), nlpar = interc),
                      prior(normal(0, 1), class = b, nlpar = lin),
                      prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
            data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE+VPD+VPD²
brq10 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + TAVE + VPD,
                quad ~ 0 + I(ldbh^2) + I(VPD^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#VPD+VPD²
brq11 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + VPD,
                quad ~ 0 + I(ldbh^2) + I(VPD^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))
#TAVE+TAVE²+VPD
brq12 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + TAVE + VPD,
                quad ~ 0 + I(ldbh^2) + I(TAVE^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#VPD
brq13 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + VPD,
                quad ~ 0 + I(ldbh^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#TAVE + VPD
brq14 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + TAVE + VPD,
                quad ~ 0 + I(ldbh^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))


#########DD5 and PPT##########
#DD5+DD5²+PPT+PPT²
brq15 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + PPT,
                quad ~ 0 + I(ldbh^2) + I(DD5^2) + I(PPT^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#DD5+PPT+PPT²
brq16 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + PPT,
                quad ~ 0 + I(ldbh^2) + I(PPT^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#DD5+DD5²
brq17 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5,
                quad ~ 0 + I(ldbh^2) + I(DD5^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))
#DD5+DD5²+PPT
brq18 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + PPT,
                quad ~ 0 + I(ldbh^2) + I(DD5^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#DD5
brq19 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5,
                quad ~ 0 + I(ldbh^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#DD5 + PPT
brq20 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + PPT,
                quad ~ 0 + I(ldbh^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))


#########DD5 and VPD##########
#DD5+DD5²+VPD+VPD²
brq21 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + VPD,
                quad ~ 0 + I(ldbh^2) + I(DD5^2) + I(VPD^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#DD5+VPD+VPD²
brq22 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + VPD,
                quad ~ 0 + I(ldbh^2) + I(VPD^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))

#VPD+DD5+DD5²
brq23 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + VPD + DD5,
                quad ~ 0 + I(ldbh^2) + I(DD5^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))


#DD5 + VPD
brq24 <- brm(bf(log(DBHI) ~ interc + lin + quad, quantile = 0.5, nl = TRUE,
                interc ~ 1, lin ~ 0 + ldbh + DD5 + VPD,
                quad ~ 0 + I(ldbh^2)),
             prior = c(prior(normal(0, 1), nlpar = interc),
                       prior(normal(0, 1), class = b, nlpar = lin),
                       prior(normal(0, 1), class = b, ub = 0, nlpar = quad)),
             data = clim_growth_norm_spe, family = asym_laplace(), chains = 2, cores = getOption("mc.cores", 2))