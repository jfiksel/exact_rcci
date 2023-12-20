task_index <- as.numeric(commandArgs(trailingOnly = TRUE))
library(here)
output_dir <- here("sim_output")
if(!dir.exists(output_dir)){
  dir.create(output_dir)
}
library(dplyr)
library(ri2)
library(Rfast)
source(here("scripts", "ancova_ri_ci.R"))

Nvec <- c(20, 50, 100)
set.seed(123)
perms_list <- lapply(Nvec, function(N) {
  declaration <- declare_ra(N = N, m = N / 2)
  random_perms <- obtain_permutation_matrix(declaration, 20000)
  return(random_perms)
})
names(perms_list) <- Nvec


all_dat <- readRDS(here('sim_data', 'sim_data.rds'))

scenario_df <- expand.grid(rep = 1:5000,
                           effect = c(0, 20),
                           N = c(20, 50, 100))

scenario_df$simindex <- 1:nrow(scenario_df)
scenario_df$taskindex <- rep(1:50, each = nrow(scenario_df) / 50)
scenarios <- filter(scenario_df, taskindex == task_index)
for(i in 1:nrow(scenarios)) {
  scenario <- scenarios[i,]
  sim_index <- scenario$simindex
  results.file <- file.path(output_dir, paste0("run-", sim_index, ".rds"))
  if(file.exists(results.file)) {
    next
  }
  dat <- filter(all_dat, simindex == sim_index)
  N <- scenario$N
  x <- dat$x
  z <- dat$z
  
  ymat <- matrix(c(dat$ynorm, dat$ycauchy), ncol = 2)
  random_perms <- perms_list[[as.character(N)]]
  diffmean_pvals <- sapply(1:ncol(ymat), function(i) {
    fisher_p_diffmeans(ymat[,i], z, random_perms)
  })
  
  diffmean_ci <- do.call(rbind, lapply(1:ncol(ymat), function(i) {
    fisher_ci_diffmeans(ymat[,i], z, random_perms)
  }))
  
  diffmean_ci$pval <- diffmean_pvals
  diffmean_ci$scenario <- c('norm', 'cauchy')
  diffmean_ci$statistic <- 'diffinmeansfrt'
  
 # ancova_pvals <- sapply(1:ncol(ymat), function(i) {
  #  fisher_p_ancova(ymat[,i], z, x, random_perms)
  #})
  
  ancova_ci_pvals <- do.call(rbind, lapply(1:ncol(ymat), function(i) {
    fisher_ci_ancova(ymat[,i], z, x, random_perms)
  }))
  
  #ancova_ci$pvals <- ancova_pvals
  ancova_ci_pvals$scenario <- c('norm', 'cauchy')
  ancova_ci_pvals$statistic <- 'ancovafrt'
  
  ### now standard CIs
  standard_methods_df <- do.call(rbind, lapply(1:ncol(ymat), function(i) {
    fit_z <- lm(ymat[,i] ~ z)
    fit_ancova <- lm(ymat[,i] ~ z + x)
    pvals <- c(summary(fit_z)$coefficients[2,4], summary(fit_ancova)$coefficients[2,4])
    ci_l <- c(confint(fit_z)[2,1], confint(fit_ancova)[2,1])
    ci_u <- c(confint(fit_z)[2,2], confint(fit_ancova)[2,2])
    df <- data.frame(pval = pvals, ci_l = ci_l, ci_u = ci_u,
                     statistic = c("diffinmeanslm", "ancovalm"))
    return(df)
  }))
  standard_methods_df$scenario <- rep(c('norm', 'cauchy'), each = 2)
  
  
  all_cis <-
    diffmean_ci %>%
    bind_rows(ancova_ci_pvals) %>%
    bind_rows(standard_methods_df) %>%
    bind_cols(scenario)
  saveRDS(all_cis, results.file)
}
quit('no')

