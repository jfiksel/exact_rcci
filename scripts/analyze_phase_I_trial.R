library(dplyr)
library(ri2)
library(Rfast)
library(here)
library(tidyverse)
source(here("scripts", "ancova_ri_ci.R"))
library(matrixStats)


dat <- read_rds(here('data', 'reserpine.rds')) 
N <- nrow(dat)
x <- dat$HR
z <- ifelse(dat$DRUGCD == 'placebo', 0, 1)
y <- dat$HRPOST
set.seed(123)
declaration <- declare_ra(N = N, m = sum(z == 1))
random_perms <- obtain_permutation_matrix(declaration, 20000)

preds <- resids <- deltas <- matrix(NA, nrow = nrow(random_perms), ncol=ncol(random_perms))
for(i in 1:ncol(preds)) {
  fit <- lm(random_perms[,i] ~ x)
  deltas[,i] <- 2*random_perms[,i] - 1
  preds[,i] <- predict(fit)
  resids[,i] <- residuals(fit)*deltas[,i]
}

min(colSums(resids))
min(colMins(preds))


diffmean_pval <- fisher_p_diffmeans(y, z, random_perms)

diffmean_ci <- fisher_ci_diffmeans(y, z, random_perms)

ancova_ci_pval <- fisher_ci_ancova(y, z, x, random_perms)

lm(y~z + x) %>% summary()
lm(y~z + x) %>% confint()

