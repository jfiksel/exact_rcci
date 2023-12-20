library(Rfast)
library(fpCompare)
fisher_p_diffmeans <- function(y, z, random_perms) {
  ri_betas <- Rfast:: allbetas(y, random_perms)[,2]
  obs_dim <- mean(y[z==1]) - mean(y[z==0])
  pi_L <- mean(ri_betas %<=% obs_dim)
  pi_U <-  mean(ri_betas %>=% obs_dim)
  p_vec <- unname(c(pi_L, pi_U))
  return(2*min(p_vec))
}


fisher_ci_diffmeans <- function(y, z, random_perms, alpha = 0.05) {
  perms_switch_ctrl <- (random_perms + 1) == z
  perms_switch_trt <- (random_perms - 1) == z
  n_switch_ctrl <- colsums(perms_switch_ctrl)
  sum_switch_ctrl <- colsums(y*perms_switch_ctrl)
  sum_switch_trt <- colsums(y*perms_switch_trt)
  tau_solve <- (sum_switch_ctrl - sum_switch_trt) / n_switch_ctrl
  if (ncol(random_perms) * alpha / 2 <= 1) {
    ci_low <- -Inf
    ci_up <- Inf
  } else {
    tau_solve_l <- ifelse(is.na(tau_solve), min(tau_solve, na.rm = T), tau_solve)
    ci_low <- sort(tau_solve_l)[floor(ncol(random_perms) * alpha / 2) + 1]
    tau_solve_h <- ifelse(is.na(tau_solve), max(tau_solve, na.rm = T), tau_solve)
    ci_up <- sort(tau_solve_h, decreasing = T)[floor(ncol(random_perms) * alpha / 2) + 1]
  }
  output <- data.frame(ci_l = ci_low, ci_u = ci_up)
  return(output)
}


fisher_ci_ancova <- function(y, z, x, random_perms, alpha = 0.05, H1 = "!=") {
  pval_start <- Sys.time() ### Optional
  beta_orig <- unname((coefficients(lm(y ~ z + x))))[2]
  n <- dim(random_perms)[1]
  mz <- Rfast::colmeans(random_perms)
  mx <- sum(x)/n
  sx <- (sum(x^2) - sum(x)^2/n)/(n - 1)
  denom <- n - 1
  r <- (Rfast::eachcol.apply(random_perms, x) - n * mx * mz)/denom
  be <- r/sx
  a <- mz - be * mx
  residuals <- random_perms - (rep_row(a, n) + rep_row(be, n)*x)
  ancovabetas <- allbetas(y, residuals)[,2]
  pi_L <- mean(ancovabetas %<=% beta_orig)
  pi_U <-  mean(ancovabetas %>=% beta_orig)
  p_vec <- unname(c(pi_L, pi_U))
  pval <- 2*min(p_vec)
  pval_end <- Sys.time()
  pval_time <- difftime(pval_end, pval_start, units = 'secs')[[1]]
  ci_start <- Sys.time()
  perms_switch_ctrl <- (random_perms + 1) == z
  perms_switch_trt <- (random_perms - 1) == z
  zchangemat <- perms_switch_trt - perms_switch_ctrl 
  covars <- colsums(zchangemat*residuals)
  vars <- colsums(residuals^2)
  betaswitch <- covars / vars
  tau_solve <- (beta_orig - ancovabetas) / betaswitch
  if (ncol(random_perms) * alpha / 2 <= 1) {
    ci_low <- -Inf
    ci_up <- Inf
  } else {
    tau_solve_l <- ifelse(is.na(tau_solve), min(tau_solve, na.rm = T), tau_solve)
    ci_low <- sort(tau_solve_l)[floor(ncol(random_perms) * alpha / 2) + 1]
    tau_solve_h <- ifelse(is.na(tau_solve), max(tau_solve, na.rm = T), tau_solve)
    ci_up <- sort(tau_solve_h, decreasing = T)[floor(ncol(random_perms) * alpha / 2) + 1]
  }
  ci <- unname(c(ci_low, ci_up))
  eff_increasing_condition <- colsums((2*random_perms - 1)*residuals)
  if(any(eff_increasing_condition < 0)) {
    monotonic <- "Non-monotonic"
  } else {
    monotonic <- "Monotonic"
  }
  ci_end <- Sys.time()
  ci_time <- difftime(ci_end, ci_start, units = 'secs')[[1]]
  output <- data.frame(monotonic = monotonic, pval = pval, ci_l = ci[1], ci_u = ci[2],
                       pval_time = pval_time, ci_time = ci_time)
  return(output)
}

