library(here)
data_dir <- here('sim_data')
if(!dir.exists(data_dir)) {
  dir.create(data_dir)
}
scenario_df <- expand.grid(rep = 1:5000,
                           effect = c(0, 20),
                           N = c(20, 50, 100))
scenario_df$simindex <- 1:nrow(scenario_df)

set.seed(123)
dat <- do.call(rbind, lapply(1:nrow(scenario_df), function(i) {
  scenario <- scenario_df[i,]
  N <- scenario$N
  x <- rnorm(N, 50, 10)
  x <- x - mean(x)
  z <- rep(0:1, each = N / 2)
 
  y0norm <- 50+2*x + rnorm(N, 0, 20)
  y0cauchy <- 50 + 2*x + rcauchy(N, 0, 4)
  trt_effect <- scenario$effect
  
  ynorm <- y0norm + z*trt_effect
  ycauchy <- y0cauchy + z*trt_effect
  df <- data.frame(ynorm = ynorm,
                   ycauchy = ycauchy,
                   x = x,
                   z = z,
                   simindex = i)
  return(df)
}))
saveRDS(dat, file.path(data_dir, 'sim_data.rds'))

q('no')

