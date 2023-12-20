library(here)
output_dir <- here("sim_output")
sim_files <- list.files(output_dir, pattern = "^run*", full.names = TRUE)
results <- do.call(rbind, lapply(sim_files, readRDS))

data_dir <- here('sim_data')
if(!dir.exists(data_dir)){
  dir.create(data_dir, recursive = TRUE)
}
saveRDS(results, file.path(data_dir, "sim_results.rds"))

#sim_time_files <- list.files(output_dir, pattern = "analysis-time-run*", full.names = TRUE)
#sim_time_results <- do.call(rbind, lapply(sim_time_files, readRDS))
#saveRDS(sim_time_results, file.path(data_dir, "sim_time_results.rds"))

quit('no')
