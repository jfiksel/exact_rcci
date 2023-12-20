# Code for On Exact Randomization-based Covariate-adjusted Confidence Intervals

The simulation study and data analysis for this paper was conducted in R version 4.0.5.
You will need to run the following lines of code to install the following packages to replicate the study:

```
package_vec <- c('tidyverse', 'here', 'Rfast', 'matrixStats', 'fpCompare', 'ri2', 'kableExtra')
install.packages(package_vec)
```

For the simulation study, you need to do the following:

1. Run `generate_data_sims.r`
2. On a cluster run `simulation_study.sh`, which will run `simulation_study.R` (the main
script to perform the simulations) in parallel. You can also re-write the code in `simulation_study.R`
so that it can run without a shell script
3. Run `collect_sim_output.R`
4. Run `analyze_sim_results.R`

The data analysis can be performed by running `analyze_phase_I_trial.R`. The data
is already is in the `data` folder, and was downloaded from the [Supporting Information from Pair-switching rerandomization](https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fbiom.13712&file=biom13712-sup-0002-SuppMat.zip).

