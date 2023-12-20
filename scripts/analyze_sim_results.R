library(here)
library(tidyverse)
library(kableExtra)

data_dir <- here('sim_data')
results <- readRDS(file.path(data_dir, "sim_results.rds"))
#time_results <- readRDS(file.path(data_dir, "sim_time_results.rds"))

results %>%
  filter(statistic == "ancovafrt") %>%
  group_by(statistic, effect, scenario, N) %>%
  summarise(mon = mean(monotonic == 'Monotonic'))

summarised_results <-
  results %>%
  filter(!is.na(statistic)) %>%
  filter(is.na(monotonic) | monotonic == 'Monotonic') %>%
  group_by(statistic, effect, scenario, N) %>%
  summarise(pct_reject = mean(pval < .05),
            coverage = mean(ci_l <= effect & effect <= ci_u),
            width = median(ci_u - ci_l))

summarised_time_results <-
  results %>%
  filter(statistic == "ancovafrt") %>%
  mutate(time_ratio = ci_time / pval_time) %>%
  group_by(N) %>%
  summarise(pvaltime = mean(pval_time),
            citime = mean(ci_time),
            ratio = mean(time_ratio))
kbl(summarised_time_results, digits = 2, format = "latex") %>%
  #kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "striped", html_font = "Cambria") 

ggplot(summarised_results, aes(x = scenario, y = coverage, color = statistic)) +
  geom_point() +
  facet_grid(N~effect)

ggplot(summarised_results, aes(x = scenario, y = width, color = statistic)) +
  geom_point(alpha = .4) +
  facet_grid(N~effect)

ggplot(summarised_results, aes(x = scenario, y = pct_reject, color = statistic)) +
  geom_point() +
  facet_grid(effect ~ N, scales = 'free')

### Now to table
summarised_results_tab <-
  summarised_results %>%
  mutate(scenario = ifelse(scenario == "norm", "Normal", "Cauchy"),
         statistic = case_when(statistic == "ancovafrt" ~ "ANCOVA FRT",
                               statistic == "ancovalm" ~ "ANCOVA Standard",
                               statistic == "diffinmeansfrt" ~ "Diff-in-means FRT",
                               statistic == "diffinmeanslm" ~ "Diff-in-means Standard")) %>%
  mutate(scenario2 = paste0(scenario, ": N = ", N, "")) %>%
  group_by(statistic, scenario2) %>%
  summarise(`Type-I Error` = pct_reject[effect == 0],
            Coverage = coverage[effect == 20],
            `Median 95% CI Width` = width[effect == 20],
            `Power` = pct_reject[effect == 20]) %>%
  rename(Method = statistic)
scenario_levels <- c("Normal: N = 20", "Normal: N = 50", "Normal: N = 100",
                     "Cauchy: N = 20", "Cauchy: N = 50", "Cauchy: N = 100")
method_levels <- c("Diff-in-means Standard", "Diff-in-means FRT",
                   "ANCOVA Standard", "ANCOVA FRT")
summarised_results_tab$scenario2 <- factor(summarised_results_tab$scenario2, levels = scenario_levels)
summarised_results_tab$Method <- factor(summarised_results_tab$Method, levels = method_levels)

summarised_results_tab <- arrange(summarised_results_tab, scenario2, Method)
kbl(select(summarised_results_tab, -scenario2), digits = 2, format = "latex") %>%
  pack_rows(index = table(summarised_results_tab$scenario2)) %>%
  #kable_classic(full_width = F, html_font = "Cambria") %>%
  kable_styling(latex_options = "striped", html_font = "Cambria") 


