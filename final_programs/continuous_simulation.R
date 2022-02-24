## script to run continuous outcomes data gen and bootstrapping
source("./shared_code/setup.R")
scenario_id = 14


## Generating 100 Samples from Our Population
source("./shared_code/data_gen_continuous_updated.R")


## The Simple Bootstrap
source("./shared_code/boot_cont_simple.R")


## Summary of 1000 Simple Bootstraps in 100 Sub-Populations
boot_result <-
  boot_estimates %>%
  group_by(seq) %>%
  summarize(ATE = mean(estimate),
            sd_ATE = sd(estimate),
            simp_perc_25 = quantile(estimate, probs = 0.025),
            simp_perc_975 = quantile(estimate, probs = 0.975))
 
fig1 <-
  boot_result %>%
  ggplot(aes(x = sd_ATE, color = sd_ATE)) +
  geom_histogram(fill = "light blue",  bins = 12,  color = "black") +
  geom_density(aes(y = ..density..*2), colour = "red", 
               fill = "black", alpha = 0.3) + 
  geom_vline(xintercept = mean(boot_result$sd_ATE), linetype = "dashed") + 
  labs(title = "SD of ATE from 1000 Bootstraps in 100 Sub-Populations",
       subtitle = "Distribution of bias",
  caption = "Ideal center: 0", x = "SD of ATE", y = "Frequency") + 
  theme(
  plot.title = element_text(color = "blue", size = 11, face = "bold"),
  plot.subtitle = element_text(color = "black"),
  plot.caption = element_text(color = "orange", face = "italic")
  )


fig2 <- 
  boot_result %>%
  ggplot(aes(x = ATE)) + 
  geom_histogram(fill = "light blue", bins = 12, color = "black") +
  geom_density(aes(y = ..density..*2), colour = "red", 
               fill = "black", alpha = 0.3) + 
  geom_vline(xintercept = mean(boot_result$ATE), linetype = "dashed") +
  labs(title = "Distribution of ATE in 1000 Bootstraps of 100 Sub-Populations", 
       caption = "Ideal center: 0.15", x = "Average Treatment Effect", y = "Frequency") + 
  theme(
  plot.title = element_text(color = "blue", size = 11, face = "bold"),
  plot.caption = element_text(color = "orange", face = "italic")
  )

plot_grid(fig1, fig2)

rm(boot_estimates, boot_tib, df, matched, matched_df, matched_tib)

## Running Complex Bootstraps
source("./shared_code/boot_cont_complex.R")


## Generating Output
cont_simple_df <- 
  boot_result %>% 
  mutate(ci_lower = ATE - qnorm(0.975)*sd_ATE,
         ci_upper = ATE + qnorm(0.975)*sd_ATE,
         covered = case_when(
           ci_lower <= beta1 & ci_upper >= beta1 ~ 1,
                                            TRUE ~ 0
         ),
         boot_type = c(0),
         scenario_id = c(scenario_id), 
         empirical_mean = continuous_empirical_mean_se$empircal_mean,
         empirical_se = continuous_empirical_mean_se$empircal_se) %>% 
  relocate(-simp_perc_25, - simp_perc_975) %>% 
  tibble()

cont_complex_df <- 
  fin_estimate_df %>% 
  mutate(ci_lower = ATE - qnorm(0.975)*sd_ATE,
         ci_upper = ATE + qnorm(0.975)*sd_ATE,
         covered = case_when(
           ci_lower <= beta1 & ci_upper >= beta1 ~ 1,
                                            TRUE ~ 0
         ),
         boot_type = c(1),
         scenario_id = c(scenario_id), 
         empirical_mean = continuous_empirical_mean_se$empircal_mean,
         empirical_se = continuous_empirical_mean_se$empircal_se) %>% 
  relocate(-comp_perc_975, -comp_perc_25) %>% 
  tibble() %>% 
  relocate(seq)

to_combine_df <- bind_rows(cont_simple_df, cont_complex_df)

dataset_name <- paste0("cont_df_scen_", scenario_id)

save_command <- paste0('save(', dataset_name, ", file = './new_output_data/", dataset_name, ".RData')")

eval(parse(text = paste(dataset_name, "to_combine_df", sep = " <- ")), envir = .GlobalEnv)

eval(parse(text = save_command))

beep()
#save(cont_df_scen_7, file = "./output_data/continuous_scen_7.RData") # change object name and file name each run
```