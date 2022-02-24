# compiling continuous data, producing visualizations
pos_beta <- 1
neg_beta <- -1

load("./new_output_data/cont_df_scen_1.RData")
load("./new_output_data/cont_df_scen_2.RData")
load("./new_output_data/cont_df_scen_3.RData")
load("./new_output_data/cont_df_scen_4.RData")
load("./new_output_data/cont_df_scen_5.RData")
load("./new_output_data/cont_df_scen_6.RData")
load("./new_output_data/cont_df_scen_13.RData")
load("./new_output_data/cont_df_scen_14.RData")
load("./new_output_data/cont_df_scen_15.RData")
load("./new_output_data/cont_df_scen_16.RData")
load("./new_output_data/cont_df_scen_17.RData")
load("./new_output_data/cont_df_scen_18.RData")

cont_all_scenarios <- tibble(
  id = c(1:18),
  n_sample = c(rep(1000, 6), rep(10000, 6), rep(100, 6)),
  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),3),
  beta1 = rep(c(1, -1),9)
)

# compiling continuous data
continuous_final_odd <- 
  cont_df_scen_1 %>% mutate(n_sample = 1000, beta1 = pos_beta, desired_prop = 0.1) %>% 
  bind_rows(cont_df_scen_3 %>% mutate(n_sample = 1000, beta1 = pos_beta, desired_prop = 0.2) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>%
  bind_rows(cont_df_scen_5 %>% mutate(n_sample = 1000, beta1 = pos_beta, desired_prop = 0.3) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  bind_rows(cont_df_scen_13 %>% mutate(n_sample = 100, beta1 = pos_beta, desired_prop = 0.1) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975)) %>% 
  bind_rows(cont_df_scen_15 %>% mutate(n_sample = 100, beta1 = pos_beta, desired_prop = 0.2) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  bind_rows(cont_df_scen_17 %>% mutate(n_sample = 100, beta1 = pos_beta, desired_prop = 0.3) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  mutate(
    ATE_bias = ATE - pos_beta,
    empirical_bias = empirical_mean - pos_beta,
    boot_type = ifelse(boot_type == 0, "Simple", "Complex")
  ) 

rm(cont_df_scen_1, cont_df_scen_3, cont_df_scen_5, cont_df_scen_13, cont_df_scen_15, cont_df_scen_17)

continuous_final_even <- 
  cont_df_scen_2 %>% 
  mutate(n_sample = 1000, beta1 = neg_beta, desired_prop = 0.1) %>% 
  mutate(
    perc_25 = case_when(
      boot_type == 0 ~ simp_perc_25,
      boot_type == 1 ~ comp_perc_25),
    perc_975 = case_when(
      boot_type == 0 ~ simp_perc_975,
      boot_type == 1 ~ comp_perc_975
    )
  ) %>% 
  select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975) %>% 
  bind_rows(cont_df_scen_4 %>% mutate(n_sample = 1000, beta1 = neg_beta, desired_prop = 0.2) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  bind_rows(cont_df_scen_6 %>% mutate(n_sample = 1000, beta1 = neg_beta, desired_prop = 0.3) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  bind_rows(cont_df_scen_14 %>% mutate(n_sample = 100, beta1 = neg_beta, desired_prop = 0.1) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975)) %>% 
  bind_rows(cont_df_scen_16 %>% mutate(n_sample = 100, beta1 = neg_beta, desired_prop = 0.2) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  bind_rows(cont_df_scen_18 %>% mutate(n_sample = 100, beta1 = neg_beta, desired_prop = 0.3) %>% 
              mutate(
                perc_25 = case_when(
                  boot_type == 0 ~ simp_perc_25,
                  boot_type == 1 ~ comp_perc_25),
                perc_975 = case_when(
                  boot_type == 0 ~ simp_perc_975,
                  boot_type == 1 ~ comp_perc_975
                )
              ) %>% 
              select(-simp_perc_25, -simp_perc_975, -comp_perc_25, -comp_perc_975)) %>% 
  mutate(
    ATE_bias = ATE - neg_beta,
    empirical_bias = empirical_mean - neg_beta,
    boot_type = ifelse(boot_type == 0, "Simple", "Complex")
  ) 

rm(cont_df_scen_2, cont_df_scen_4, cont_df_scen_6, cont_df_scen_14, cont_df_scen_16, cont_df_scen_18)

continuous_final <-
  continuous_final_odd %>%
  bind_rows(continuous_final_even)

rm(continuous_final_even, continuous_final_odd)

# continuous coverage rates
cr_df_cont <- 
  continuous_final %>%
  mutate(scenario = factor(scenario_id),
         new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
         treat_effect = ifelse(beta1 == pos_beta, "True ATE = 1", "True ATE = -1"),
         Method = factor(boot_type, levels = c("Simple", "Complex"))) %>%  
  group_by(new_name, treat_effect, Method) %>%
  summarize(cr = sum(covered)/ 100) 

cont_cvg_plot <- 
  ggplot(cr_df_cont, aes(x=cr, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.5), size = 3)+
  geom_vline(xintercept=0.9927, linetype="dashed", color = "black") +
  geom_vline(xintercept=0.9072, linetype="dashed", color = "black") +
  geom_vline(xintercept=0.95, linetype="dashed", color = "black") +
  facet_grid(~treat_effect) + 
  labs(
    title = "Continuous Coverage Rates by Parameters of Interest", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "CI Coverage Rate"
  ) +
  theme_bw() +
  theme(text = element_text(size = 16))

# continuous bias
empirical_data_cont <- 
  continuous_final %>%
  mutate(
    scenario = factor(scenario_id),
    new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
    treat_effect = ifelse(beta1 == pos_beta, "True ATE = 1", "True ATE = -1")
  ) %>% 
  select(scenario, new_name, treat_effect, empirical_bias, empirical_se) %>%
  unique() %>% 
  rename(
    ATE_bias  = empirical_bias,
    bias_se = empirical_se
  ) %>%
  mutate(
    Method = "Empirical"
  )

boot_data_cont <- 
  continuous_final %>%
  mutate(scenario = factor(scenario_id),
         new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
         treat_effect = ifelse(beta1 == pos_beta, "True ATE = 1", "True ATE = -1"),
         Method = factor(boot_type)
  ) %>% 
  group_by(Method, scenario, new_name, treat_effect ) %>%
  summarize(
    ATE_bias = mean(ATE_bias),
    bias_se = mean(sd_ATE)
  ) 

cont_bias_plot <- 
  bind_rows(boot_data_cont, empirical_data_cont) %>%
  mutate(
    Method = factor(Method, levels = c("Simple", "Complex", "Empirical"))
  ) %>% 
  ggplot(aes(x=ATE_bias, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.7), size = 3)+
  geom_errorbar(aes(xmin=ATE_bias-1.96*bias_se, xmax=ATE_bias+1.96*bias_se), width=.2,
                position=position_dodge(0.7), size = 1) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  facet_grid(~treat_effect) +
  labs(
    title = "Continuous Simulation Bias and Standard Error CI", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "Bias"
  ) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  scale_color_manual(values = c("#F8766D", "#00B9E3", "#00BA38"))

cont_se_plot <- 
  bind_rows(boot_data_cont, empirical_data_cont) %>%
  mutate(
    Method = factor(Method, levels = c("Simple", "Complex", "Empirical"))
  ) %>% 
  ggplot(aes(x=bias_se, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.7), size = 3)+
  facet_grid(~treat_effect) +
  labs(
    title = "Continuous Simulation Standard Error", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "Standard Error"
  ) +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  scale_color_manual(values = c("#F8766D", "#00B9E3", "#00BA38"))

rm(boot_data_cont, empirical_data_cont, cr_df_cont, continuous_final)