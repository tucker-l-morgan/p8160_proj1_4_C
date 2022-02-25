# compiling binary data, producing visualizations
smaller_true_ATE <- 0.15
bigger_true_ATE <- 0.3

# loading data
load("./new_output_data/binary_scen_1.RData")
load("./new_output_data/binary_scen_2.RData")
load("./new_output_data/binary_scen_3.RData")
load("./new_output_data/binary_scen_4.RData")
load("./new_output_data/binary_scen_5.RData")
load("./new_output_data/binary_scen_6.RData")
load("./new_output_data/binary_scen_13.RData")
load("./new_output_data/binary_scen_14.RData")
load("./new_output_data/binary_scen_15.RData")
load("./new_output_data/binary_scen_16.RData")
load("./new_output_data/binary_scen_17.RData")
load("./new_output_data/binary_scen_18.RData")

bin_all_scenarios <- tibble(
  id = c(1:18),
  n_sample = c(rep(1000, 6), rep(10000, 6), rep(100, 6)),
  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),3),
  beta1 = rep(c(0.767, 1.587),9),
  beta0 = rep(c(0.422, 0.873, 0.46, 0.952, 0.499, 1.032), 3)
)

# combining data
binary_final_odd <- 
  binary_scen_1 %>% mutate(n_sample = 1000, beta1 = 0.767, desired_prop = 0.1) %>% 
  bind_rows(binary_scen_3 %>% mutate(n_sample = 1000, beta1 = 0.767, desired_prop = 0.2)) %>%
  bind_rows(binary_scen_5 %>% mutate(n_sample = 1000, beta1 = 0.767, desired_prop = 0.3)) %>%
  bind_rows(binary_scen_13 %>% 
              filter(boot_type != 1) %>% 
              mutate(n_sample = 100, beta1 = 0.767, desired_prop = 0.1)) %>%
  bind_rows(binary_scen_15 %>% mutate(n_sample = 100, beta1 = 0.767, desired_prop = 0.2)) %>%
  bind_rows(binary_scen_17 %>% mutate(n_sample = 100, beta1 = 0.767, desired_prop = 0.3))

binary_final_odd <- binary_final_odd %>% 
  mutate(
    ATE_bias = ATE - smaller_true_ATE,
    empirical_bias = empirical_mean - smaller_true_ATE,
    boot_type = ifelse(boot_type == 0, "Simple", "Complex")
  ) 

rm(binary_scen_1, binary_scen_3, binary_scen_5, binary_scen_13, binary_scen_15, binary_scen_17)

binary_final_even <- 
  binary_scen_2 %>% mutate(n_sample = 1000, beta1 = 1.587, desired_prop = 0.1) %>% 
  bind_rows(binary_scen_4 %>% mutate(n_sample = 1000, beta1 = 1.587, desired_prop = 0.2)) %>%
  bind_rows(binary_scen_6 %>% mutate(n_sample = 1000, beta1 = 1.587, desired_prop = 0.3)) %>%
  bind_rows(binary_scen_14 %>% mutate(n_sample = 100, beta1 = 1.587, desired_prop = 0.1)) %>%
  bind_rows(binary_scen_16 %>% mutate(n_sample = 100, beta1 = 1.587, desired_prop = 0.2)) %>%
  bind_rows(binary_scen_18 %>% mutate(n_sample = 100, beta1 = 1.587, desired_prop = 0.3))


binary_final_even <- binary_final_even %>% 
  mutate(
    ATE_bias = ATE - bigger_true_ATE,
    empirical_bias = empirical_mean - bigger_true_ATE,
    boot_type = ifelse(boot_type == 0, "Simple", "Complex")
  ) 

rm(binary_scen_2, binary_scen_4, binary_scen_6, binary_scen_14, binary_scen_16, binary_scen_18)

binary_final <- binary_final_even %>% bind_rows(binary_final_odd)

rm(binary_final_odd, binary_final_even)

# calculating coverage rates and producing plots
cr_df<- binary_final %>%  
  mutate(scenario = factor(scenario),
         #new_name = str_c("sample = ", n_sample, ", treat prop = ", desired_prop),
         new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
         treat_effect = ifelse(beta1 == 0.767, "True ATE = 0.15", "True ATE = 0.3"),
         Method = factor(boot_type, levels = c("Simple", "Complex", "Empirical"))) %>%  
  group_by(new_name, treat_effect, Method) %>%
  summarize(cr = sum(covered)/ 100) 

bin_cvg_plot <- 
  ggplot(cr_df, aes(x=cr, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.5), size = 3)+
  geom_vline(xintercept=0.9927, linetype="dashed", color = "black") +
  geom_vline(xintercept=0.9072, linetype="dashed", color = "black") +
  geom_vline(xintercept=0.95, linetype="dashed", color = "black") +
  facet_grid(~treat_effect) + 
  labs(
    title = "Binary Coverage Rates by Parameters of Interest", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "CI Coverage Rate"
  ) + 
  theme_bw() + 
  theme(text = element_text(size = 16))

# bias plots
empirical_data <- binary_final %>%
  mutate(
    scenario = factor(scenario),
    new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
    treat_effect = ifelse(beta1 == 0.767, "True ATE = 0.15", "True ATE = 0.3")
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

boot_data <- binary_final %>%
  mutate(scenario = factor(scenario),
         new_name = str_c("(", n_sample, ", ", desired_prop, ")"),
         treat_effect = ifelse(beta1 == 0.767, "True ATE = 0.15", "True ATE = 0.3"),
         Method = boot_type
  ) %>% 
  group_by(Method, scenario, new_name, treat_effect ) %>%
  summarize(
    ATE_bias = mean(ATE_bias),
    bias_se = mean(sd_ATE)
  ) 

bin_bias_plot <- 
  bind_rows(boot_data,empirical_data ) %>%
  mutate(
    Method = factor(Method, levels = c("Simple", "Complex", "Empirical"))
  ) %>% 
  ggplot( aes(x=ATE_bias, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.7), size = 3)+
  geom_errorbar(aes(xmin=ATE_bias-1.96*bias_se, xmax=ATE_bias+1.96*bias_se), width=.2,
                position=position_dodge(0.7), size = 1) +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  facet_grid(~treat_effect) +
  labs(
    title = "Binary Simulation Bias and Standard Error CI", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "Bias"
  ) + 
  theme_bw() +
  theme(text = element_text(size = 16)) +
  scale_color_manual(values = c("#F8766D", "#00B9E3", "#00BA38"))

# standard error plot
bin_se_plot <- 
  bind_rows(boot_data,empirical_data ) %>%
  mutate(
    Method = factor(Method, levels = c("Simple", "Complex", "Empirical"))
  ) %>% 
  ggplot( aes(x=bias_se, y=new_name, color=Method)) + 
  geom_point(position=position_dodge(0.7), size = 3)+
  facet_grid(~treat_effect) +
  labs(
    title = "Binary Simulation Standard Error", 
    y = "Scenario (Sample Size, Proportion Treated)",
    x = "Standard Error"
  )  +
  theme_bw() +
  theme(text = element_text(size = 16)) +
  scale_color_manual(values = c("#F8766D", "#00B9E3", "#00BA38"))

rm(boot_data, empirical_data, cr_df, binary_final)