# #########
#
# Empirical Calculations
#
# #########

source("shared_code/setup.R")

r <- log(0.5/0.5)

before_a <- log((0.50/0.50)/(0.2/0.8))

q <- log(0.2/0.8)

beta0 <- function(prop_treat) {
  return((r - q - before_a*prop_treat)/2)
}

scenid <- c(1:6, 13:18)

empiricals <- tibble()

for (q in scenid) {

  scenario_id <- q
  
 all_scenarios <- tibble(
  id = c(1:18),
  n_sample = c(rep(1000, 6), rep(10000, 6), rep(100, 6)),
  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),3),
  beta1 = rep(c(0.767,  1.386294), 9),
  beta0 = rep(c(0.422, -1.069315, 0.46, -1.138629, 0.499, -1.207944), 3)
)

  desired_prop = all_scenarios %>% filter(id == scenario_id) %>% pull(desired_prop)
  alpha1   = log(1.25)
  alpha2   = log(1.75)
  beta0    = all_scenarios %>% filter(id == scenario_id) %>% pull(beta0)
  beta1    = all_scenarios %>% filter(id == scenario_id) %>% pull(beta1)
  beta2    = log(1.75)
  beta3    = log(1.25)
  m_sample = 100
  m_boot   = 500
  n_sample = all_scenarios %>% filter(id == scenario_id) %>% pull(n_sample)

source("./shared_code/data_gen_binary_updated.R")

df <- no_boot_list
matched_df <- list()

for (i in 1:length(df)) {
  
  matched <- matchit(A ~ L2 + L3, 
                     data = df[[i]], 
                     distance = "glm", 
                     link = "logit",
                     method = "nearest", 
                     ratio = 1) # perform NNM
  
  matched_df[[i]] <- match.data(matched, distance = "ps")
}

outcome_model_df <- function(df) {
  mod <- glm(Y ~ A + ps, 
             data = df, 
             weights = weights,
             family = "binomial"
  )
  
  sampl_all_treated <- 
    df %>%
    mutate(A = 1)
  
  sampl_all_untreated <- 
    df %>%
    mutate(A = 0)
  
  sampl_all_treated$pred.y <- 
    predict(mod, sampl_all_treated, type = "response")
  
  sampl_all_untreated$pred.y <- 
    predict(mod, sampl_all_untreated, type = "response")
  
  mean1 <- mean(sampl_all_treated$pred.y)
  mean0 <- mean(sampl_all_untreated$pred.y)
  ATE <- mean1 - mean0
  
  tib_coef <- tibble(estimate = mean(ATE), se = sd(ATE))
  return(tib_coef)
}

  # running glm function
  # adding progress bar for sanity
  binary_empirical_mean_se <- 
    tibble(data = matched_df) %>%
    mutate(
      scenario = q,
      outcoef = map(.x = data, ~outcome_model_df(.x))
    ) %>% 
    unnest(cols = outcoef) %>%  # preparing dataset to get estimates
    select(scenario, estimate, se)  %>% 
    group_by(scenario) %>%
    summarize(empirical_mean = mean(estimate),
              empirical_se = sd(estimate))

 empiricals <- bind_rows(empiricals, binary_empirical_mean_se)
}

binary_empiricals <- empiricals

save(binary_empiricals, file = './output_data/binary_empiricals.RData')

scenarios <- c(1:6, 13:18)

for (k in scenarios){

  load_dataset_name <- paste0("binary_scen_", k)

  load_command <- paste0("load(file = './new_output_data/", load_dataset_name, ".RData')")

  eval(parse(text = load_command))
  eval(parse(text = paste("current_dataset", load_dataset_name, sep = " <- ")))
  
  # current_dataset <- current_dataset %>% select(-empirical_mean, -empirical_se, -covered)

  # adding empiricals and re-evaluating covered
  
  if (k %% 2 == 0){
    modified_ds <- 
      current_dataset %>% 
      mutate(
        covered = (0.30 >= ci_lower) & (0.30 <= ci_upper)
      ) %>%
      left_join(binary_empiricals, on = c("scenario"))
     
  }
  if (k %% 2 != 0){
  modified_ds <- 
    current_dataset %>% 
    mutate(
      covered = (0.15 >= ci_lower) & (0.15 <= ci_upper)
    ) %>%
    left_join(binary_empiricals, on = c("scenario"))
    
  }
  
  save_command <- paste0('save(', load_dataset_name, ", file = './new_output_data/", load_dataset_name, ".RData')")
  
  eval(parse(text = paste(load_dataset_name, "modified_ds", sep = " <- ")))
  
  eval(parse(text = save_command))

}

