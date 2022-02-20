# #####
#
# Binary Simulation RScript
#
# Waveley Qiu
# 
# ###

source("./shared_code/setup.R")

## Call Function to Generate No Boot Data

### Set Parameters

scenario_id <- 2

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

source("./shared_code/data_gen_binary.R")

## Simple Bootstrap

### Nearest Neighbors Matching

df <- no_boot_list
pb4 <- progress_bar$new(format = "nearest neighbor matching... [:bar] :percent eta: :eta",
                        total = length(df))

matched_df <- list()

for (i in 1:length(df)) {
  pb4$tick()
  matched <- matchit(A ~  L2 + L3, 
                     data = df[[i]], 
                     distance = "glm", 
                     link = "logit",
                     method = "nearest", 
                     ratio = 1) # perform NNM
  
  matched_df[[i]] <- match.data(matched, distance = "ps") 
  
}

### Simple Bootstrap

#### Create SBS Function
# creating the tibble to apply map function
matched_tib <-
  tibble(data = matched_df)

# ### function to iterate glm over a list, to be used in purr:map ###
# returns tibble of parameter estimates and standard errors.

outcome_model_list <- function(list) {
  tib_coef <- tibble()
  boots <- tibble(mean1 = NA,
                  mean0 = NA,
                  difference = NA)
  pb3$tick()
  for (i in 1:length(list)) {
    mod <- glm(Y ~ A + ps, 
               data = list[[i]], 
               weights = weights, 
               family = "binomial")
    
    sampl_all_treated <- 
      list[[i]] %>%
      mutate(A = 1)
    
    sampl_all_untreated <- 
      list[[i]] %>%
      mutate(A = 0)
    
    sampl_all_treated$pred.y <- 
      predict(mod, sampl_all_treated, type = "response")
    
    sampl_all_untreated$pred.y <- 
      predict(mod, sampl_all_untreated, type = "response")
    
    boots[i, "mean1"] <- mean(sampl_all_treated$pred.y)
    boots[i, "mean0"] <- mean(sampl_all_untreated$pred.y)
    boots[i, "difference"] <- boots[i, "mean1"] - boots[i, "mean0"]
  }
  return(boots)
}

#### Running SBS

simple_boot <- function(df, n = m_boot, size = df %>% pull(A) %>% sum(), seeds = seed_vec){
  boots <- list()
  pb2$tick()
  for (i in 1:n) {
    set.seed(seeds[i])
    boots[[i]] <- 
      df %>% 
      filter(subclass %in% sample(levels(subclass), 
                                  size, 
                                  replace =  TRUE))
  }
  return(boots)
}

# adding progress bars for sanity

pb2 <- progress_bar$new(format = "bootstrapping... [:bar] :percent eta: :eta",
                        total = length(df))
pb3 <- progress_bar$new(format = "outcome modeling... [:bar] :percent eta: :eta",
                        total = length(df))

# creating booted tibbles, applying functions through purr:map.
boot_tib <- 
  matched_tib %>% 
  mutate(
    boots = map(.x = data, ~simple_boot(.x))
  ) %>%
  mutate(ATE = map(.x = boots, ~outcome_model_list(.x)))

## Unnesting bootstrap coefficients

boot_estimates <- 
  boot_tib %>% 
  mutate(seq = seq(1:nrow(boot_tib))) %>% 
  select(ATE, seq) %>% 
  unnest(ATE)

### Simple Boot Results

simple_boot_result <-
  boot_estimates %>% 
  group_by(seq) %>% 
  summarize(ATE = mean(difference), 
            sd_ATE = sd(difference),
            perc_25 = quantile(difference, probs = 0.025),
            perc_975 = quantile(difference, probs = 0.975),
            ci_lower = ATE + qnorm(0.025)*sd_ATE,
            ci_upper = ATE - qnorm(0.025)*sd_ATE,
            covered = ci_lower <= 0.15 & 0.15 <= ci_upper,
            boot_type = 0,
            scenario = scenario_id
  )

### Intermediate Step, Remove Obsolete Datasets

rm(boot_estimates, boot_tib, df, matched, matched_df, matched_tib)

## Complex Bootstrap

### Writing Bootstrap Function

generate_boots <- function(df, iter = m_boot, seeds = seed_vec){
  
  pb3$tick()
  boot_samples <- list()
  matched_boot_df <- list()
  boot_results <- tibble()
  
  for (i in 1:iter) {
    set.seed(seeds[[i]])
    
    boot_samples[[i]] <- sample_n(df, nrow(df), replace = TRUE)
    
    matched <- matchit(A ~ L2 + L3, data = boot_samples[[i]], 
                       distance = "glm", link = "logit",
                       method = "nearest", ratio = 1)
    
    matched_boot_df[[i]] <- match.data(matched, distance = "ps")
  }
  
  return(matched_boot_df)
}


### Run Complex Bootstrap

nb_tib <- tibble(
  seq = seq(1, length(no_boot_list)),
  nb = no_boot_list
)

pb3 <- progress_bar$new(format = "bootstrapping... [:bar] :percent eta: :eta",
                        total = length(nb_tib$nb))
booted_tib <- nb_tib %>% 
  mutate(
    complex_boot = map(.x = nb, ~generate_boots(.x))
  )

pb3 <- progress_bar$new(format = "calculating outcomes.. [:bar] :percent eta: :eta",
                        total = length(nb_tib$nb))
result_list <- booted_tib %>% 
  mutate(
    outcomes = map(.x = complex_boot, ~outcome_model_list(.x))
  )

complex_boot_estimates <- result_list %>% unnest(outcomes)

complex_boot_result <-
  complex_boot_estimates %>% 
  group_by(seq) %>% 
  summarize(ATE = mean(difference), 
            sd_ATE = sd(difference),
            perc_25 = quantile(difference, probs = 0.025),
            perc_975 = quantile(difference, probs = 0.975),
            ci_lower = ATE + qnorm(0.025)*sd_ATE,
            ci_upper = ATE - qnorm(0.025)*sd_ATE,
            covered = ci_lower <= 0.15 & 0.15 <= ci_upper,
            boot_type = 1,
            scenario = scenario_id
  )

## Put it All Together


to_output_bin_ds <- bind_rows(simple_boot_result, complex_boot_result)

#to_output_bin_ds <- bind_cols(to_output_bin_ds, binary_empirical_mean_se)

#to_output_bin_ds <- bind_cols(simple_boot_result, binary_empirical_mean_se)

output_dataset_name <- paste0("binary_scen_", scenario_id)

save_command <- paste0('save(', output_dataset_name, ", file = './output_data/", output_dataset_name, ".RData')")

eval(parse(text = paste(output_dataset_name, "to_output_bin_ds", sep = " <- ")))

eval(parse(text = save_command))

beep("fanfare")



