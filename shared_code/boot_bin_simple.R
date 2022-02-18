
## Simple Bootstrap

### Nearest Neighbors Matching

df <- no_boot_list
pb4 <- progress_bar$new(format = "bootstrapping... [:bar] :percent eta: :eta",
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

# finding the empirical estimates for all the matched data sets. 
#########################################
outcome_model_df <- function(df) {
  pb1$tick()
    mod <- glm(Y ~ A + ps, 
               data = df, 
               weights = weights,
               family = "binomial"
               ) %>% 
      summary()
    coefs <- mod$coefficients[2,1:2]
    tib_coef <- tibble(estimate = coefs[1], se = coefs[2])
    return(tib_coef)
}

# running glm function
# adding progress bar for sanity
pb1 <- progress_bar$new(format = "glming... [:bar] :percent eta: :eta", total = nrow(matched_tib))
binary_empirical_mean_se <- 
  matched_tib %>% 
  mutate(
    outcoef = map(.x = data, ~outcome_model_df(.x))
    ) %>% 
  unnest(cols = outcoef) %>%  # preparing dataset to get estimates
  select(estimate, se)  %>% 
  summarize(empircal_se = sd(estimate), 
            empircal_mean = mean(estimate))

##################################

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

simple_boot <- function(df, n = m_boot, size = n_sample*desired_prop, seeds = seed_vec){
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
pb3 <- progress_bar$new(format = "bootstrapping... [:bar] :percent eta: :eta",
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


### Summary of 1000 bootstrap of 100 no boot

simple_boot_result <-
  boot_estimates %>% 
  group_by(seq) %>% 
  summarize(ATE = mean(difference), 
            sd_ATE = sd(difference),
            ci_lower = ATE - qnorm(0.025)*sd_ATE,
            ci_upper = ATE - qnorm(0.025)*sd_ATE,
            covered = ci_lower <= 0.767 & 0.767 <= ci_upper,
            boot_type = 0,
            scenario = "test"
  )

