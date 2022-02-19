# simple bootstrap function for continuous outcomes

## Implementing Nearest-Neighbor Matching
df <- no_boot_list

# could possible turn this into a function later.
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

## The Simple Bootstrap

# creating the tibble to apply map function
matched_tib <-
  tibble(data = matched_df)

# finding the empirical estimates for all the matched data sets. 
#########################################
outcome_model_df <- function(df) {
  pb1$tick()
    mod <- glm(Y ~ A + ps, 
               data = df, 
               weights = weights
               #family = "binomial"
               ) %>% 
      summary()
    coefs <- mod$coefficients[2,1:2]
    tib_coef <- tibble(estimate = coefs[1], se = coefs[2])
    return(tib_coef)
}

# running glm function
# adding progress bar for sanity
pb1 <- progress_bar$new(format = "glming... [:bar] :percent eta: :eta", total = nrow(matched_tib))
continuous_empirical_mean_se <- 
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
  pb3$tick()
  for (i in 1:length(list)) {
    mod <- glm(Y ~ A + ps, 
               data = list[[i]], 
               weights = weights) %>% summary()
    coefs <- mod$coefficients[2,1:2]
    tib_coef <- bind_rows(tib_coef, tibble(estimate = coefs[1], se = coefs[2]))
  }
  return(tib_coef)
}

# ### input matched dataframe, output however many bootstrapped samples you want ###

# now define function
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
pb2 <- progress_bar$new(format = "bootstrapping... [:bar]", total = nrow(matched_tib))
pb3 <- progress_bar$new(format = "performing glm... [:bar]", total = nrow(matched_tib))

# creating booted tibbles, applying functions through purr:map.
boot_tib <- 
  matched_tib %>% 
  mutate(
    boots = map(.x = data, ~simple_boot(.x, n = m_boot))
  ) %>%
  mutate(coef = map(.x = boots, ~outcome_model_list(.x)))

boot_estimates <- 
  boot_tib %>% 
  mutate(seq = seq(1:nrow(boot_tib))) %>% 
  select(coef, seq) %>% unnest(coef)