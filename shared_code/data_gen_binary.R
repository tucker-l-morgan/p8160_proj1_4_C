# data gen file 

# this is the function that will generate binary noboot data 

set.seed(20220217)

# here is all the decided upon parameters 

seed_vec <- runif(100000, min = 100, max = 99999999) %>% round(0) %>% unique()

generate_no_boot_data <- function(n=m_sample, size = n_sample, seeds = seed_vec, 
                                  desired_prop_treated, alpha1, alpha2, beta0, beta1, beta2,
                                  beta3) {
  
  df <- list()
  
  cov_df <- list()
  
  pb <- progress_bar$new(format = "generating data... [:bar] :percent eta :eta", total = n)
  
    for (i in 1:n) {
    pb$tick()
    set.seed(seeds[i])
    
    pre_data <- defData(varname = "L1", formula = "0", variance = 1,
                dist = "normal")
    
    pre_data <- defData(pre_data, varname = "L2", formula = "0", variance = 1,
                dist = "normal")
    
    pre_data <- defData(pre_data, varname = "L3", formula = "0", variance = 1,
                dist = "normal")
    
    pre_data <- defData(pre_data, varname = "beta_error", formula = "0", variance = 0.25,
                dist = "normal")

    alpha0 <- log(desired_prop_treated/(1 - desired_prop_treated))
  
    log(0.01/0.99)

    pre_data <- defData(pre_data, varname = "alpha0", formula = alpha0)
    pre_data <- defData(pre_data, varname = "alpha1", formula = alpha1)
    pre_data <- defData(pre_data, varname = "alpha2", formula = alpha2)
    pre_data <- defData(pre_data, varname = "beta0", formula = beta0)
    pre_data <- defData(pre_data, varname = "beta1", formula = beta1)
    pre_data <- defData(pre_data, varname = "beta2", formula = beta2)
    pre_data <- defData(pre_data, varname = "beta3", formula = beta3)
    
    pre_data <- defData(pre_data, varname = "A", 
                        formula = "alpha0 + alpha1*L1  + alpha2*L2", 
                        dist = "binary", link = "logit")
    
    pre_data <- defData(pre_data, varname = "Y", 
                        formula = "beta0 + beta1*A + beta2*L2 + beta3*L3 + beta_error" ,
                        dist = "binary", link = "logit")
    
    df[[i]] <- genData(size, pre_data)
    df[[i]] <- df[[i]] %>% select(-alpha0, -alpha1, -alpha2,
                                  -beta0, -beta1, -beta2, -beta3, -beta_error)
    }
  return(df)
}

# to generate more datasets, write wrapper function around this function
# beta_vec
# alpha_vec 

no_boot_list <- 
  generate_no_boot_data(
  n = m_sample, 
  size = n_sample, 
  seeds = seed_vec, 
  desired_prop, 
  alpha1, 
  alpha2, 
  beta0, 
  beta1, 
  beta2,
  beta3
  )


################ calculating empirical ##################

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
pb1 <- progress_bar$new(format = "glming... [:bar] :percent eta: :eta", total = length(matched_df))
binary_empirical_mean_se <- 
  tibble(data = matched_df) %>%
  mutate(
    outcoef = map(.x = data, ~outcome_model_df(.x))
  ) %>% 
  unnest(cols = outcoef) %>%  # preparing dataset to get estimates
  select(estimate, se)  %>% 
  summarize(empirical_mean = mean(estimate),
            empirical_se = sd(estimate))


###################### end empirical ####################### 

# creating a data frame will all the senarios 


# all_scenarios <- tibble(
#  id = c(1:12),
#  n_sample = c(rep(1000, 6), rep(10000, 6)),
#  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),2),
#  beta1 = rep(c(0.767, 1.25),6)
#)

