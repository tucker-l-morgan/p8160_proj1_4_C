# data gen file 

# this is the function that will generate binary noboot data 

# expit function

expit <- function(x){
  res <- exp(x)/(1 + exp(x))
  return(res)
}

set.seed(20220217)

seed_vec <- runif(100000, min = 100, max = 99999999) %>% round(0) %>% unique()

generate_no_boot_data <- function(n=m_sample, 
                                  size = n_sample, 
                                  seeds = seed_vec, 
                                  desired_prop_treated, 
                                  alpha1, 
                                  alpha2, 
                                  beta0, 
                                  beta1, 
                                  beta2,
                                  beta3) {
  
  df <- list()
  
  cov_df <- list()
  
  pb <- progress_bar$new(format = "generating data... [:bar] :percent eta :eta", total = n)
  
  for (i in 1:n) {
    pb$tick()
    set.seed(seeds[i])
    long_rnorm <- rnorm(size*3, mean = 0, sd = 1)
    L1 <- long_rnorm[1:size]
    L2 <- long_rnorm[(size + 1):(2*size)]
    L3 <- long_rnorm[(2*size + 1):(3*size)]
    
    beta_error <- rnorm(size, mean = 0, sd = 0.25)
    alpha0 <- log(desired_prop_treated/(1 - desired_prop_treated))
    logit_p <- alpha0 + alpha1*L1 + alpha2*L2
    p <- expit(logit_p)
    
    long_runif <- runif(size*2)
    
    collated_tib <- tibble(
      L1 = L1,
      L2 = L2,
      L3 = L3, 
      prob_A = p,
      comp_pA = long_runif[1:size],
    )
    
    A_tib <- 
      collated_tib %>% mutate(
        A = (prob_A > comp_pA) %>% as.numeric()
      ) %>%
      select(-prob_A, -comp_pA)
    
    df[[i]] <- A_tib %>%
      mutate(
        logit_y = beta0 + beta1*A + beta2*L2 + beta3*L3 + beta_error,
        y = expit(logit_y),
        comp_pY = long_runif[(size + 1):(2*size)],
        Y = (y > comp_pY) %>% as.numeric()
      )
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









