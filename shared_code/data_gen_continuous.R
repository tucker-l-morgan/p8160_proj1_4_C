# data gen file for continuous outcomes

set.seed(1)

# set up R script should be run prior to this
source("./shared_code/setup.R")

# a tibble of all scenarios to be tested
all_scenarios <- tibble(
  id = c(1:12),
  n_sample = c(rep(1000, 6), rep(10000, 6)),
  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),2),
  beta1 = rep(c(1, -1),6)
)

# list of parameters
desired_prop = 0.1 # 0.1, 0.2, 0.3 (AKA alpha0)
alpha1   = log(1.25)
alpha2   = log(1.75)
beta0    = 0
beta1    = 1       # 1, -1
beta2    = 2
beta3    = 1
m_sample = 100     # 100
m_boot   = 500     # 500
n_sample = 10000   # 1000, 10000

# creating vector of seeds from which to generate m samples
seed_vec <- rnorm(100000, mean = 0, sd = 100) %>% round(0) %>% unique()

# data gen function, continuous outcome
generate_no_boot_data <- function(n = m_sample, size = n_sample, seeds = seed_vec, 
                                  alpha1, alpha2, beta0, beta1, beta2,
                                  beta3, desired_prop) {
  
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
    
    alpha0 = log(desired_prop / (1 - desired_prop))
    
    pre_data <- defData(pre_data, varname = "alpha0", formula = alpha0)
    pre_data <- defData(pre_data, varname = "alpha1", formula = alpha1)
    pre_data <- defData(pre_data, varname = "alpha2", formula = alpha2)
    pre_data <- defData(pre_data, varname = "beta0", formula = beta0)
    pre_data <- defData(pre_data, varname = "beta1", formula = beta1)
    pre_data <- defData(pre_data, varname = "beta2", formula = beta2)
    pre_data <- defData(pre_data, varname = "beta3", formula = beta3)
  
    
    pre_data <- defData(pre_data, varname = "A", 
                        formula = "alpha0 + alpha1*L1 + alpha2*L2", 
                        dist = "binary", link = "logit")
    
    pre_data <- defData(pre_data, varname = "Y", 
                        formula = "beta0 + beta1*A + beta2*L2 + beta3*L3 + beta_error" ,
                        dist = "nonrandom")
    
    df[[i]] <- genData(size, pre_data)
    df[[i]] <- df[[i]] %>% select(-alpha0, -alpha1, -alpha2,
                                  -beta0, -beta1, -beta2, -beta3, -beta_error)
  }
  return(df)
}

no_boot_list <- generate_no_boot_data(n = m_sample, size = n_sample, seeds = seed_vec, 
                                      alpha1, alpha2, beta0, beta1, beta2, beta3, desired_prop)
