# data gen file 

# this is the function that will generate all the data 
library(tidyverse)
library(simstudy)
library(MatchIt)
library(dplyr)
library(progress) 
library(ggpmisc)
library(cowplot)
library(beepr)

set.seed(1)


# here is all the decided upon parameters 

desired_prop = 0.14    # 0.1, 0.2, 0.3 (AKA alpha0)
alpha1   = log(1.25)
alpha2   = log(1.75)
beta0    = 0.437
beta1    = 0.767       # 0.75, 1.25
beta2    = log(1.75)
beta3    = log(1.25)
m_sample = 100        # 100
m_boot   = 500        # 500
n_sample = 10000      # 1000, 10000


seed_vec <- rnorm(100000, mean = 0, sd = 100) %>% round(0) %>% unique()

generate_no_boot_data <- function(n = m_sample, size = n_sample, seeds = seed_vec, 
                                  desired_prop, alpha1, alpha2, beta0, beta1, beta2,
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
    
    pre_data <- defData(pre_data, varname = "beta_error", formula = "0", variance = 1,
                dist = "normal")

    alpha0 = log(desired_prop/(1 - desired_prop))

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
                        formula = "beta0 + beta1*A + beta2*L2 + beta3*L3 " ,
                        dist = "binary", link = "logit")
    
    df[[i]] <- genData(size, pre_data)
    df[[i]] <- df[[i]] %>% select(-alpha0, -alpha1, -alpha2,
                                  -beta0, -beta1, -beta2, -beta3, -beta_error)
    }
  return(df)
}

no_boot_list <- generate_no_boot_data(n=m_sample, size = n_sample, seeds = seed_vec, 
                                      desired_prop, alpha1, alpha2, beta0, beta1, beta2,
                                  beta3)
