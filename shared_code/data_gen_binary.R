# data gen file 

# this is the function that will generate binary noboot data 

set.seed(20220217)


# here is all the decided upon parameters 

desired_prop = 0.1 # 0.2, 0.3 (AKA alpha0)
alpha1   = log(1.25)
alpha2   = log(1.75)
beta0    = 0
beta1    = 0.75 #1.25
beta2    = log(1.75)
beta3    = log(1.25)
m_sample = 1000
m_boot   = 500
n_sample = 100 # 1000


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
   # df[[i]] <- df[[i]] %>% select(-alpha0, -alpha1, -alpha2,
    #                              -beta0, -beta1, -beta2, -beta3, -beta_error)
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



# creating a data frame will all the senarios 


all_scenarios <- tibble(
  n_sample = c(rep(1000, 6), rep(10000, 6)),
  desired_prop = rep(c(0.1, 0.1, 0.2, 0.2, 0.3, 0.3),2),
  beta1 = rep(c(0.75, 1.75),6)
)
