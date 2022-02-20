# complex bootstrap function for continuous outcomes

generate_boots <- function(df, iter = m_boot, seeds = seed_vec){
  pb3$tick()
  boot_samples <- list()
  matched_boot_df <- list()
  boot_ate <- tibble()
  
  for (i in 1:iter) {
    set.seed(seeds[[i]])
    
    boot_samples[[i]] <- sample_n(df, nrow(df), replace = TRUE)
    
    matched <- matchit(A ~ L2 + L3, 
                       data = boot_samples[[i]], 
                       distance = "glm", 
                       link = "logit",
                       method = "nearest", 
                       ratio = 1)
    
    matched_boot_df[[i]] <- match.data(matched, distance = "ps")
    
    bootmod <- glm(Y ~ A + ps, data = matched_boot_df[[i]], 
                   weights = weights)
    
    sum_bootmod <- summary(bootmod)
    
    estimate_val <- sum_bootmod$coef[2,1]
    to_bind_rows_estimate <- tibble(estimate = estimate_val)
    boot_ate <- bind_rows(boot_ate, to_bind_rows_estimate)
  }
  
  results <-
    tibble(
      ATE = mean(boot_ate$estimate),
      sd_ATE = sd(boot_ate$estimate),
      comp_perc_25 = quantile(boot_ate$estimate, probs = 0.025),
      comp_perc_975 = quantile(boot_ate$estimate, probs = 0.975)
    )
  
  return(results)
}

# running complex bootstrap

nb_tib <- tibble(nb = no_boot_list)

pb3 <- progress_bar$new(format = "bootstrapping... [:bar] :percent eta: :eta", total = nrow(nb_tib))

result_list <- nb_tib %>% mutate(res_tib = map(.x = nb, ~generate_boots(.x, iter = m_boot)))

fin_estimate_df <- 
  result_list %>% 
  unnest(res_tib) %>% 
  select(-nb) %>% 
  mutate(seq = c(1:nrow(nb_tib)))