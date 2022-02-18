
df_plot <- tibble(
   treat_rate = c(rep("Large 0.1", 3), 
                  rep("Large 0.2", 3),
                  rep("Large 0.3", 3),
                  rep("small 0.1", 3),
                  rep("small 0.2", 3),
                  rep("samll 0.3", 3)),
   Method = rep(c("Empirical", "Simple", "Complex"),6),
   bias = c(0.1, -0.02, 0.15,
            0.2, 0.12, -0.05, 
            0.1, 0.04, -0.1, 
            0.1, 0.3 ,0.01, 
            0.05, 0.1, 0.2, 
            0.15, 0.1111, 0.21),
   sd_bias = c(0.01, 0.05, 0.08, 
               0.01, 0.01, 0.03, 
               0.01, 0.02, 0.05,
               0.01, 0.05, 0.08, 
               0.01, 0.01, 0.03, 
               0.01, 0.02, 0.05))
# plot the point plot
ggplot(df_plot, aes(x=bias, y=treat_rate, color=Method)) + 
  geom_point(position=position_dodge(0.3))+
  geom_errorbar(aes(xmin=bias-sd_bias, xmax=bias+sd_bias), width=.2,
               position=position_dodge(0.3)) +
  #theme(legend.position="none") +
  geom_vline(xintercept=0, linetype="dashed", color = "black") +
  labs(
   title = "Bias and Bias Standard Error rate of Simulations", 
   y = "Treatment Rate",
   x = "Bias"
  )+theme_bw()

