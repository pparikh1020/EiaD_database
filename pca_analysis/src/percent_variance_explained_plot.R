plot_variance_explained <- 
  function(sdev_obs_col) {

pc_eigenvalues <- (sdev_obs_col)^2
pc_eigenvalues <- tibble(PC = factor(1:length(pc_eigenvalues)), 
                         variance = pc_eigenvalues) %>% 
  mutate(pct = variance/sum(variance)*100) %>% 
  mutate(pct_cum = cumsum(pct))
pc_eigenvalues$PC <- as.numeric(pc_eigenvalues$PC)
pc_eigenvalues %>% 
  ggplot(aes(x = PC)) +
  geom_col(aes(y = pct)) +
  geom_line(aes(y = pct_cum, group = 1)) + 
  geom_point(aes(y = pct_cum)) +
  labs(x = "Principal component", y = "Fraction variance explained") + 
  scale_x_continuous(limits = c(0, 30))
}