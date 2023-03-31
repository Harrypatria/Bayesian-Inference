



library(greta)

cormat <- round(cor(rock),2)
head(cormat)

x <- as_data(rock$peri)
y <- as_data(rock$perm)

beta0 <- uniform(-100, 100)
beta1 <- uniform(-100, 100)
sigma <- uniform(0, 1000)

head(x)

beta0


mu <- beta0 + beta1*x

distribution(y) <- normal(mu, sigma)

model_greta <- model(beta0, beta1, sigma)

plot(model_greta)


set.seed(4233)

draws_greta <- mcmc(model_greta, warmup = 1000,  n_samples = 1000)

summary(draws_greta)

bayesplot::mcmc_intervals(draws_greta)

bayesplot::mcmc_trace(draws_greta)

library(ggplot2)

draws_greta_df <- as.data.frame(draws_greta[[1]])

ggplot(draws_greta_df, aes(beta0, beta1, color = sigma)) + 
  geom_point(alpha = 0.3) + 
  geom_density2d(color = "gray30") + 
  scale_color_viridis_c(option = "plasma") + 
  labs(title = "greta parameter space")

ggplot(rock, aes(peri, perm)) + 
  geom_point() + 
  geom_abline(aes(intercept = beta0, slope = beta1), draws_greta_df, 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(draws_greta_df$beta1), 
              intercept = mean(draws_greta_df$beta0), 
              color = "blue", size = 1) + 
  labs(title = "Bayesian regression on rock data via greta")

############
ggplot(rock, aes(peri, perm)) + 
  geom_point() + 
  geom_abline(aes(intercept = beta0, slope = beta1), draws_greta_df, 
              alpha = 0.2, color = "lightblue") + 
  geom_abline(slope = mean(draws_greta_df$beta1), 
              intercept = mean(draws_greta_df$beta0), 
              color = "dark blue", size = 1) + 
  labs(title = "Bayesian regression on rock data via greta")

###########
ggplot(data = draws_greta_df,
       aes(beta0, beta1, color = "lightblue")) + 
  geom_abline(data = draws_greta_df %>% 
                slice_sample(n = 50),
              mapping = aes(intercept = beta0, 
                            slope = beta1),
              alpha = 0.3,
  geom_point() 
ggplot
