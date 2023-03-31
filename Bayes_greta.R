
#  simple linear regression of a single variable
lm(mpg ~ wt, mtcars)

library(ggplot2)
theme_set(hrbrthemes::theme_ipsum_rc())

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# THE TOOLS

# 1. STAN


# 2. GRETA
#install.packages("greta")
#install_greta_deps()
library(greta)

x <- as_data(mtcars$wt)
y <- as_data(mtcars$mpg)

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

draws_greta_df <- as.data.frame(draws_greta[[1]])

ggplot(draws_greta_df, aes(beta0, beta1, color = sigma)) + 
  geom_point(alpha = 0.3) + 
  geom_density2d(color = "gray30") + 
  scale_color_viridis_c(option = "plasma") + 
  labs(title = "greta parameter space")

ggplot(mtcars, aes(wt, mpg)) + 
  geom_point() + 
  geom_abline(aes(intercept = beta0, slope = beta1), draws_greta_df, 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(draws_greta_df$beta1), 
              intercept = mean(draws_greta_df$beta0), 
              color = "blue", size = 1) + 
  labs(title = "Bayesian regression on mtcars via greta")


cormat <- round(cor(mtcars),2)
head(cormat)

ggplot(mtcars, aes(mpg, qsec)) + 
  geom_point() + 
  geom_abline(aes(intercept = beta0, slope = beta1), draws_greta_df, 
              alpha = 0.1, color = "gray50") + 
  geom_abline(slope = mean(draws_greta_df$beta1), 
              intercept = mean(draws_greta_df$beta0), 
              color = "blue", size = 1) + 
  labs(title = "Bayesian regression on mtcars via greta")











###################
model_stan <- data {
  int<lower=0> n;
  vector[n] x;
  vector[n] y;
  }

parameters {
  real beta0;
  real beta1;
  real<lower=0> sigma;
  }

model {
  y ~ normal(beta0 + beta1*x, sigma);
  }


library(rstan)

draws_stan <- sampling(model_stan, 
                       data = list(n = nrow(mtcars),
                                   x = mtcars$wt, 
                                   y = mtcars$mpg),
                       seed = 2132,
                       algorithm = "HMC",
                       chains = 1)