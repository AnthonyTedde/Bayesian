library(magrittr)

################################################################################
#
# Beta prior -> Binomial Sampling -> Beta post.
#
# Peter D. Hoff (2009), A First Course In Bayesian Statistical Methods P58
# (Example Log-odds)
#
# General Social Survey: 
#
################################################################################

a <- 1; b <- 1
n <- 860
Y <- 441

##
# Theta prior: 
#  * unif = beta(1, 1)
##

set.seed(42)
theta_post_mc <- rbeta(10000, shape1 = Y + a, shape2 = n - Y + b)

ggplot2::ggplot(data = data.frame(x = theta_post_mc),
                ggplot2::aes(x)) +
  ggplot2::geom_density(color = "red") +
  ggplot2::stat_function(fun = dbeta, args = list(shape1 = Y + a,
                                                  shape2 = n - Y + b)) +
  ggplot2::stat_function(fun = dbeta, args = list(shape1 = a, shape2 = b),
                         color = "darkgreen")

# Without using Bayesian thinking, we know that the agreement to the ruling for
# people follow the binomial law. We have no idea about the parameter prod
# (see dbinom) but we have a sample at hand.

# Basically the Bayesian pior uniform is the same that saying "We have no clue"
# So we could skip this step and directly infer the parameter from sample.

# From the non protestant part of the sample, 860 people, 441 agreed. Those are 
# of interest. We therefore compute the statistic:

theta_freq <- 441 / 860

# To put into rbinom to construct monte carlo approximation:
# (size = 500) is set sufficiently large to have a bell curved distribution.

set.seed(42)
rsample_freq <- rbinom(10000, 500, prob = theta_freq)

# If we compare with the expected value of the bayesian statistic (posterior)
# to assess the parameter theta

set.seed(42)
rsample_bayes <- rbinom(10000, 500, prob = mean(theta_post_mc))

# Plot the result
list(frequentist = rsample_freq,
     bayesian = rsample_bayes) %>% 
  tibble::as_tibble() %>% 
  tidyr::pivot_longer(cols = c("frequentist", "bayesian")) %>% 
  ggplot2::ggplot(ggplot2::aes(x = value, color = name, fill = name)) +
  ggplot2::geom_bar(position = "dodge2")

# -> The same conslusion was brought from frequentist approach and bayesian for
# this trivial exemple.
