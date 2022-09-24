# function to generate sample of size n from distribution Expon(1/lambda)

generate_sample <- function(lambda=2, n=10000) {
  sample <- rexp(n, 1/lambda)
  return(sample)
}

# function to generate N samples of size n from distribution Expon(1/lambda)
generate_samples <- function(lambda=2, n=10000, N=100000) {
  samples <- matrix(nrow = N, ncol = n)
  
  for (i in 1:N)
    samples[i,] <- generate_sample(lambda, n)
  
  return(samples)
}

#------------------------------------------------------------------------------

# function to compute proportion of estimators 1/n * sum(Y_i) such that 
# |estimator - lambda| < epsilon for an arbitrarily small epsilon

proportion_1 <- function(lambda, epsilon, n, N) {
  # generate N samples of size n
  samples <- generate_samples(lambda, n, N)
  
  # compute the estimator for each sample
  estimator_vec <- vector(length = N)
  for (i in 1:N)
    estimator_vec[i] <- mean(samples[i,])
  
  # compute the absolute value of the difference between the estimator and 
  # the true lambda for each sample
  absolute_difference_vec <- vector(length = N)
  for (i in 1:N)
    absolute_difference_vec[i] <- abs(estimator_vec[i] - lambda)
  
  # compute the proportion of these samples such that the absolute 
  # difference < epsilon
  prop <- length(absolute_difference_vec[absolute_difference_vec < epsilon])/N
  
  return(prop)
}


# function to plot the proportion of estimators 1/n * sum(Y_i) such that 
# |estimator - lambda| < epsilon for an arbitrarily small epsilon against
# the sample size

plot_proportion_1 <- function(lambda, epsilon, n, N) {
  prop_vec <- vector(length = n)
  for(x in 1:n) 
    prop_vec[x] <- proportion_1(lambda, epsilon, x, N)
  
  plot(c(1:n), prop_vec, main = "Proportion vs Sample Size for ~lambda_1", sub 
       = "N = 1000, lambda = 0.01", xlab = "Sample Size", ylab = "Proportion")
}


#------------------------------------------------------------------------------

# function to compute proportion of estimators n*min(Y_1,...Y_n) such that 
# |estimator - lambda| < epsilon for an arbitrarily small epsilon

proportion_2 <- function(lambda, epsilon, n, N) {
  # generate N samples of size n
  samples <- generate_samples(lambda, n, N)
  
  # compute the estimator for each sample
  estimator_vec <- vector(length = N)
  for (i in 1:N)
    estimator_vec[i] <- n*(min(samples[i,]))
  
  # compute the absolute value of the difference between the estimator and 
  # the true lambda for each sample
  absolute_difference_vec <- vector(length = N)
  for (i in 1:N)
    absolute_difference_vec[i] <- abs(estimator_vec[i] - lambda)
  
  # compute the proportion of these samples such that the absolute 
  # difference < epsilon
  prop <- length(absolute_difference_vec[absolute_difference_vec < epsilon])/N
  
  return(prop)
}

# function to plot the proportion of estimators n*min(Y_1,...Y_n) such that 
# |estimator - lambda| < epsilon for an arbitrarily small epsilon against
# the sample size

plot_proportion_2 <- function(lambda, epsilon, n, N) {
  prop_vec <- vector(length = n)
  for(x in 1:n) 
    prop_vec[x] <- proportion_2(lambda, epsilon, x, N)
  
  plot(c(1:n), prop_vec, main = "Proportion vs Sample Size for ~lambda_2", sub
       = "N = 1000, lambda = 0.01", xlab = "Sample Size", ylab = "Proportion")
}

#------------------------------------------------------------------------------

# plot the proportions for ~lambda_1
plot_proportion_1(0.01, 0.001, 10000, 1000)

# plot the proportion for ~lambda_2
plot_proportion_2(0.01, 0.001, 10000, 1000)