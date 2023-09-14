######################### Problem Set 1 #######################################




dgp = function(n, p, beta = rep(1, (p+1)), mu_x = rep(0, p), Sigma_x = diag(rep(1,p)), sigma_eps = 1){
  
  # Libraries and functions:
  require(mvtnorm)         # For multivariate normal.
  
  # Drawing predictors as multivariate normal:
  X = rmvnorm(n, mean = mu_x, sigma = Sigma_x)
  
  # Noise is also normal:
  eps = rnorm(n, 0, sigma_eps)
  
  # Outcome:
  Y = cbind(rep(1, n), X) %*% beta + eps
  
  # Data:
  data = data.frame(Y, X)
  return(data)
}