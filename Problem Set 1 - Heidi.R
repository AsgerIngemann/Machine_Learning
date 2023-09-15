###############################################################################
########################## Problem Set 1 ######################################
###############################################################################

############################# Problem 1 #######################################
# In this exercise we conduct a simple simulation study to explore how well
# OLS predicts as a function of the number of predictors.

# Min kode

n <- 50
p <- seq(5,45, by = 5)

for (i in p){
  b1 <- 
  beta <- runif((i + 1), 0, 5)
  return(beta)
}

for (i in p){
  dgp = function(n, p, beta = runif((p+1),0,5), mu_x = rep(0, p), Sigma_x = diag(rep(1,p)), sigma_eps = 1){
    
    # Libraries and functions:
    require(mvtnorm)         # For multivariate normal.
    
    # Drawing predictors as multivariate normal:
    X <- rmvnorm(n, mean = mu_x, sigma = Sigma_x)
    
    # Noise is also normal:
    eps <- rnorm(n, 0, sigma_eps)
    
    # Outcome:
    Y <- cbind(rep(1, n), X) %*% beta + eps
    
    # Data:
    data <- data.frame(Y, X)
    return(data)
  }
}

data

# Agsers kode

MSE <- matrix(0, ncol = 9, nrow = 1000)
c <- 0

for (j in seq(5,45, by = 5)) {
  
  j <- j+1
  c <- c + 1
  
  for (i in 1:1000) {
    
    X <- cbind(rep(1,50), matrix(rep(rnorm(45),50), nrow = 50, ncol = 45, 
                                 byrow = FALSE))
    Y <- X %*% runif(46,0,5) + rnorm(50)
    Training <- as.data.frame(cbind(Y,X))
    
    X <- cbind(rep(1,100), matrix(rnorm(45*100), nrow = 100, ncol = 45))
    Y <- X %*% runif(46,0,5) + rnorm(100)
    
    Test <- as.data.frame(cbind(Y,X))
    
    Model <- lm(V1 ~ ., data = Training[, 1:j])
    
    fhat <- predict(Model, newdata = Test)
    
    MSE[i,c] <- mse(Y, fhat)
    
  }
  
}

summary(MSE)

colors <- c("#00AFBB", "#E7B800")

plot(Y, fhat, col = colors)

legend("bottomright", legend = c("Actual", "Predicted"),
       col =  c("#00AFBB", "#E7B800"),
       pch = c(16, 17, 18) )
