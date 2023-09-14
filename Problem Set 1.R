######################### Problem Set 1 #######################################


## Conduct a simple simulation study to explore how well OLS predicts as a function f the number of predcitors.

n = 50 ## Training Observations
N =  100 ## Tset Observations

p = seq(5, 45, by = 5) # Dimenstions (Variables)
R = 1000 ## Number of times

X <- matrix(nrow = n, ncol = 46)
Beta <- c(runif(46,0,5))

for (i in 1:n) {
  
  X[i,] <- c(1, rnorm(45))
  
}

Y <- X %*% Beta + rnorm(n)

training <- as.data.frame(cbind(Y,X))

X <- matrix(nrow = N, ncol = 46)

for (i in 1:N) {
  
  X[i,] <- c(1, rnorm(45))
  
}

Y <- X %*% Beta + rnorm(N)

test <- as.data.frame(cbind(Y,X))

counter <- 1

for(i in seq(5, 45, by=5)) {
  
  model_name <- paste("model_", counter, sep="")
  assign(model_name, lm(V1 ~ ., data = training[, c(1, 2:i)]))
  
  counter <- 1 + counter
  
}

counter <- 1

for(i in 0:8) {
  
  model_name <- paste("model_", counter, "_MSE",  sep ="")
  assign(model_name,(seq(1:1000)))

  
  counter <- counter + 1
  
}



f_hat <- predict(model_1, test)
model_1_MSE <- mean(test[,1] - f_hat)^2


## Rewrite the whole thing :)

MSE <- matrix(0, ncol = 9, nrow = 1000)
c <- 0

for (j in seq(5,45, by = 5)) {
  
  j <- j+1
  c <- c + 1
  
for (i in 1:1000) {
  
  X <- cbind(rep(1,50), matrix(rep(rnorm(45),50), nrow = 50, ncol = 45, byrow = FALSE))
  Y <- X %*% c(runif(46,0,5)) + rnorm(50)
  Training <- as.data.frame(cbind(Y,X))
  
  X <- cbind(rep(1,100), matrix(rep(rnorm(45),100), nrow = 100, ncol = 45))
  Y <- X %*% c(runif(46,0,5)) + rnorm(100)
  Test <- as.data.frame(cbind(Y,X))
  
  Model <- lm(V1 ~ ., data = Training[, 1:j])
  
  fhat <- predict(Model, newdata = Test)
  
  MSE[i,c] <- mean((Test[,1] - fhat)^2)
  
  }
  
}

summary(MSE)


