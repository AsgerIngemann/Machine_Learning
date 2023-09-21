###############################################################################
########################## Problem Set 1 ######################################
###############################################################################

############################# Problem 1 #######################################
# In this exercise we conduct a simple simulation study to explore how well
# OLS predicts as a function of the number of predictors.

# Konstants
n <- 50
N <- 100
P <- seq(5,45, by = 5)
R <- 1000

# Training the model
olsBeta <- function(YTrain, XTrain){
  BetaHat <- solve(t(XTrain) %*% XTrain) %*% (t(XTrain) %*% YTrain)
  return(BetaHat)
}

# Estimating the Y-values
LinearPredictor <- function(BetaHat, XTest){
  YHat <- XTest %*% BetaHat
  return(YHat)
}

# Testing the model
MeanSquaredError <- function(YHat, YTest){
  mse <- mean((YHat - YTest)^2 )
  return(mse)
}

# Simulating the data
DataSimulator <- function(size, p, beta){
  X <- cbind(rep(1, times = size), matrix(rnorm(size * p, 0, 1), size, p))
  eps <- rnorm(size, 0, 1)
  Y <- X %*% beta + eps
  
  return(list(X = X, Y = Y))
}

GetData <- function(p){
  beta <- runif(p + 1, 0, 5)
  
  TrainingData <- DataSimulator(n, p, beta)
  TestingData <- DataSimulator(N, p, beta)
  
  return(list(TestingData = TestingData, TrainingData = TrainingData))
}

# Conducting the test
Study <- function(p){
  Data <- GetData(p)
  # Training
  BetaHat <- olsBeta(Data$TrainingData$Y, Data$TrainingData$X)
  # Estimation
  YHat <- LinearPredictor(BetaHat, Data$TestingData$X)
  # Test
  mse <- MeanSquaredError(YHat, Data$TestingData$Y)
  
  return(mse)
}

# Simulation study
SimulationStudy <- function(){

  StudyResults <- rep(0, length(P))
  for (p in P){
    
    PResults <- rep(0, R)
    for (r in 1:R){
      PResults[r] <- Study(p)
    }
    
    StudyResults[p/5] <- mean(PResults)
  }
  
  return(StudyResults)
}