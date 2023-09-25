###############################################################################
########################## Problem Set 1 ######################################
###############################################################################

library(forecast)
library(glmnet)
library(MLmetrics)

############################# Problem 1 #######################################
# In this exercise we conduct a simple simulation study to explore how well
# OLS predicts as a function of the number of predictors.

set.seed(1)

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


############################ Problem 2 ########################################
# In this exercise we conduct a simulation study that explores how OLS, ridge
# and LASSO do when we vary the number of predictors with zero coefficients.

set.seed(1)

n <- 50
N <- 100
p <- 45
R <- 30
p_z <- c(0, 10, 20, 30, 40)


# OLS
## Training the model using OLS
olsBeta <- function(YTrain, XTrain){
  olsBetaHat <- solve(t(XTrain) %*% XTrain) %*% (t(XTrain) %*% YTrain)
  return(olsBetaHat)
}

## Estimating the Y-values of OLS
olsEstimation <- function(olsBetaHat, XTest){
  YHat <- XTest %*% olsBetaHat
  return(YHat)
}

## Simulating the data
DataSimulator <- function(size, p, beta){
  X <- cbind(rep(1, times = size), matrix(rnorm(size * p, 0, 1), size, p))
  eps <- rnorm(size, 0, 1)
  Y <- X %*% beta + eps
  
  return(list(X = X, Y = Y))
}
GetData <- function(p, p_z){
  beta <- runif(p + 1, 0.2, 1)
  
  Sample <- sample(2:(p+1), p_z)
  for (i in Sample){
    beta[i] <- 0
  }
  
  TrainingData <- DataSimulator(n, p, beta)
  TestingData <- DataSimulator(N, p, beta)
  
  return(list(TestingData = TestingData, TrainingData = TrainingData))
}

## MSE
MeanSquaredError <- function(YHat, YTest){
  mse <- mean((YHat - YTest)^2 )
  return(mse)
}

## Conducting the test
olsStudy <- function(Data){
  
  # Training OLS
  olsBetaHat <- olsBeta(Data$TrainingData$Y, Data$TrainingData$X)
  # Estimation OLS
  olsYHat <- olsEstimation(olsBetaHat, Data$TestingData$X)
  
  # Tests
  mseOls <- MeanSquaredError(olsYHat, Data$TestingData$Y)
  
  return(mseOls)
}


# Ridge and LASSO

## Choosing the tuning parameter
lambda <- function(XTrain, YTrain){
  lambda_grid <- 10^seq(10, -2, length = 100)
  cv_out <- cv.glmnet(XTrain, YTrain, alpha = 0, nfolds = 3, lambda = lambda_grid)
  lambda <- cv_out$lambda.min
  
  return(lambda)
}


## Ridge Regression
RidgeEstimation <- function(XTrain, YTrain){
  lambda <- lambda(XTrain, YTrain)
  RidgeEst <- glmnet(XTrain, YTrain, alpha = 0, lambda = lambda)
  
  return(RidgeEst)
}

RidgeStudy <- function(Data){
  
  # Training
  RidgeEstimation <- RidgeEstimation(Data$TrainingData$X, Data$TrainingData$Y)
  
  # Estimation
  RidgePrediction <- as.vector(predict(RidgeEstimation, newx = Data$TestingData$X))
  
  # Test
  mseRidge <- MSE(RidgePrediction, Data$TestingData$Y)
  
  return(mseRidge) 
}


## LASSO Regression
lassoEstimation <- function(XTrain, YTrain){
  lambda <- lambda(XTrain, YTrain)
  lassoEst <- glmnet(XTrain, YTrain, alpha = 1, lambda = lambda)
  
  return(lassoEst)
}

lassoStudy <- function(Data){
  
  # Training
  lassoEstimation <- lassoEstimation(Data$TrainingData$X, Data$TrainingData$Y)
  
  # Estimation
  lassoPrediction <- as.vector(predict(lassoEstimation, newx = Data$TestingData$X))

  # Test
  mseLasso <- MSE(lassoPrediction, Data$TestingData$Y)
  
  return(mseLasso) 
}

# Simulation study
SimulationStudy <- function(){
  
  Results <- data.frame(matrix(nrow = length(p_z), ncol = 3), row.names = p_z)
  idx <- 1
  for (z in p_z){
    
    StudyResultsOls <- rep(0, R)
    StudyResultsRidge <- rep(0, R)
    StudyResultsLasso <- rep(0, R)
    
    for (r in 1:R){
      
      Data <- GetData(p, z)
      
      StudyResultsOls[r] <- olsStudy(Data)
      StudyResultsRidge[r] <- RidgeStudy(Data)
      StudyResultsLasso[r] <- lassoStudy(Data)
    }
    
    olsMean <- mean(StudyResultsOls)
    lassoMean <- mean(StudyResultsLasso)
    RidgeMean <- mean(StudyResultsRidge)
    
    Results[idx,] <- c(olsMean, RidgeMean, lassoMean)
    idx <- idx + 1
    
  }
  
  colnames(Results)<- c('OLS', 'Ridge', 'Lasso')
  
  return(Results)
}

SimulationStudy()

############################## Problem 5 ######################################
