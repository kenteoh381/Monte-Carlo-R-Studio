library(data.table, dplyr, ggplot2, stargazer,fixest)
##Parameters and Seed 
beta_0 = 1.5 #Intercept 
beta_1 = 2.0 # Slope 
set.seed(1) #Seed 
n = 251 # sample size 
M = 500 # Number of Experiments/Iterations 

## Storage 
Slope_DT <- rep(0,M)
Intercept_DT <- rep(0,M)

# Begin Monte Carlo 
for (i in 1:M) { # M is the number of iterations 
  
  #Generate Data 
  U_i = rnorm(n, mean = 0, sd = 2) # Error
  X_i = rnorm(n, mean = 5, sd = 5) # Independent Variable 
  Y_i = beta_0 + beta_1 + U_i # Dependent Variable 
  
  # Formulate data.table 
  data_i = data.table (X = X_i, Y = Y_i)
  
  # Run regressions 
  #Extract slope coefficient and save 
  print(i)
  Slope_DT[i] <- ols_i$coefficients[2]
  Intercept_DT[i] <- ols_i$coefficients[1]
}

#Summary statistics 
estimates_DT <- data.table(beta_1 = Slope_DT, beta_0 = Intercept_DT)
stargazer(estimates_DT[, c("beta_1", "beta_0")], type = "text")

#visual inspection 
hist(estimates_DT[,beta_1], xlim = c(1.5,2.5))
