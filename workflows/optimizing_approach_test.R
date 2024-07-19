#install.packages("sn")
library(sn)

# Set parameters for the skew normal distribution
xi <- 1.0     # Location parameter (mean)
omega <- 1.0   # Scale parameter (standard deviation)
alpha <- 20  # Shape parameter (controls skewness)

# Generate random numbers from the skew normal distribution
set.seed(1001) # For reproducibility
data <- rsn(n = 10000, xi = xi, omega = omega, alpha = alpha)

# Plot the histogram of the generated data
hist(data, breaks = 30, probability = TRUE, col = "lightblue", 
     main = "Histogram of Skew Normal Distribution", xlab = "Value")

# Add the density curve
x_vals <- seq(min(data), max(data), length.out = 100)
density_vals <- dsn(x_vals, xi = xi, omega = omega, alpha = alpha)
lines(x_vals, density_vals, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Density Curve"), col = "red", lwd = 2)


# quantiles
quantile(x_vals, probs = c(0.05, 0.5, 0.95))


####
library(sn)
library(optimx)

# Objective function to find the optimal xi, omega, and alpha
objective_function <- function(params) {
  xi <- params[1]
  omega <- params[2]
  alpha <- params[3]
  
  # Calculate the quantiles for the given parameters
  q5 <- qsn(0.05, xi = xi, omega = omega, alpha = alpha)
  q50 <- qsn(0.50, xi = xi, omega = omega, alpha = alpha)
  q95 <- qsn(0.95, xi = xi, omega = omega, alpha = alpha)
  
  # Desired quantiles
  desired_q5 <- 2.0
  desired_q50 <- 3.0
  desired_q95 <- 5.0
  
  # Calculate the squared differences
  error <- (q5 - desired_q5)^2 + (q50 - desired_q50)^2 + (q95 - desired_q95)^2
  return(error)
}


# Initial guesses for xi, omega, and alpha
initial_params <- c(1, 1, 1)

# Perform the optimization
result <- optimx(par = initial_params, 
                 fn = objective_function, 
                 method = "L-BFGS-B", 
                 lower = c(-10, 0.1, -10), 
                 upper = c(10, 10, 10))

# Extract the optimal parameters
optimal_params <- result$par
optimal_xi <- optimal_params[1]
optimal_omega <- optimal_params[2]
optimal_alpha <- optimal_params[3]

print(optimal_params)

#########################
# Objective function to find the optimal xi, omega, and alpha
objective_function <- function(params) {
  xi <- params[1]
  omega <- params[2]
  alpha <- params[3]
  
  # Calculate the quantiles for the given parameters
  q5 <- qsn(0.05, xi = xi, omega = omega, alpha = alpha)
  q50 <- qsn(0.50, xi = xi, omega = omega, alpha = alpha)
  q95 <- qsn(0.95, xi = xi, omega = omega, alpha = alpha)
  
  # Desired quantiles
  desired_q5 <- 2.0
  desired_q50 <- 3.0
  desired_q95 <- 5.0
  
  # Calculate the squared differences
  error <- (q5 - desired_q5)^2 + (q50 - desired_q50)^2 + (q95 - desired_q95)^2
  
  # Print intermediate values for debugging
  cat("xi:", xi, "omega:", omega, "alpha:", alpha, "error:", error, "\n")
  
  return(error)
}

# Initial guesses for xi, omega, and alpha
initial_params <- c(3, 1, 0)

# Perform the optimization
result <- optimx(par = initial_params, 
                 fn = objective_function, 
                 method = "L-BFGS-B", 
                 lower = c(-10, 0.1, -20), 
                 upper = c(10, 10, 20))

# Check the result
print(result)

# Extract the optimal parameters
optimal_params <- result[which.min(result$value), 1:3]
optimal_xi <- optimal_params[1]
optimal_omega <- optimal_params[2]
optimal_alpha <- optimal_params[3]

print(optimal_params)

# Set parameters
xi <- optimal_xi
omega <- optimal_omega
alpha <- optimal_alpha

# Generate random data with the optimal parameters
set.seed(123)
data <- rsn(n = 10000, xi = 1.95, omega = 1.56, alpha = 8.82)

# Calculate the quantiles
quantiles <- quantile(data, probs = c(0.05, 0.5, 0.95))

# Print the results
print(quantiles)

# Plot the histogram and density curve
hist(data, breaks = 30, probability = TRUE, col = "lightblue", 
     main = "Histogram of Optimized Skew Normal Distribution", xlab = "Value")

x_vals <- seq(min(data), max(data), length.out = 100)
density_vals <- dsn(x_vals, xi = 1.95, omega = 1.56, alpha = 8.82)
lines(x_vals, density_vals, col = "red", lwd = 2)
legend("topright", legend = c("Density Curve"), col = "red", lwd = 2)

