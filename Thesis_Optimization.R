##############################################################################
# Script to calculate optimal combination of T and Psi (Maximize LEV) ########
##############################################################################

# Packages needed for analysis ################################################
library(ggplot2)
library(readxl)

# Download data and save the parameter values for the chosen case #############

# Adjust Parameter Values to Species Case and Scenario #
case <- "Beech_2"   # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

df <- read_excel(".../Data_Thesis.xlsx",  # Change to Working Directory with the Excel data sheet
                 sheet = "Scenario_2")                                          # Change the sheet to choose between scenario 1 and 2 

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# The timber price is differentiated between the two species. Only use either #
# for one calculation #########################################################

# Time-dependent timber price function (Beech)
Pf_function_beech <- function(t) {
  D_t <- 95.20 * (exp(0.00407 * t) - 1)                                        
  price <- -0.001483 * D_t^3 + 0.14113 * D_t^2 + 2.6173 * D_t + 115.374         
  return(price)
}

# Time-dependent timber price function (N. Spruce)
Pf_function_spruce <- function(t) {
  D_t <- 130.14 * (exp(0.00404 * t) - 1)                                       
  price <- 0.000262 * D_t^3 - 0.1416 * D_t^2 + 11.6177 * D_t + 8.9358           
  return(price)
}

# Conditional assignment based on species case
if (case %in% c("Beech_1", "Beech_2")) {
  Pf_function <- Pf_function_beech
} else {
  Pf_function <- Pf_function_spruce
}

# Functions for Optimization ##################################################
# 1. Forest Growth Pre-Thinning
y_volume <- function(VMax, g, b, t) {
  numerator <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
  denominator <- 1 - exp(-exp(g * b))
  VMax * (numerator / denominator)
}

# 2. Forest Growth Post-Thinning
x_volume <- function(par, VMax, g, b, theta) {
  T <- par[1]
  psi <- par[2]
  
  integrand <- function(t) {
    numerator <- VMax * g * exp(-g * (t - b)) * exp(-exp(-g * (t - b)))
    denominator <- 1 - exp(-exp(g * b))
    numerator / denominator
  }
  
  integral_result <- integrate(integrand, lower = T - psi, upper = T)$value
  
  yt <- y_volume(VMax, g, b, T - psi)
  
  x <- yt * (1 - theta) + integral_result
  return(x)
}

# 3. Time Derivative Forest Growth Pre-Thinning
y_prime <- function(VMax, g, b, t) {
  numerator <- VMax * g * exp(-g * (t - b)) * exp(-exp(-g * (t - b)))
  denominator <- 1 - exp(-exp(g * b))
  numerator / denominator
}

# 4. Time Derivative Forest Growth Post-Thinning
x_prime <- function(VMax, g, b, t) {
  numerator <- VMax * g * exp(-g * (t - b)) * exp(-exp(-g * (t - b)))
  denominator <- 1 - exp(-exp(g * b))
  numerator / denominator
}

# 5. Present Value Carbon 
PVc <- function(par, VMax, Pc, alpha, r, g, b, theta) {
  T <- par[1]
  psi <- par[2]
  if (psi <= 0 || psi >= T) return(-Inf)
  
  # Phase 1: Pre-Thinning
  integrand1 <- function(t) {
    Pc * alpha * y_prime(VMax, g, b, t) * exp(-r * t)
  }
  
  # Phase 2: Post-Thinning
  integrand2 <- function(t) {
    Pc * alpha * x_prime(VMax, g, b, t) * exp(-r * t)
  }
  
  I1 <- integrate(integrand1, lower = 0, upper = T - psi)$value
  I2 <- integrate(integrand2, lower = T - psi, upper = T)$value
  
  return(I1 + I2)
}

# 6. Present Value Timber Revenues (incl. Regenration Cost)
PVf <- function(par, VMax, Pc, g, b, theta, r, beta, Cf, alpha) {
  T <- par[1]
  psi <- par[2]
  
  first_cut <- y_volume(VMax, g, b, T - psi) * theta * exp(-r * (T - psi))
  harvest <- x_volume(c(T, psi), VMax, g, b, theta) * exp(-r * T)
  
  return((((Pf_function(T - psi) - Pc*beta*alpha) * first_cut) +
            ((Pf_function(T)       - Pc*beta*alpha) * harvest)) - Cf
  )
}


# 7. Land Expectation Value (Reparametrized for Constraints)
LEV <- function(par, VMax, Pc, alpha, r, g, b, theta, S, beta, Cf) {
  T <- par[1]
  rho <- par[2]
  psi <- rho * T
  
  # Avoid edge cases
  if (psi <= 0 || psi >= T) return(-Inf)
  
  pvc_val <- PVc(c(T, psi), VMax, Pc, alpha, r, g, b, theta)
  pvf_val <- PVf(c(T, psi), VMax, Pc, g, b, theta, r, beta, Cf, alpha)
  
  lev <- S + ((pvc_val + pvf_val) / (1 - exp(-r * T)))
  return(-lev)
}

# Optimization ################################################################
res <- optim(
  par = c(60, 0.5),                                                             # Start Values
  fn = function(par) LEV(par, VMax, Pc, alpha, 
                         r, g, b, theta, S, beta, Cf),
  method = "L-BFGS-B",
  lower = c(20, 0.01),                                                          # T in [20, 200], rho in [0.01, 0.99] - Rho as an ...
  upper = c(200, 0.99)
)

# Results #####################################################################
T_rotation <- floor(res$par[1])
rho <- res$par[2]
psi <- floor(rho * T_rotation)                                                  # Convert Rho back to Psi for final result

cat("Optimal T:", T_rotation, "\n")
cat("Optimal ψ:", psi, "\n")
cat("Max LEV:", -res$value, "\n")

