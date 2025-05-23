# Create Plot "Scenario 2 Growth" ##########################################

# Load Packages #################################################################
library(readxl)
library(dplyr)
library(ggplot2)

# Run the Optimization Code first (to get T and Psi) ############################

# Forest Growth #################################################################
y <- Vectorize(function(VMax, g, b, t) {
  numerator <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
  denominator <- 1 - exp(-exp(g * b))
  VMax * (numerator / denominator)
})

x <- function(VMax, g, b, t, theta, psi, T_rotation) {
  # BeforeThinning
  if (t <= (T_rotation - psi)) {
    return(y(VMax, g, b, t))
  }
  
  # After Thinning
  remaining_volume <- y(VMax, g, b, T_rotation - psi) * (1 - theta)
  
  integrand <- function(t_inner) {
    num1 <- g * exp(-g * (t_inner - b)) * exp(-exp(-g * (t_inner - b)))
    den1 <- exp(-exp(-g * (t_inner - b))) - exp(-exp(g * b))
    
    num2 <- exp(-exp(-g * (t_inner - b))) - exp(-exp(g * b))
    den2 <- 1 - exp(-exp(g * b))
    
    rate <- num1 / den1
    volume <- num2 / den2
    
    rate * VMax * volume
  }
  
  integral_result <- integrate(integrand, lower = T_rotation - psi, upper = t)$value
  
  xt_result <- remaining_volume + integral_result
  return(xt_result)
}

y_prime <- Vectorize(function(VMax, g, b, t) {   
  numerator <- VMax * g * exp(-g * (t - b)) * exp(-exp(-g * (t - b)))
  denominator <- 1 - exp(-exp(g * b))
  numerator / denominator
})  

x_prime <- Vectorize(function(VMax, g, b, t, theta, psi, T_rotation) {
  t_switch <- T_rotation - psi
  
  if (t < t_switch) {
    return(y_prime(VMax, g, b, t))
  } else {
    # exakte Ableitung des Integranden (aus deinem neuen x_volume)
    num1 <- g * exp(-g * (t - b)) * exp(-exp(-g * (t - b)))
    den1 <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
    
    num2 <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
    den2 <- 1 - exp(-exp(g * b))
    
    rate <- num1 / den1
    volume <- num2 / den2
    
    return(rate * VMax * volume)
  }
})

# Income from Carbon Sequestration ##############################################
PVc_up_to_t <- function(t, T_rotation, psi, VMax, Pc, alpha, r, g, b, theta) {
  if (psi < 0 || psi > T_rotation) return(0)
  
  t_switch <- T_rotation - psi  # boundary between phase 1 and phase 2
  
  if (t <= 0) return(0)
  
  # Phase 1 only
  if (t <= t_switch) {
    integrand1 <- function(t) {
      Pc * alpha * y_prime(VMax, g, b, t) * exp(-r * t)
    }
    return(integrate(integrand1, lower = 0, upper = t)$value)
  }
  
  # Phase 1 + Phase 2 (t > T - psi)
  integrand1 <- function(t) {
    Pc * alpha * y_prime(VMax, g, b, t) * exp(-r * t)
  }
  integrand2 <- function(t) {
    Pc * alpha * x_prime(VMax, g, b, t, theta, psi, T_rotation) * exp(-r * t)
  }
  
  I1 <- integrate(integrand1, lower = 0, upper = t_switch)$value
  I2 <- integrate(integrand2, lower = t_switch, upper = min(t, T_rotation))$value
  
  return(I1 + I2)
}

# Income from Timber + Tax on Released Carbon ###################################
PVf_up_to_t <- function(VMax, g, b, theta, psi, T_rotation, Pc, beta, Cf, r, t, alpha) {
  if (t < (T_rotation - psi)) {
    return(-Cf)  # Only cost in early years
    
  } else if (t == (T_rotation - psi)) {
    # Add thinning income
    Pf_thinning <- Pf_function(T_rotation - psi)
    thinning_income <- (Pf_thinning - Pc * beta * alpha) * (y(VMax, g, b, T_rotation - psi) * exp(-r * (T_rotation - psi)))
    return(-Cf + thinning_income)
    
  } else if (t > (T_rotation - psi) & t < T_rotation) {
    # Value remains the same after thinning
    Pf_thinning <- Pf_function(T_rotation - psi)
    thinning_income <- (Pf_thinning - Pc * beta * alpha) * (y(VMax, g, b, T_rotation - psi) * exp(-r * (T_rotation - psi)))
    return(-Cf + thinning_income)
    
  } else if (t == T_rotation) {
    # Add final harvest revenue at t = T
    Pf_final <- Pf_function(T_rotation)
    final_harvest <- (Pf_final - Pc * beta * alpha) * (x(VMax, g, b, t, theta, psi, T_rotation) * exp(-r * T_rotation))
    Pf_thinning <- Pf_function(T_rotation - psi)
    thinning_income <- (Pf_thinning - Pc * beta * alpha) * (y(VMax, g, b, T_rotation - psi) * exp(-r * (T_rotation - psi)))
    return(-Cf + thinning_income + final_harvest)
    
  } else {
    Pf_final <- Pf_function(T_rotation)
    final_harvest <- (Pf_final - Pc * beta * alpha) * (x(VMax, g, b, t, theta, psi, T_rotation) * exp(-r * T_rotation))
    Pf_thinning <- Pf_function(T_rotation - psi)
    thinning_income <- (Pf_thinning - Pc * beta * alpha) * (y(VMax, g, b, T_rotation - psi) * exp(-r * (T_rotation - psi)))
    return(-Cf + thinning_income + final_harvest)
  }
}

# Generate data for plotting with thinning #####################################
t_values <- seq(0, 200, by = 1)

# Load Data ###################################################################
df <- read_excel("/Users/lukashasler/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni Copenhagen/Jahr 2/Thesis/Data_final.xlsx",
                 sheet = "Scenario_2")

case <- "Beech_1" # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# Compute PVc(t) for different time steps  ####################################
time_values <- seq(0, T_rotation, by = 1)  
PVc_values <- sapply(time_values, function(t) {
  PVc_up_to_t(t, T_rotation, psi, VMax, Pc, alpha, r, g, b, theta)
})

data_pvc <- data.frame(
  Time = time_values,
  PVc = PVc_values
)


# Compute PVf(t) for different time steps #####################################
PVf_values <- sapply(time_values, function(t){ 
  PVf_up_to_t(VMax, g, b, theta, psi, T_rotation, Pc, beta, Cf, r, t, alpha)
})

# Create a data frame
PVf_df <- data.frame(
  Time = time_values, 
  PVf = PVf_values
)

combined <- merge(data_pvc, PVf_df, by = "Time")
combined$Net <- combined$PVc+combined$PVf

#
y_min <- floor((-Cf - 10) / 1000) * 1000    
y_max <- ceiling(max(combined$Net, combined$PVc, combined$PVf) / 1000) * 1000  # next higher 1000

# Plot with ggplot2 ###########################################################
ggplot() +
  # Before thinning
  geom_line(data = combined, aes(x = Time, y = PVc), size = 2, color = "blue") +
  
  # After thinning
  geom_line(data = combined, aes(x = Time, y = PVf), size = 2, color = "green") +
  
  # Thinning event marker
  geom_line(data = combined, aes(x = Time, y = Net), size = 2, color = "darkgrey") +
  
  # Styling
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 35),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 25),
    axis.title = element_text(color = "black", size = 30),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  
  # Labels and axes
  labs(
    x = "t (years)",
    y = "Income (DKK)",
    title = "Beech Income First Rotation (Suitable Site)"
  ) +
  
  # Axis limits and ticks
  scale_x_continuous(limits = c(0, T_rotation), breaks = seq(0, T_rotation, by = 20), expand = c(0, 0)) +
  scale_y_continuous(limits = c(y_min, y_max), breaks = scales::pretty_breaks(n = 5),expand = c(0, 0))

ggsave("Beech22_income.png", width = 20, height = 10, dpi = 600)
