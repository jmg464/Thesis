# Create Plot "Scenario 1 Growth" ##########################################

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

# Generate data for plotting with thinning #####################################
t_values <- seq(0, 200, by = 1)

# Load Data ###################################################################
df <- read_excel("/Users/lukashasler/Library/Mobile Documents/com~apple~CloudDocs/Documents/Uni Copenhagen/Jahr 2/Thesis/Data_final.xlsx",
                 sheet = "Scenario_1")

case <- "Beech_1" # "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

x_values <- sapply(t_values, function(t) x(VMax, g, b, t, theta, psi, T_rotation))

# Store in a dataframe
data_growth <- data.frame(t = t_values, x = x_values)

# Multiple cycla dataframe #####################################################
# Step 1: Cut data at rotation time
df_cut <- data_growth %>%
  filter(t <= T_rotation)

# Step 2: Define time shift
time_shift <- T_rotation + 1

# Step 3: Create second rotation
df_shifted_2 <- df_cut
df_shifted_2$t <- df_shifted_2$t + time_shift
df_shifted_2$rotation <- 2

# Step 4: Create third rotation
df_shifted_3 <- df_cut
df_shifted_3$t <- df_shifted_3$t + 2 * time_shift
df_shifted_3$rotation <- 3

# Step 4: Create third rotation
df_shifted_4 <- df_cut
df_shifted_4$t <- df_shifted_4$t + 3 * time_shift
df_shifted_4$rotation <- 4

# Step 4: Create third rotation
df_shifted_5 <- df_cut
df_shifted_5$t <- df_shifted_5$t + 4 * time_shift
df_shifted_5$rotation <- 5

# Step 5: Tag the first rotation
df_cut$rotation <- 1

# Step 6: Combine all
df_combined <- bind_rows(df_cut, df_shifted_2, df_shifted_3, df_shifted_4, df_shifted_5)

# Plot Forest Growth multiple cycles ########################################################
ggplot() +
  
  geom_line(data = df_combined, aes(x = t, y = x), size = 2, color = "blue") +
  
  # Rotation length (T) as a vertical red dashed line
  geom_segment(aes(x = T_rotation, y = 0, xend = T_rotation, yend = max(y_values, x_values)),
               color = "red", linetype = "dashed", size = 2) +
  
  # Rotation length (2T) as a vertical red dashed line
  geom_segment(aes(x = 2*T_rotation+1, y = 0, xend = 2*T_rotation+1, yend = max(y_values, x_values)),
               color = "red", linetype = "dashed", size = 2) +
  # Rotation length (2T) as a vertical red dashed line
  geom_segment(aes(x = 3*T_rotation+2, y = 0, xend = 3*T_rotation+2, yend = max(y_values, x_values)),
               color = "red", linetype = "dashed", size = 2) +
  
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
    y = "Forest Volume (m^3/ha)",
    title = "Norway Spruce Growth Over Multiple Cycles (Optimal Site)"
  ) +
  
  # Axis limits and ticks
  scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 20), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

ggsave("NSpruce11.png", width = 20, height = 10, dpi = 600)
