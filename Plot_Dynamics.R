# Create Plot "Growth Dynamics" ##########################################

# Load Packages #################################################################
library(readxl)
library(dplyr)
library(ggplot2)

# Forest Growth #################################################################

y <- Vectorize(function(VMax, g, b, t) {
  numerator <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
  denominator <- 1 - exp(-exp(g * b))
  VMax * (numerator / denominator)
})

x <-  Vectorize(function(VMax, g, b, t) {
  numerator <- exp(-exp(-g * (t - b))) - exp(-exp(g * b))
  denominator <- 1 - exp(-exp(g * b))
  VMax * (numerator / denominator)
})

# Generate data for plotting with thinning #####################################
t_values <- seq(0, 200, by = 1)

# Compute growth values
# Load Data ###################################################################
df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

case <- "Beech_1" # "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

y_values <- sapply(t_values, function(t) y(VMax, g, b, t))

# Load Data ###################################################################
df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

case <- "N_Spruce_1" # "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

x_values <- sapply(t_values, function(t) x(VMax, g, b, t))

# Generate data for plotting no thinning #######################################
data_no_thinning <- data.frame(t = t_values, y = y_values, x = x_values)

# Plot Forest Growth 200 years  ################################################
ggplot() +
  # Before thinning
  geom_line(data = data_no_thinning, aes(x = t, y = y), size = 2, color = "green") +
  
  # After thinning
  geom_line(data = data_no_thinning, aes(x = t, y = x), size = 2, color = "blue") +
  
  # Styling
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 35),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_text(color = "black", size = 30),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  
  # Labels and axes
  labs(
    x = "t (years)",
    y = "Forest Volume (m^3/ha)",
    title = "Forest Growth Dynamics"
  ) +
  
  # Axis limits and ticks
  scale_x_continuous(limits = c(0, 200), breaks = seq(0, 200, by = 20), expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))

# Save Image ###################################################################
ggsave("Dynamics.png", width = 20, height = 10, dpi = 600)
