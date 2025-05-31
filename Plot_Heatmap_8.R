library(cowplot)
library(patchwork)
library(openxlsx)
library(readxl)

max_LEV_points <- data.frame(
  Scenario = character(),
  T = numeric(),
  psi = numeric(),
  LEV = numeric(),
  stringsAsFactors = FALSE
)

case <- "Beech_1"  

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

# Time-dependent timber price function
Pf_function <- function(t) {
  D_t <- 95.20 * (exp(0.00407 * t) - 1)              # Diameter at time t
  price <- -0.001483*D_t^3 + 0.14113*D_t^2 + 2.6173*D_t+ 115.374                      # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p1 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Beech Optimal Scenario 1",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "Beech_2"  

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

# Time-dependent timber price function
Pf_function <- function(t) {
  D_t <- 95.20 * (exp(0.00407 * t) - 1)              # Diameter at time t
  price <- -0.001483*D_t^3 + 0.14113*D_t^2 + 2.6173*D_t+ 115.374                      # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p2 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Beech Suitable Scenario 1",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "N_Spruce_1"   

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

Pf_function <- function(t) {
  D_t <- 130.14 * (exp(0.00404 * t) - 1)              # Diameter at time t
  price <- 0.000262*D_t^(3)-0.1416*D_t^2+11.6177*D_t+8.9358                  # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p3 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Norway Spruce Optimal Scenario 1",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "N_Spruce_2"  

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_1")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

Pf_function <- function(t) {
  D_t <- 130.14 * (exp(0.00404 * t) - 1)              # Diameter at time t
  price <- 0.000262*D_t^(3)-0.1416*D_t^2+11.6177*D_t+8.9358                  # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p4 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Norway Spruce Suitable Scenario 1",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "Beech_1"   # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_2")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

# Time-dependent timber price function
Pf_function <- function(t) {
  D_t <- 95.20 * (exp(0.00407 * t) - 1)              # Diameter at time t
  price <- -0.001483*D_t^3 + 0.14113*D_t^2 + 2.6173*D_t+ 115.374                      # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p5 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Beech Optimal Scenario 2",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "Beech_2"   # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_2")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

# Time-dependent timber price function
Pf_function <- function(t) {
  D_t <- 95.20 * (exp(0.00407 * t) - 1)              # Diameter at time t
  price <- -0.001483*D_t^3 + 0.14113*D_t^2 + 2.6173*D_t+ 115.374                      # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p6 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Beech Suitable Scenario 2",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "N_Spruce_1"   # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_2")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

Pf_function <- function(t) {
  D_t <- 130.14 * (exp(0.00404 * t) - 1)              # Diameter at time t
  price <- 0.000262*D_t^(3)-0.1416*D_t^2+11.6177*D_t+8.9358                  # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p7 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Norway Spruce Optimal Scenario 2",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

case <- "N_Spruce_2"   # "Beech_1"   "Beech_2"   "N_Spruce_1"   "N_Spruce_2"

df <- read_excel(".../Data_final.xlsx",
                 sheet = "Scenario_2")

param_values <- df[[case]]
param_names <- df$Parameters

for (i in seq_along(param_names)) {
  assign(param_names[i], param_values[i], envir = .GlobalEnv)
}

# 1. Finer grid of T and rho values
T_values <- seq(1, 200, by = 1)
psi_values <- seq(1, 199, by = 1)
LEV_grid <- expand.grid(T = T_values, psi = psi_values) %>%
  filter(psi < T)

Pf_function <- function(t) {
  D_t <- 130.14 * (exp(0.00404 * t) - 1)              # Diameter at time t
  price <- 0.000262*D_t^(3)-0.1416*D_t^2+11.6177*D_t+8.9358                  # Price at that diameter
  return(price)
}

LEV_grid$LEV <- mapply(function(T, psi) {
  rho <- psi / T
  -LEV(c(T, rho), VMax, Pc, alpha, r, g, b, theta, S, beta, Cf)
}, LEV_grid$T, LEV_grid$psi)

LEV_grid <- LEV_grid %>%
  mutate(LEV = if_else(!is.na(LEV) & LEV < -1, -1, LEV))

max_point <- LEV_grid[which.max(LEV_grid$LEV), ]

max_LEV_points <- rbind(
  max_LEV_points,
  data.frame(
    Scenario = case,
    T = max_point$T,
    psi = max_point$psi,
    LEV = max_point$LEV
  )
)

my_colors <- c(
  "#000000",  # black
  "#67000d",  # dark red
  "#a50f15",  # red
  "#cb181d",  # lighter red
  "#ef3b2c",  # red-orange
  "#fb6a4a",  # light red
  "#fc9272",  # orange-pink
  "#fcbba1",  # soft light coral
  "#fdd9b5",  # peachy beige
  "#feebc4",  # creamy apricot
  "#fff3b0",  # pale butter yellow
  "#fff8a1",  # light pastel yellow
  "#f5fcb0",  # light lemon-lime
  "#e6fbb4",  # soft lime-yellow
  "#c7e9b4",  # yellow-green
  "#41ab5d",  # green
  "#006d2c",  # dark green
  "#00441b",
  "#002910",
  "#001f0c"
)

LEV_grid <- LEV_grid %>%
  mutate(LEV_class = cut(
    LEV,
    breaks = c(-Inf, 0, 10000, 20000, 30000, 40000, 50000,
               60000, 70000, 80000, 90000, 100000, 110000, 120000,
               130000, 140000, 150000, 160000, 170000, 180000, Inf),
    labels = my_colors,
    include.lowest = TRUE
  ))

# Create dummy data with all LEV_class levels
dummy <- data.frame(
  T = NA,
  psi = NA,
  LEV_class = factor(levels(LEV_grid$LEV_class), levels = levels(LEV_grid$LEV_class))
)

# Bind with real data
LEV_grid_all <- bind_rows(LEV_grid, dummy)


p8 <- ggplot(LEV_grid_all, aes(x = T, y = psi, fill = LEV_class)) +
  geom_tile(na.rm = TRUE) +
  geom_point(
    data = max_point,
    aes(x = T, y = psi),
    color = "blue",
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_fill_identity(
    guide = "legend",
    name = "LEV Class",
    labels = c(
      "< 0", "< 10'000", "< 20'000", "< 30'000", "< 40'000", "< 50'000", "< 60'000", "< 70'000",
      "< 80'000", "< 90'000", "< 100'000", "< 110'000", "< 120'000", "< 130'000",
      "< 140'000", "< 150'000", "< 160'000", "< 170'000", "< 180'000", "> 180,000"
    ),
    breaks = levels(LEV_grid$LEV_class),
    drop = FALSE
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = NA),
    plot.background = element_rect(fill = "transparent", color = NA),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(color = "black", size = 1),
    axis.ticks = element_line(color = "black", size = 1),
    axis.text = element_text(color = "black", size = 12),
    plot.margin = margin(10, 30, 20, 10)
  ) +
  labs(
    title = "Norway Spruce Suitable Scenario 2",
    x = "Rotation Age (T)",
    y = "Thinning Age (ψ)"
  ) 

p2 <- p2 + theme(legend.position = "none")
p3 <- p3 + theme(legend.position = "none")
p4 <- p4 + theme(legend.position = "none")
p5 <- p5 + theme(legend.position = "none")
p6 <- p6 + theme(legend.position = "none")
p7 <- p7 + theme(legend.position = "none")
p8 <- p8 + theme(legend.position = "none")

legend <- get_legend(p1)

p1 <- p1 + theme(legend.position = "none")

main_plots <- (p1 | p2 | p3 | p4) /
  (p5 | p6 | p7 | p8)

final_plot <- main_plots + plot_layout(guides = "collect") & theme(legend.position = "right")

final_plot

write.xlsx(max_LEV_points, "max_LEV_points.xlsx", rowNames = FALSE)

ggsave("Heatmap8.png", width = 20, height = 10, dpi = 600)
