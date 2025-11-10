install.packages(c("readxl", "ggplot2"))
library(readxl)
library(ggplot2)
# --- Load Data ---
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# --- Clean Data: remove missing values ---
data_clean <- data[!is.na(data$`GDP Growth rate`) & !is.na(data$`CO2 emission(in tons)`), ]

# --- HYPOTHESIS ---
# H0: Industrialization (GDP growth) has no effect on CO2 emissions
# H1: Industrialization significantly increases CO2 emissions

# --- Correlation Test (One-tailed, testing for positive relationship) ---
cor_result <- cor.test(data_clean$`GDP Growth rate`,
                       data_clean$`CO2 emission(in tons)`,
                       method = "pearson",
                       alternative = "greater")   # one-tailed test

print(cor_result)

# --- Linear Regression Model ---
model <- lm(`CO2 emission(in tons)` ~ `GDP Growth rate`, data = data_clean)
summary(model)

# --- Visualization: Scatter Plot with Regression Line ---
ggplot(data_clean, aes(x = `GDP Growth rate`, y = `CO2 emission(in tons)`)) +
  geom_point(color = "#0072B2", size = 2.5) +
  geom_smooth(method = "lm", color = "#E69F00", se = TRUE) +
  labs(title = "Effect of Industrialization (GDP Growth) on CO₂ Emissions in India",
       x = "GDP Growth Rate",
       y = "CO₂ Emission (in tons)") +
  theme_minimal()
# --- Install and Load Packages ---
install.packages(c("readxl", "ggplot2"))
library(readxl)
library(ggplot2)

# --- Load Data ---
data <- read_excel("India Case new.xlsx", sheet = "India Case 2")

# --- Clean Data: remove missing values ---
data_clean <- data[!is.na(data$`CO2 emission(in tons)`) & !is.na(data$`Temperature change from CO2`), ]

# --- HYPOTHESIS ---
# H0: There is no relationship between CO2 emissions and temperature change
# H1: Increase in CO2 emissions leads to rise in average temperature

# --- Correlation Test (One-tailed, testing positive relationship) ---
cor_result <- cor.test(data_clean$`CO2 emission(in tons)`,
                       data_clean$`Temperature change from CO2`,
                       method = "pearson",
                       alternative = "greater")   # one-tailed test

print(cor_result)

# --- Linear Regression Model ---
model <- lm(`Temperature change from CO2` ~ `CO2 emission(in tons)`, data = data_clean)
summary(model)

# --- Visualization: Scatter Plot with Regression Line ---
ggplot(data_clean, aes(x = `CO2 emission(in tons)`, y = `Temperature change from CO2`)) +
  geom_point(color = "#0072B2", size = 2.5) +
  geom_smooth(method = "lm", color = "#E69F00", se = TRUE) +
  labs(title = "Relationship Between CO₂ Emissions and Temperature Change in India",
       x = "CO₂ Emission (in tons)",
       y = "Temperature Change (°C)") +
  theme_minimal()

