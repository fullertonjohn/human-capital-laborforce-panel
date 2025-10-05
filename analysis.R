library(tidyverse)
library(fixest)
library(modelsummary)
library(janitor)
data <- read_csv("data/merged_state_panel.csv")
glimpse(data)
rm(df)
summary(data)
cor(data$lfp_sa, data$bachelors_share, use = "complete.obs")
ggplot(data, aes(x = bachelors_share, y = lfp_sa)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Share of Population with at least a Bachelor's Degree vs Labor Force Participation rate (2010-2018)",
    x = "% of population with a Bachelor's degree or Higher",
    y = "Labor Force Participation Rate (%)"
  ) + 
    theme_minimal()
  lm_model <- lm(lfp_sa ~ bachelors_share, data = data)
  summary(lm_model)
  model_fe <- feols(lfp_sa ~ bachelors_share | state_abbr + year, data = data)
  summary(model_fe)
  
  