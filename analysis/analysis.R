#install libraries
library(tidyverse)
library(fixest)
library(modelsummary)
library(janitor)
#import, preview data
data <- read_csv("data/merged_state_panel.csv")
glimpse(data)
summary(data)
#compute Pearson correlation coefficient
cor(data$lfp_sa, data$bachelors_share, use = "complete.obs")
# generate lsr dot plot
ggplot(data, aes(x = bachelors_share, y = lfp_sa)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Share of Population with at least a Bachelor's Degree vs Labor Force Participation rate (2010-2019)",
    x = "% of population with a Bachelor's degree or Higher",
    y = "Labor Force Participation Rate (%)"
  ) + 
    theme_minimal()
  lm_model <- lm(lfp_sa ~ bachelors_share, data = data)
  summary(lm_model)
  model_fe <- feols(lfp_sa ~ bachelors_share | state_abbr + year, data = data)
  summary(model_fe)
  ggsave("outputs/laborforce_vs_education.png", width = 8, height = 5)
  
  install.packages("modelsummary")
  library(modelsummary)

  # Export both side-by-side as an HTML table
   modelsummary(
     list("OLS (Pooled)" = lm_model,
          "Two-way Fixed Effects" = model_fe),
     output = "outputs/regression_summary.html"
   )

  