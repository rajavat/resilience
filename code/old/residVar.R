# INITIALISE ———————————————————————————————————————————————————————————————————
library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

# read in EVI data —————————————————————————————————————————————————————————————
EVIdata <- read.csv("data/GEE/EVIdataQA.csv") %>% 
  drop_na()

# perform linear regression for each PID
plots <- EVIdata %>%
  group_by(PID) %>%
  do(model = lm(mean ~ year, data = .)) %>%
  mutate

# calculate the variance in residuals for each PID
residuals <- plots %>%
  mutate(
    sum_abs_residuals = sum(abs(resid(model)))
  ) %>%
  select(PID, sum_abs_residuals)


# plotting —————————————————————————————————————————————————————————————————————
heatmap_data <- EVIdata %>%
  group_by(PID) %>%
  group_modify(~{
    model <- lm(mean ~ year, data = .x)
    .x %>% mutate(residual = resid(model))
  }) %>%
  # select top 30 PIDs by absolute residual sum
  filter(PID %in% (residuals %>% 
                     arrange(-sum_abs_residuals) %>% 
                     head(30) %>% 
                     pull(PID)))

# create heatmap
ggplot(heatmap_data, aes(x = year, y = PID, fill = residual)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  labs(title = "Residual Patterns Over Time by Plot", 
       x = "Year", 
       y = "Plot ID", 
       fill = "Residual")

