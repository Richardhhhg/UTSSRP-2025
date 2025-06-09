library(dplyr)
library(ggplot2)
library(tidyverse)
df <- as.data.frame(df_complete) 
colnames(df) <- c("Laureate", "Title", "Prize_Winning", "P_Value", "Gender", 
                  "Field", "Citations_Count", "Citation_Percentile", "Nobel_Citation_Percentile", "Publication_Year")

# Divide with field and P_value
chem_p <- df %>% filter(Field == "Chemistry")
phys_p <- df %>% filter(Field == "Physics")
med_p <- df %>% filter(Field == "Medicine")

chem_p <- chem_p %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

phys_p <- phys_p %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

med_p <- med_p %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

combine_p <- df %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))



# Plots over P_value
chem_p_plot <- ggplot(chem_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution by P_Value Bins(Chemistry)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
chem_p_plot

phys_p_plot <- ggplot(phys_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution by P_Value Bins(Physics)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
phys_p_plot

med_p_plot <- ggplot(med_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution by P_Value Bins(Medicine)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
med_p_plot

# Plots over Laureate (too many!!!maybe need sampling)
chem_pl_plot <- chem_p %>%
                ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = chem_p, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Chemistry)",
      x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
chem_pl_plot

phys_pl_plot <- phys_p %>%
  ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = phys_p, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Physics)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
phys_pl_plot

med_pl_plot <- med_p %>%
  ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = med_p, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Medicine)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
med_pl_plot


# single laureate
chem_filter <- chem_p %>% filter(Laureate == "stoddart, j")
s_plot <- chem_filter %>%
  ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  geom_vline(data = chem_filter, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(st)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
s_plot

# Linear Regression
data <- df %>% filter(Prize_Winning == "YES")
model <- lm(Citation_Percentile ~ P_Value, data)
summary(model)


lm_plot <- ggplot(data, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs P_Value",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_plot

# Scale log10 y
data_1 <- df %>% filter(Prize_Winning == "YES")
model_1 <- lm(Citations_Count ~ P_Value, data)
summary(model_1)

lm_plot_1 <- ggplot(data_1, aes(x = P_Value, y = Citations_Count)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citations Count (log10) vs P_Value",
       x = "P_Value", y = "Citations Count (log10)") +
  scale_y_log10() +
  theme_minimal()
lm_plot_1

# Anova
anova_model <- aov(Citation_Percentile ~ P_bin, data = combine_p)
summary(anova_model)
TukeyHSD(anova_model)

anova_plot <- ggplot(combine_p, aes(x = P_bin, y = Citation_Percentile, fill = P_bin)) +
  geom_boxplot() +
  labs(title = "ANOVA: Citation Percentile by P_bin",
       x = "P_bin",
       y = "Citation Percentile") +
  theme_minimal()
anova_plot
# p-value (Pr(>F)): 0.821, which is far above the 0.05 significance threshold, 
# meaning P_bin has no significant effect on Citation_Percentile.

# General Linear Model
glm_model <- glm(Citation_Percentile ~ P_Value, data, family = poisson(link = "identity"))
summary(glm_model)
# Still No Significant

# Bayesian Hierarchical Model
library(brms)

bayesian_model <- brm(Citation_Percentile ~ P_Value + (1 | Field), 
                      data = data, 
                      family = gaussian(), 
                      prior = prior(normal(0, 10), class = "b"))
summary(bayesian_model)

