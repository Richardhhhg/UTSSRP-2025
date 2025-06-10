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

chem_p <- chem_p %>% filter(Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

phys_p <- phys_p %>% filter(Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

med_p <- med_p %>% filter(Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

combine_p <- df %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))



# Histogram binned by P_value
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

# Linear Regression across fields
data_chem <- combine_p %>% filter(Prize_Winning == "YES", Field == "Chemistry")

lm_chem_plot <- ggplot(data_chem, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs P_Value (Chemistry)",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_chem_plot

chem_model <- lm(Citation_Percentile ~ P_Value, data = data_chem)
summary(chem_model)

data_phys <- combine_p %>% filter(Prize_Winning == "YES", Field == "Physics")

lm_phys_plot <- ggplot(data_phys, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs P_Value (Physics)",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_phys_plot

phys_model <- lm(Citation_Percentile ~ P_Value, data = data_phys)
summary(phys_model)

data_med <- combine_p %>% filter(Prize_Winning == "YES", Field == "Medicine")

lm_med_plot <- ggplot(data_med, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs P_Value (Medicine)",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_med_plot

med_model <- lm(Citation_Percentile ~ P_Value, data = data_med)
summary(med_model)

# Binned by Publication Year
decade_df <- data %>%
  mutate(decade_bin = cut(Publication_Year, breaks = seq(1887, 2010, by = 10), include.lowest = TRUE))

decade_plot <- ggplot(decade_df, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~decade_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution by decade Bins",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
decade_plot

# Binned by Gender
gender_plot <- ggplot(data, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~Gender, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution by Gender",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
gender_plot






