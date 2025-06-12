library(dplyr)
library(ggplot2)
library(tidyverse)
df <- as.data.frame(df_complete) 
colnames(df) <- c("Laureate", "Title", "Prize_Winning", "P_Value", "Gender", 
                  "Field", "Citations_Count", "Citation_Percentile", "Nobel_Citation_Percentile", "Publication_Year")

# Divide by field and Relative Position
chem_p <- df %>% filter(Field == "Chemistry", Prize_Winning == "YES") %>% 
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
phys_p <- df %>% filter(Field == "Physics", Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
med_p <- df %>% filter(Field == "Medicine", Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
combine_p <- df %>% filter(Prize_Winning == "YES") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))




# Histogram binned by Relative Position (For Nobel-Prize papers)
chem_p_plot <- ggplot(chem_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Relative Position(Chemistry)",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
chem_p_plot

phys_p_plot <- ggplot(phys_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Relative Position(Physics)",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
phys_p_plot

med_p_plot <- ggplot(med_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Relative Position(Medicine)",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
med_p_plot

combine_p_plot <- ggplot(combine_p, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~P_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Relative Position",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
combine_p_plot




# Plots over Laureate for each field for all papers
# (too many!!!maybe need sampling)
chem_ps <- df %>% filter(Field == "Chemistry") %>% 
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
phys_ps <- df %>% filter(Field == "Physics") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))
med_ps <- df %>% filter(Field == "Medicine") %>%
  mutate(P_bin = cut(P_Value, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE))

chem_pl_plot <- chem_ps %>%
                ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = chem_ps, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Chemistry)",
      x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
chem_pl_plot

phys_pl_plot <- phys_ps %>%
  ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = phys_ps, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Physics)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
phys_pl_plot

med_pl_plot <- med_ps %>%
  ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate) +
  geom_vline(data = med_ps, 
             aes(xintercept = Nobel_Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Percentile Distribution by Laureates(Medicine)",
       x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
med_pl_plot


# check for a single laureate
chem_st <- chem_p %>% filter(Laureate == "stoddart, j")
st_plot <- chem_st %>%
  ggplot(aes(x = Citations_Count)) +
  geom_histogram(binwidth = 50, fill = "blue", color = "black") +
  geom_vline(data = chem_st, 
             aes(xintercept = 2247),
             color ="black", linetype = "dashed", linewidth = 0.5) +
  labs(title = "Citation Counts Distribution by Laureates(stoddart, j)",
       x = "Citation Counts", y = "Count of Papers") +
  theme_minimal()
st_plot



# Linear Regression across fields (for Nobel-Prize papers)
data_chem <- combine_p %>% filter(Field == "Chemistry")

lm_chem_plot <- ggplot(data_chem, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs Relative Position (Chemistry)",
       x = "Relative", y = "Citation Percentile") +
  theme_minimal()
lm_chem_plot

chem_model <- lm(Citation_Percentile ~ P_Value , data = data_chem)
summary(chem_model)

data_phys <- combine_p %>% filter(Field == "Physics")

lm_phys_plot <- ggplot(data_phys, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs Relative Position (Physics)",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_phys_plot

phys_model <- lm(Citation_Percentile ~ P_Value, data = data_phys)
summary(phys_model)

data_med <- combine_p %>% filter(Field == "Medicine")

lm_med_plot <- ggplot(data_med, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs Relative Position (Medicine)",
       x = "P_Value", y = "Citation Percentile") +
  theme_minimal()
lm_med_plot

med_model <- lm(Citation_Percentile ~ P_Value, data = data_med)
summary(med_model)

lm_combine_plot <- ggplot(combine_p, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Linear Regression: Citation Percentile vs Relative Position",
       x = "Relative Position", y = "Citation Percentile") +
  theme_minimal()
lm_combine_plot



# Divide by Publication Year
combine_d <- combine_p %>%
  mutate(decade_bin = cut(Publication_Year, breaks = seq(1880, 2010, by = 10), include.lowest = TRUE))

overall_model <- lm(Citation_Percentile ~ P_Value + as.factor(Field) + decade_bin + as.factor(Gender), data = combine_d)
summary(overall_model)

overall_model_1 <- lm(Citation_Percentile ~ as.factor(Field) + as.factor(Gender) + as.factor(Field) * as.factor(Gender), data = combine_d)
summary(overall_model_1)



# Histogram binned by Publication Year
combine_d <- combine_p %>%
  filter(!is.na(Publication_Year)) %>%
  mutate(
    decade_start = floor((Publication_Year - 1) / 10) * 10,
    decade_end = decade_start + 10,
    decade_bin = paste0(decade_start, "–", decade_end)
  )

decade_plot_ci <- ggplot(combine_d, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~decade_bin, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Decade",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
decade_plot_ci


decade_plot_p <- ggplot(combine_d, aes(x = P_Value)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~decade_bin, ncol = 5, scales = "free_y") +
  labs(title = "Relative Position Distribution binned by Decade",
       x = "Relative Position", y = "Count of Nobel_Prize Papers") +
  theme_minimal()

decade_plot_p


# Binned by Gender
gender_plot_ci <- ggplot(combine_d, aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~Gender, ncol = 5, scales = "free_y") +
  labs(title = "Citation Percentile Distribution binned by Gender",
       x = "Citation Percentile", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
gender_plot_ci

gender_plot_p <- ggplot(combine_d, aes(x = P_Value)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  facet_wrap(~Gender, ncol = 5, scales = "free_y") +
  labs(title = "Relative Position Distribution binned by Gender",
       x = "Relative Position", y = "Count of Nobel-Prize Papers") +
  theme_minimal()
gender_plot_p



gender_combine_p <- ggplot(combine_d, aes(x = P_Value, y = Citation_Percentile)) +
  geom_point(color = "blue") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  facet_wrap(~Gender, ncol = 5, scales = "free_y") +
  labs(title = "Relative Position vs Citation Percentile binned by Gender",
       x = "Relative Position", y = "Citation Percentile") +
  theme_minimal()
gender_combine_p






