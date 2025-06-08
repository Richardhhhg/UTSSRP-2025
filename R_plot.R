library(dplyr)
library(ggplot)
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

nobel_citations_percentile <- df %>% filter(Prize_Winning == "YES") %>%
  select(Laureate, Citation_Percentile)


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

#Plots over Laureate (too many!!!maybe need sampling)
chem_pl_plot <- chem_p %>%
                ggplot(aes(x = Citation_Percentile)) +
  geom_histogram(binwidth = 0.05, fill = "red", color = "black") +
  facet_wrap(~Laureate + P_bin) +
  geom_vline(data = nobel_citations_percentile, 
             aes(xintercept = Citation_Percentile),
             color = "blue", linetype = "dashed", linewidth = 1) +
  labs(title = "Citation Percentile Distribution by Laureates(Chemistry)",
      x = "Citation Percentile", y = "Count of Papers") +
  theme_minimal()
chem_pl_plot

