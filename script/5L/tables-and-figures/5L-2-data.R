# DATA TABLES AND FIGURES -----------------------------------------------------

# Code for tables 1-2, figures 3-6 and appendix A1, contained in the data 
# section

# Loading packages and data ---------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw())

hse <- read_csv("data/hse-5L.csv", show_col_types = FALSE)

# Figure 4: age and HSUV Distributions ----------------------------------------
hse |> 
  select(sex, age, index) |> 
  pivot_longer(c(age, index), names_to = "variable", values_to = "value") |> 
  
  ggplot() +
  facet_wrap(vars(variable), scale = "free") +
  
  geom_density(aes(x = value, fill = sex), alpha = 0.5, linewidth = 0) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    caption = "Source: Pooled HSE Data 2017-2018",
    y = "Density"
  )
ggsave("output-5L/2-data/fig-04--distributions.png", height = 4, width = 7)

# Figure 5: Overall Means and CIs ---------------------------------------------
hse |>
  group_by(sex, age) |>
  summarise(
    N = n(),
    HSUV = mean(index),
    SD = sd(index),
    Min = HSUV - (1.96 * SD) / sqrt(N),
    Max = HSUV + (1.96 * SD) / sqrt(N),
    .groups = "drop"
  ) |>
  ggplot(aes(x = age, y = HSUV)) +
  geom_line(aes(colour = sex), linewidth = 0.25, alpha = 0.5) +
  geom_point(aes(colour = sex), alpha = 0.8, size = 0.7) +
  geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.1) +
  
  facet_grid(rows = vars(sex)) +
  
  coord_cartesian(xlim = c(16, 100), ylim = c(0.5, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    caption = "Source: Pooled HSE Data 2017-2018"
  ) +
  guides(colour = "none")
ggsave("output-5L/2-data/fig-05--means.png", height = 4, width = 7)

# Figure : Boxplots ---------------------------------------------
ggplot(hse) +
  geom_boxplot(aes(x = age16g5, y = index), outliers = FALSE) + 
  # geom_bar(aes(x = age16g5, y = after_stat(count) / 3000))

  
  facet_grid(rows = vars(sex)) +
  
  coord_cartesian(ylim = c(0.25, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    caption = "Source: Pooled HSE Data 2017-2018"
  ) 
ggsave("output-5L/2-data/fig-04a--boxplots.png", height = 4, width = 7)

# Figure 6: Std Devs by age Decile --------------------------------------------
hse |> 
  group_by(sex, age = cut(age, quantile(age, seq(0, 1, 0.1)), 
                     include.lowest = TRUE)) |> 
  summarise(var = var(index),
            se = sd(index - mean(index))/sqrt(n()),
            var.min = var - 1.96*se,
            var.max = var + 1.96*se,
            sd = sqrt(var),
            sd.min = sqrt(var.min),
            sd.max = sqrt(var.max)
  ) |> 
  ggplot(aes(x = age, y = sd)) +

  geom_col(aes(fill = sex), position = "dodge") +

  labs(
    caption = "Source: Pooled HSE Data 2017-2018",
    y = "Standard Deviation",
    x = "age Decile"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
ggsave("output-5L/2-data/fig-06--variation.png", height = 5, width = 7)

# (Unused) age plots for individual dimensions --------------------------------
hse |> 
  mutate(across(mobil17:anxiet17, as.factor)) |> 
  pivot_longer(mobil17:anxiet17, names_to = "Dimension", values_to = "Score") |> 
  
  ggplot(aes(x = age16g5)) +
  facet_grid(rows = vars(Dimension), cols = vars(sex)) +
  
  geom_bar(aes(fill = Score), position = position_fill(reverse = TRUE)) +
  
  theme(legend.position="bottom",
        panel.grid=element_blank(),
        axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1)) +
  scale_y_continuous(labels = scales::percent_format(), breaks = c(0, 0.5, 1)) +
  scale_fill_viridis_d(
    option = "inferno", begin = 0.3, end = 0.9,
    labels=c("No Problems", "Mild Problems", "Some Problems", "Moderate Problems", "Extreme Problems")
  ) +
  labs(x = "Age band", y = "Proportion")
ggsave("output-5L/2-data/fig-07--dimensions.png", height = 8, width = 5)

# Summary table
by_yr <- hse |> 
  group_by(hseyr) |> 
  summarise(
    n = n(),
    male = sum(sex == "Male"),
    mean_age = mean(age),
    sd_age = sd(age),
    a16_24 = sum(age >= 16 & age <= 24),
    a25_34 = sum(age >= 25 & age <= 34),
    a35_44 = sum(age >= 35 & age <= 44),
    a45_54 = sum(age >= 45 & age <= 54),
    a55_64 = sum(age >= 55 & age <= 64),
    a65_74 = sum(age >= 65 & age <= 74),
    a75_84 = sum(age >= 75 & age <= 84),
    a85plus = sum(age >= 85),
    utility = mean(index),
    utility_sd = sd(index)
  )
tot <- hse |> 
  summarise(
    n = n(),
    male = sum(sex == "Male"),
    mean_age = mean(age),
    sd_age = sd(age),
    a16_24 = sum(age >= 16 & age <= 24),
    a25_34 = sum(age >= 25 & age <= 34),
    a35_44 = sum(age >= 35 & age <= 44),
    a45_54 = sum(age >= 45 & age <= 54),
    a55_64 = sum(age >= 55 & age <= 64),
    a65_74 = sum(age >= 65 & age <= 74),
    a75_84 = sum(age >= 75 & age <= 84),
    a85plus = sum(age >= 85),
    utility = mean(index),
    utility_sd = sd(index)
  )

write.csv(bind_rows(tot, by_yr), "output-5L/2-data/characeristics.csv")
