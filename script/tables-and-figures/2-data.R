# DATA TABLES AND FIGURES -----------------------------------------------------

# Code for tables 1-2, figures 3-6 and appendix A1, contained in the data 
# section

# Loading packages and data ---------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw())

hse <- read_csv("data/hse.csv", show_col_types = FALSE)

old <- hse |> filter(Year %in% c(2003))
new <- hse |> filter(Year %in% c(2010, 2011))
pred <- hse |> filter(Year == 2012)

# Table 1: Summary statistics for different rounds ----------------------------
rounds_sum <- hse |>
  group_by(Year) |>
  summarise(
    n = n(),
    `n (Over 75)` = mean(Age >= 75) * n,
    `n (Over 90)` = mean(Age >= 90) * n,
    `Mean Age` = mean(Age),
    `SD of Age` = sd(Age),
    `Mean HSUV` = mean(Index),
    `SD of HSUV` = sd(Index),
  )
write_csv(rounds_sum, "output/2-data/tab-01--summary.csv")

# Appendix A1: Old vs New OLS Regression results -------------------------------
ols_old_m <- lm(Index ~ poly(Age, 2, raw = TRUE), 
                data = old, subset = (Sex == "Male"))
ols_new_m <- lm(Index ~ poly(Age, 2, raw = TRUE), 
                data = new, subset = (Sex == "Male"))
ols_old_f <- lm(Index ~ poly(Age, 2, raw = TRUE), 
                data = old, subset = (Sex == "Female"))
ols_new_f <- lm(Index ~ poly(Age, 2, raw = TRUE), 
                data = new, subset = (Sex == "Female"))

stargazer::stargazer(
  ols_old_m, ols_new_m, ols_old_f, ols_new_f, 
  digits = 7, df = FALSE,
  column.labels = c("old m", "new m", "old f", "new f"),
  type = "html",
  out = "output/2-data/app-a1--regression.html"
)


# Figure 3: Old Means vs New Means --------------------------------------------
bind_rows(old, new) |>
  mutate(Source = as.factor(case_when(
    Year < 2008 ~ "Old Data", 
    .default = "New Data"))
  ) |> 
  group_by(Source, Age, Sex) |>
  summarise(n = n(), HSUV = mean(Index), sd = sd(Index), .groups = "drop") |>
  
  ggplot(aes(x = Age, y = HSUV)) +
  facet_grid(rows = vars(Sex)) +
  
  geom_line(aes(colour = Source), linewidth = 0.25, alpha = 0.5) +
  geom_point(aes(colour = Source), alpha = 0.8, size = 0.7) +
  
  coord_cartesian(ylim = c(0.5, 1)) +
  theme(panel.grid.minor = element_blank()) +
  scale_colour_viridis_d(begin = 0.2, end = 0.8, option = "E")

ggsave("output/2-data/fig-03--means.png", width = 7, height = 4)

# Table 2: Old vs New Error Statistics ----------------------------------------
pred <- pred |>
  arrange(Sex) |>
  mutate(
    Old = c(
      predict(ols_old_f, filter(pred, Sex == "Female")),
      predict(ols_old_m, filter(pred, Sex == "Male"))
    ),
    New = c(
      predict(ols_new_f, filter(pred, Sex == "Female")),
      predict(ols_new_m, filter(pred, Sex == "Male"))
    )
  )
etab <- pred |> 
  mutate(Old = Index - Old, New = Index - New) |>
  pivot_longer(Old:New, names_to = "Source", values_to = "Error") |> 
  group_by(Source) |> 
  summarise(
    RMSE = sqrt(mean(Error^2)),
    MAE = mean(abs(Error)),
    ME = mean(Error)
  )
write_csv(etab, "output/2-data/tab-02--errors.csv")

# Figure 4: Age and HSUV Distributions ----------------------------------------
hse |> 
  select(Sex, Age, Index) |> 
  pivot_longer(c(Age, Index), names_to = "Variable", values_to = "Value") |> 
  
  ggplot() +
  facet_wrap(vars(Variable), scale = "free") +
  
  geom_density(aes(x = Value, fill = Sex), alpha = 0.5, linewidth = 0) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    caption = "Source: Pooled HSE Data 2003-2014",
    y = "Density"
  )
ggsave("output/2-data/fig-04--distributions.png", height = 4, width = 7)

# Figure 5: Overall Means and CIs ---------------------------------------------
hse |>
  group_by(Sex, Age) |>
  summarise(
    N = n(),
    HSUV = mean(Index),
    SD = sd(Index),
    Min = HSUV - (1.96 * SD) / sqrt(N),
    Max = HSUV + (1.96 * SD) / sqrt(N),
    .groups = "drop"
  ) |>
  ggplot(aes(x = Age, y = HSUV)) +
  geom_line(aes(colour = Sex), linewidth = 0.25, alpha = 0.5) +
  geom_point(aes(colour = Sex), alpha = 0.8, size = 0.7) +
  geom_ribbon(aes(ymin = Min, ymax = Max), alpha = 0.1) +
  
  facet_grid(rows = vars(Sex)) +
  
  coord_cartesian(xlim = c(16, 100), ylim = c(0.5, 1)) +
  theme(panel.grid.minor = element_blank()) +
  labs(
    caption = "Source: Pooled HSE Data 2003-2014"
  ) +
  guides(colour = "none")
ggsave("output/2-data/fig-05--means.png", height = 4, width = 7)

# Figure 6: Std Devs by Age Decile --------------------------------------------
hse |> 
  group_by(Sex, Age = cut(Age, quantile(Age, seq(0, 1, 0.1)), 
                     include.lowest = TRUE)) |> 
  summarise(var = var(Index),
            se = sd(Index - mean(Index))/sqrt(n()),
            var.min = var - 1.96*se,
            var.max = var + 1.96*se,
            sd = sqrt(var),
            sd.min = sqrt(var.min),
            sd.max = sqrt(var.max)
  ) |> 
  ggplot(aes(x = Age, y = sd)) +

  geom_col(aes(fill = Sex), position = "dodge") +

  labs(
    caption = "Source: Pooled HSE Data 2003-2014",
    y = "Standard Deviation",
    x = "Age Decile"
  ) +
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  )
ggsave("output/2-data/fig-06--variation.png", height = 5, width = 7)

# (Unused) Age plots for individual dimensions --------------------------------
hse |> 
  mutate(across(MO:AD, as.factor), 
         Age = case_match(Age, 92.5 ~ 93, .default = Age)) |> 
  pivot_longer(MO:AD, names_to = "Dimension", values_to = "Score") |> 
  group_by(Age, Sex, Dimension) |> filter(n() >= 50) |> 
  
  ggplot(aes(x = Age)) +
  facet_grid(rows = vars(Dimension), cols = vars(Sex)) +
  
  geom_bar(aes(fill = Score), position = position_fill(reverse = TRUE)) +
  
  theme(legend.position="bottom",
        panel.grid=element_blank()) +
  scale_y_continuous(breaks=c(0, 0.5, 1)) +
  scale_fill_discrete(
    name="",
    labels=c("No Problems", "Some Problems", "Extreme Problems"),
  )
