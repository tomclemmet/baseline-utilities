# RESULTS TABLES AND FIGURES --------------------------------------------------

# Loading packages/data and other setup ---------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rms)
library(ggpubr)
library(rms)
library(broom)
theme_set(theme_bw())

mod.ord <- c(
  "Ones",  "Mean", "Linear", "ALDVMM-1", "ALDVMM-2", "Quadratic", "Poly-3", "Poly-4", 
  "Poly-5", "Poly-6", "Poly-7", "Poly-8",  "Poly-9", "Poly-10", "Poly-11", 
  "Poly-12", "RCS-3", "RCS-4", "RCS-5", "RCS-6", "RCS-7", "RCS-8", "RCS-9", 
  "RCS-10", "RCS-11", "RCS-12"
)

errs <- read_csv("output-5L/kfold-results.csv", show_col_types = FALSE) |> 
  mutate(
    Model = factor(Model, levels = mod.ord),
    Type = factor(case_when(
      substr(Model, 1, 4) %in% c("Poly", "Quad") ~ "Polynomial",
      substr(Model, 1, 4) == "RCS-" ~ "RCS",
      substr(Model, 1, 6) == "ALDVMM" ~ "ALDVMM",
      Model == "Linear" ~ "Linear",
      .default = "Other"
    ), levels = c("Linear", "ALDVMM", "Polynomial", "RCS", "Other"))
  )

# Overall Error Statistics ----------------------------------------------------
etab.ovr <- errs |> 
  group_by(Type, Model, Fold) |> 
  summarise(
    ME = mean(Error),
    MSE = mean(Error^2),
    MAE = mean(abs(Error)),
    .groups = "drop_last"
  ) |> 
  summarise(
    ME = mean(ME),
    RMSE = sqrt(mean(MSE)),
    MAE = mean(MAE),
    .groups = "drop"
  )
write.csv(etab.ovr, "output-5L/app-b1--errors.csv")

# Age-stratified Error Statistics ---------------------------------------------
etab.age <- errs |> 
  group_by(
    agegrp = cut(
      age, 
      breaks = c(0, 30, 50, 70, Inf), 
      labels = c("Age under 30 (n = 2,078)", "Age 30 to 49 (n = 4,692)", "Age 50 to 69 (n = 4,897)", "Age 70 and over (n = 2,746)"),
      right = FALSE,
      include.lowest = TRUE
    ), 
    Type, Model, Fold
  ) |> 
  summarise(
    ME = mean(Error),
    MSE = mean(Error^2),
    MAE = mean(abs(Error)),
    .groups = "drop_last"
  ) |> 
  summarise(
    ME = mean(ME),
    RMSE = sqrt(mean(MSE)),
    MAE = mean(MAE),
    .groups = "drop"
  )
write.csv(etab.age, "output-5L/app-b2--errors.csv")

# Visualisations  -------------------------------------------------------------
errs |> 
  group_by(Model, Fold, age, sex) |> 
  summarise(Predicted = mean(Predicted)) |> 

  ggplot(aes(x = age)) +
  facet_wrap(vars(Model), ncol = 5) +
  
  geom_line(aes(y = Predicted, colour = sex, group = Fold), alpha = 0.2) +
  
  labs(y = "Predicted utility") +

  theme(panel.grid = element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0.6, 1)) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

ggsave("output-5L/fig-07--kfold.png", height = 7, width = 6)


# Overall RMSE plot -----------------------------------------------------------
etab.ovr |> 
  filter(Model != "ALDVMM-1") |> 
  ggplot(aes(x = Model, y = RMSE)) +

  geom_point(aes(colour = Type), size = 2, show.legend = FALSE) +
  
  labs(y = "Root mean square error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid= element_blank(),
    strip.text = element_blank()
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)

ggsave("output-5L/fig-08--rmse.png", height = 4, width = 7)

# RMSE plot by age ------------------------------------------------------------
y_span <- 0.001
scales_anchors_rmse <- etab.age |>
  filter(Model != "ALDVMM-1") |> 
  group_by(agegrp) |>
  summarise(
    centre = mean(RMSE, na.rm = TRUE),
    ymin = round(centre - (y_span / 2), 4),
    ymax = round(centre + (y_span / 2), 4)
  ) |>
  pivot_longer(cols = c(ymin, ymax), names_to = "limit", values_to = "RMSE")

etab.age |> 
  filter(Model != "ALDVMM-1") |> 
  ggplot(aes(x = Model, y = RMSE)) +
  facet_wrap(vars(agegrp), scales = "free") +
  
  geom_point(aes(colour = Type), show.legend = FALSE) +
  geom_blank(aes(y = RMSE), inherit.aes = FALSE, data = scales_anchors_rmse) +
  
  scale_y_continuous(breaks = seq(0, 0.5, 0.0002)) +
  labs(y = "Root mean square error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)
ggsave("output-5L/fig-09--rmse.png", height = 5, width = 7)

# Mean error plot -------------------------------------------------------------
etab.ovr |> 
  filter(!Model %in% c("ALDVMM-1", "ALDVMM-2")) |> 
  ggplot(aes(x = Model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_point(aes(y = ME, colour = Type), show.legend = FALSE) +
  
  labs(y = "Mean error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid = element_blank()
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)

ggsave("output-5L/fig-11--me.png", height = 4, width = 7)

# Mean error by age -----------------------------------------------------------
y_span <- 0.001
scales_anchors_me <- etab.age |>
  filter(!Model %in% c("ALDVMM-1", "ALDVMM-2")) |> 
  group_by(agegrp) |>
  summarise(
    centre = mean(ME, na.rm = TRUE),
    ymin = round(centre - (y_span / 2), 4),
    ymax = round(centre + (y_span / 2), 4)
  ) |>
  pivot_longer(cols = c(ymin, ymax), names_to = "limit", values_to = "RMSE")

etab.age |> 
  filter(!Model %in% c("ALDVMM-1", "ALDVMM-2")) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_wrap(vars(agegrp), scales = "free") +
  
  geom_hline(yintercept = 0, linetype = 2, alpha = 0.5) +
  geom_point(aes(colour = Type), show.legend = FALSE) +
  geom_blank(aes(y = RMSE), inherit.aes = FALSE, data = scales_anchors_me) +
  
  scale_y_continuous(breaks = seq(-0.1, 0.1, 0.0025)) +
  labs(y = "Mean error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)
ggsave("output-5L/fig-12--me-age.png", height = 5, width = 7)

# RCS-5 -----------------------------------------------------------------------
ages <- data.frame(age = seq(16, 100, 1))

male <- hse |> 
  filter(sex == "Male") |> 
  lm(index ~ rcs(age, 5), data = _) |> 
  augment(newdata = ages, interval = "confidence")

female <- hse |> 
  filter(sex == "Female") |> 
  lm(index ~ rcs(age, 5), data = _) |> 
  augment(newdata = ages, interval = "confidence")

bind_rows(list(male = male, female = female), .id = "sex") |> 
  write.csv("output-5L/RCS-5-predictions.csv")
