# RESULTS TABLES AND FIGURES --------------------------------------------------

# Figures 7-13 and appendices B1-C1, contained in the results section

# Loading packages/data and other setup ---------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rms)
library(ggpubr)
theme_set(theme_bw())

exclude <- c("Ones", "Mean")
capt <- "Source: HSE 2017-18, 10-fold cross-validation"
lw <- 0.25
al <- 0.5
mod.ord <- c(
  "Ones",  "Mean", "Linear", "ALDVMM1", "ALDVMM2", "Quadratic", "Poly-3", "Poly-4", 
  "Poly-5", "Poly-6", "Poly-7", "Poly-8",  "Poly-9", "Poly-10", "Poly-11", 
  "Poly-12", "RCS-3", "RCS-4", "RCS-5", "RCS-6", "RCS-7", "RCS-8", "RCS-9", 
  "RCS-10", "RCS-11", "RCS-12"
)

errs <- read_csv("output-5L/3-results/kfold-results.csv", show_col_types = FALSE) |> 
  mutate(
    Model = factor(Model, levels = mod.ord),
    Type = factor(case_when(
      substr(Model, 1, 4) %in% c("Poly", "Quad") ~ "Polynomial",
      substr(Model, 1, 4) == "RCS-" ~ "RCS",
      Model  %in% c("ALDVMM1, ALDVMM2") ~ "ALDVMM",
      Model == "Linear" ~ "Linear",
      .default = "Other"
    ), levels = c("Linear", "ALDVMM", "Polynomial", "RCS", "Other"))
  ) |> 
   #filter(! if_any(everything(), is.na))
  group_by(age, Fold) |> # New code to filter out data if one of the models in that fold can't predict it (complete case analysis)
  filter(!any(is.na(Predicted))) |> 
  ungroup()

# Appendix B1: Overall Error Statistics ---------------------------------------
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
write_csv(etab.ovr, "output-5L/3-results/app-b1--errors.csv")

# Appendix B2: age-stratified Error Statistics --------------------------------
etab.age <- errs |> 
  group_by(
    agedec = cut(
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
write_csv(etab.age, "output-5L/3-results/app-b2--errors.csv")

etab.age |> 
  mutate(.by = agedec, diff = RMSE - min(RMSE)) |> 
  summarise(.by = Model, diff = sum(diff)) |> 
  arrange(diff)

# (Unused) sex-stratified Error Statistics ------------------------------------
etab.sex <- errs |> 
  group_by(Type, Model, sex, Fold) |> 
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

# Figure 7: Visualisations  ---------------------------------------------------
errs |> 
  filter(!Model %in% exclude) |> 
  group_by(Model, Fold, age, sex) |> 
  summarise(Predicted = mean(Predicted)) |> 

  ggplot(aes(x = age)) +
  facet_wrap(vars(Model), ncol = 5) +
  
  geom_line(aes(y = Predicted, colour = sex, group = Fold), alpha = 0.2) +
  
  labs(y = "Predicted utility") +

  theme(panel.grid = element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0.6, 1)) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

ggsave("output-5L/3-results/fig-07--kfold.png", height = 7, width = 6)


# Figure 8: Overall RMSE plot ---------------------------------------------------
etab.ovr |> 
  filter(! Model %in% exclude) |> 
  ggplot(aes(x = Model, y = RMSE)) +
  #facet_grid(cols = vars(Type), scale = "free", space = "free") +

  geom_point(aes(colour = Type), size = 2, show.legend = FALSE) +
  
  labs(y = "Root mean square error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid= element_blank(),
    strip.text = element_blank()
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)

ggsave("output-5L/3-results/fig-08--rmse.png", height = 4, width = 7)

# Figure 9: RMSE plot by age ---------------------------------------------------
y_span <- 0.001
scales_anchors_rmse <- etab.age |>
  filter(! Model %in% c("Ones", "Mean", "ALDVMM1")) |> 
  group_by(agedec) |>
  summarise(
    centre = mean(RMSE, na.rm = TRUE),
    ymin = round(centre - (y_span / 2), 4),
    ymax = round(centre + (y_span / 2), 4)
  ) |>
  pivot_longer(cols = c(ymin, ymax), names_to = "limit", values_to = "RMSE")

etab.age |> 
  filter(! Model %in% c("Ones", "Mean", "ALDVMM1")) |> 
  ggplot(aes(x = Model, y = RMSE)) +
  facet_wrap(vars(agedec), scales = "free") +
  
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
ggsave("output-5L/3-results/fig-09--rmse.png", height = 5, width = 7)

# Figure 11: Overall ME plot ----------------------------------------------------
etab.ovr |> 
  filter(! Model %in% c(exclude, "ALDVMM1", "ALDVMM2")) |> 
  
  ggplot(aes(x = Model)) +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_point(aes(y = ME, colour = Type), show.legend = FALSE) +
  
  labs(y = "Mean error") +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid = element_blank()
  ) +
  scale_colour_viridis_d(option = "plasma", begin = 0.2, end = 0.8)

ggsave("output-5L/3-results/fig-11--me.png", height = 4, width = 7)

# Figure 12: ME plot by age -----------------------------------------------------
y_span <- 0.001
scales_anchors_me <- etab.age |>
  filter(! Model %in% c("Ones", "Mean", "ALDVMM1")) |> 
  group_by(agedec) |>
  summarise(
    centre = mean(ME, na.rm = TRUE),
    ymin = round(centre - (y_span / 2), 4),
    ymax = round(centre + (y_span / 2), 4)
  ) |>
  pivot_longer(cols = c(ymin, ymax), names_to = "limit", values_to = "RMSE")

etab.age |> 
  filter(! Model %in% c("Ones", "Mean", "ALDVMM1")) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_wrap(vars(agedec), scales = "free") +
  
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
ggsave("output-5L/3-results/fig-12--me-age.png", height = 5, width = 7)

## RCS-5
library(rms)
library(broom)
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
  write.csv("output-5L/3-results/RCS-5-predictions.csv")


