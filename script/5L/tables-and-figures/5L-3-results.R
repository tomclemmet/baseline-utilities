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

exclude <- c("Ones", "Mean", "ALDVMM")
capt <- "Source: HSE 2017-18, 10-fold cross-validation"
lw <- 0.25
al <- 0.5
mod.ord <- c(
  "Ones",  "Mean", "Linear", "ALDVMM", "Quadratic", "Poly-3", "Poly-4", 
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
      Model == "ALDVMM" ~ "ALDVMM",
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
  group_by(agedec = cut(
    age, quantile(age, seq(0, 1, 0.1)), 
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

  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0.4, 1)) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

ggsave("output-5L/3-results/fig-07--kfold.png", height = 7, width = 6)


# Figure 8: Overall RMSE plot ---------------------------------------------------
etab.ovr |> 
  filter(! Model %in% exclude) |> 
  ggplot(aes(x = Model, y = RMSE)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_col(aes(fill = Type)) +
  
  labs(
    caption = capt,
    y = "Difference in RMSE"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid= element_blank(),
    strip.text = element_blank()
  ) +
  coord_cartesian(ylim = c(0.1915, 0.192))

ggsave("output-5L/3-results/fig-08--rmse.png", height = 4, width = 7)

# Figure 9: RMSE plot by age ---------------------------------------------------
etab.age |> 
  filter(Model == "Linear") |> 
    right_join(etab.age, by = "agedec") |> 
    group_by(agedec, Type = Type.y, Model = Model.y) |> 
    summarise(
      ME.diff = ME.y - ME.x,
      RMSE.loss = RMSE.y - RMSE.x,
      MAE.loss = MAE.y - MAE.x,
      .groups = "drop"
    ) |> 
  filter(! Model %in% exclude) |> 
  
  ggplot(aes(x = Model, y = RMSE.loss)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = agedec, colour = agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = agedec, shape = agedec), alpha = 0.8) +
  
  labs(
    y = "Difference in RMSE",
    caption = capt,
    colour = "age Decile", 
    shape = "age Decile"
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.1, end = 0.9) +
  scale_shape_manual(values = c(17, 18, rep(15:18, 2))) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
  )

ggsave("output-5L/3-results/fig-09--rmse.png", height = 5, width = 7)

# Figure 10: Ones vs linear error distributions (relevant for MAEs) -------------
errs |> 
  filter(Model %in% c("Ones", "Linear")) |> 
  group_by(Model) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  
  geom_density(aes(x = Error, fill = Model), alpha = 0.5, linewidth = 0) +
  
  labs(
    y = "Density",
    caption = "Source: Pooled HSE 2017-18, 10-fold cross-validation"
  ) +
  coord_cartesian(xlim = c(-1.239, 0.3)) +
  theme(panel.grid.minor = element_blank()) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.8)

ggsave("output-5L/3-results/fig-10--errors.png", width = 7, height = 4)

# Figure 11: Overall ME plot ----------------------------------------------------
etab.sex |> 
  select(-c(RMSE, MAE)) |> 
  rename(ME_bysex = ME) |> 
  full_join(select(etab.ovr, -c(RMSE, MAE))) |> 
  filter(! Model %in% c(exclude, "ALDVMM")) |> 
  mutate(
    Type = if_else(Type == "Linear", "Polynomial", Type),
    lab1 = "Overall"
  ) |> 
  
  ggplot(aes(x = Model)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(
    aes(y = ME_bysex, colour = sex, group = sex), 
    linewidth = lw, alpha = al
  ) +
  geom_point(aes(y = ME_bysex, colour = sex), alpha = al, size = 0.5) +
    
  geom_line(
    aes(y = ME, group = Type, linetype = lab1), 
    linewidth = lw, colour = "#31446b"
  ) +
  geom_point(aes(y = ME, shape = lab1), colour = "#31446b") +
  
  labs(
    caption = capt, linetype = "", shape = ""
  ) +   
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  ) +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) 
ggsave("output-5L/3-results/fig-11--me.png", height = 4, width = 7)

# Figure 12: ME plot by age -----------------------------------------------------
etab.age |> 
  filter(! Model %in% c(exclude, "ALDVMM")) |> 
  mutate(Type = case_match(Type, "Linear" ~ "Polynomial", .default = Type)) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = agedec, colour = agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = agedec, shape = agedec)) +
  
  
  labs(
    caption = "Source: Pooled HSE 2017-18, 10-fold cross-validation",
    colour = "age Decile",
    shape = "age Decile"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.1, end = 0.9) +
  scale_shape_manual(values = c(17, 18, rep(15:18, 2)))

ggsave("output-5L/3-results/fig-12--me.png", height = 5.5, width = 7)


