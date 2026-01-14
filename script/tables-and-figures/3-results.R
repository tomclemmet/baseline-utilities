# RESULTS TABLES AND FIGURES --------------------------------------------------

# Figures 7-13 and appendices B1-C1, contained in the results section

# Loading packages/data and other setup ---------------------------------------
pacman::p_load(dplyr, tidyr, readr, ggplot2, rms, ggpubr)
theme_set(theme_bw())

exclude <- c("Ones", "Mean", "Poly-6", "Poly-7", 
              "Poly-8", "Poly-9", "Poly-10")
capt <- "Source: HSE 2003-2014, 10-fold cross-validation"
lw <- 0.25
al <- 0.5
mod.ord <- c(
  "Ones",  "Mean", "Linear", "ALDVMM", "Quadratic", "Poly-3", "Poly-4", 
  "Poly-5", "Poly-6", "Poly-7", "Poly-8",  "Poly-9", "Poly-10", "Poly-11", 
  "Poly-12", "RCS-3", "RCS-4", "RCS-5", "RCS-6", "RCS-7", "RCS-8", "RCS-9", 
  "RCS-10", "RCS-11", "RCS-12"
)

errs <- read_csv("output/3-results/kfold-results.csv", show_col_types = FALSE) |> 
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
  group_by(Age, Fold) |> # New code to filter out data if one of the models in that fold can't predict it (complete case analysis)
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
write_csv(etab.ovr, "output/3-results/app-b1--errors.csv")

# Appendix B2: Age-stratified Error Statistics --------------------------------
etab.age <- errs |> 
  group_by(Agedec = cut(
    Age, quantile(Age, seq(0, 1, 0.1)), 
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
write_csv(etab.age, "output/3-results/app-b2--errors.csv")

# (Unused) Sex-stratified Error Statistics ------------------------------------
etab.sex <- errs |> 
  group_by(Type, Model, Sex, Fold) |> 
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

# Figure 7: Visualisations for one fold -----------------------------------------
errs |> 
  filter(Model != "Ones") |> 
  group_by(Model, Fold, Age, Sex) |> 
  summarise(Predicted = mean(Predicted)) |> 

  ggplot(aes(x = Age)) +
  facet_wrap(vars(Model), ncol = 5) +
  
  geom_line(aes(y = Predicted, colour = Sex, group = Fold), alpha = 0.2) +
  
  labs(y = "Predicted HSUV") +

  theme(panel.grid.minor = element_blank(), legend.position = "bottom") +
  coord_cartesian(ylim = c(0.4, 1)) +
  guides(colour = guide_legend(override.aes = list(alpha=1)))

ggsave("output/3-results/fig-07--kfold.png", height = 7, width = 6)


# Figure 8: Overall RMSE plot ---------------------------------------------------
etab.ovr |> 
  mutate(RMSE.loss = RMSE - filter(etab.ovr, Model == "Linear")$RMSE) |> 
  filter(! Model %in% exclude) |> 
  ggplot(aes(x = Model, y = RMSE.loss)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Type), 
            colour = "#31446b", linewidth = lw, alpha = al) +
  geom_point(colour = "#31446b") +
  
  labs(
    caption = capt,
    y = "Difference in RMSE"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank()
  )

ggsave("output/3-results/fig-08--rmse.png", height = 4, width = 7)

# Figure 9: RMSE plot by age ---------------------------------------------------
etab.age |> 
  filter(Model == "Linear") |> 
    right_join(etab.age, by = "Agedec") |> 
    group_by(Agedec, Type = Type.y, Model = Model.y) |> 
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
  
  geom_line(aes(group = Agedec, colour = Agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = Agedec, shape = Agedec), alpha = 0.8) +
  
  labs(
    y = "Difference in RMSE",
    caption = capt,
    colour = "Age Decile", 
    shape = "Age Decile"
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.1, end = 0.9) +
  scale_shape_manual(values = c(17, 18, rep(15:18, 2))) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    strip.text = element_blank()
  )

ggsave("output/3-results/fig-09--rmse.png", height = 5, width = 7)

# Figure 10: Ones vs linear error distributions (relevant for MAEs) -------------
errs |> 
  filter(Model %in% c("Ones", "Linear")) |> 
  group_by(Model) |> 
  ggplot() +
  geom_vline(xintercept = 0, linetype = 2) +
  
  geom_density(aes(x = Error, fill = Model), alpha = 0.5, linewidth = 0) +
  
  labs(
    y = "Density",
    caption = "Source: Pooled HSE 2004-2014, 10-fold cross-validation"
  ) +
  coord_cartesian(xlim = c(-1.239, 0.3)) +
  theme(panel.grid.minor = element_blank()) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.8)

ggsave("output/3-results/fig-10--errors.png", width = 7, height = 4)

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
    aes(y = ME_bysex, colour = Sex, group = Sex), 
    linewidth = lw, alpha = al
  ) +
  geom_point(aes(y = ME_bysex, colour = Sex), alpha = al, size = 0.5) +
    
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
ggsave("output/3-results/fig-11--me.png", height = 4, width = 7)

# Figure 12: ME plot by age -----------------------------------------------------
etab.age |> 
  filter(! Model %in% c(exclude, "ALDVMM")) |> 
  mutate(Type = case_match(Type, "Linear" ~ "Polynomial", .default = Type)) |> 
  ggplot(aes(x = Model, y = ME)) +
  facet_grid(cols = vars(Type), scale = "free_x", space = "free") +
  geom_hline(yintercept = 0, linetype = 2) +
  
  geom_line(aes(group = Agedec, colour = Agedec), linewidth = lw, alpha = al) +
  geom_point(aes(colour = Agedec, shape = Agedec)) +
  
  
  labs(
    caption = "Source: Pooled HSE 2003-2014, 10-fold cross-validation",
    colour = "Age Decile",
    shape = "Age Decile"
  ) +
  theme(
    axis.text.x = element_text(angle = 270, vjust = 0.5, hjust = 0.1),
    panel.grid.minor = element_blank(),
    strip.text = element_blank(),
    legend.position = "bottom"
  ) +
  scale_colour_viridis_d(option = "E", begin = 0.1, end = 0.9) +
  scale_shape_manual(values = c(17, 18, rep(15:18, 2)))

ggsave("output/3-results/fig-12--me.png", height = 5.5, width = 7)


# Appendix C1: QALE Estimates ---------------------------------------------

hse <- read_csv("data/hse.csv", show_col_types = FALSE)
lt <- read_csv("data/lifetables-1820.csv", show_col_types = FALSE)

# Half-cycle correction
lt$s_hcc <- NA
for (i in 1:84) {
  lt$s_hcc[i] <- 0.5 * lt$Survival[i] + 0.5 * lt$Survival[i + 1]
}
for (i in 86:169) {
  lt$s_hcc[i] <- 0.5 * lt$Survival[i] + 0.5 * lt$Survival[i + 1]
}

rcs7 <- lm(Index ~ rcs(Age, 7)*Sex, data = hse)
quad <- lm(Index ~ poly(Age, 2, raw = TRUE)*Sex, data = hse)

target <- as_tibble(expand.grid(Age = c(16:100), Sex = c("Male", "Female")))
preds <- target |> 
  mutate(
    rcs7 = predict(rcs7, target),
    quad = predict(quad, target)
  )

r <- 0.035
qale <- preds |> 
  full_join(lt, by = c("Age", "Sex")) |> 
  filter(Age != 100) |> 
  mutate(
    rcs7.q = mapply(\(x, y, z)
                    sum(
                      rcs7[Age >= x & Sex == y] *
                        (s_hcc[Age >= x & Sex == y]/z) *
                        1/((1+r)^(0:(99 - x)))
                    ),
                    Age, Sex, s_hcc
    ),
    quad.q = mapply(\(x, y, z)
                    sum(
                      quad[Age >= x & Sex == y] * 
                        (s_hcc[Age >= x & Sex == y]/z) *
                        1/((1+r)^(0:(99 - x)))
                    ),
                    Age, Sex, s_hcc
    ),
    .keep = "used"
  ) |> 
  select(-c(s_hcc, rcs7:quad)) |> 
  mutate(diff = rcs7.q-quad.q) |> 
  pivot_wider(names_from = Sex, values_from = c(rcs7.q:diff))
write_csv(qale, "output/3-results/app-c1--qales.csv")


# Figure 13: QALEs Illustration -----------------------------------------------
ages <-  c(30, 50, 70, 90)
s <-  "Female"
r <- 0.035

plots <- lapply(ages, function(a) {
  preds |> 
    full_join(lt, by = c("Age", "Sex")) |> 
    filter(Age != 100, Sex == s, Age >= a) |> 
    mutate(
      Age = Age,
      Survival = s_hcc/s_hcc[Age == a & Sex == s],
      uRCS = rcs7,
      uQuad = quad,
      Discount = 1/((1+r)^(0:(99 - a))),
      RCS7 = Survival * uRCS * Discount,
      Quad = Survival * uQuad * Discount,
      .keep = "none"
    ) |>
    pivot_longer(c(Survival, Discount), names_to = "Variable", values_to = "Value") |> 
    pivot_longer(uRCS:uQuad, names_to = "Model", values_to = "HSUV") |> 
    
    ggplot(aes(x = Age)) +
    geom_line(aes(y = Value, linetype = Variable)) +
    geom_line(aes(y = HSUV, colour = Model)) +
    
    scale_colour_viridis_d(name = "", option = "E", begin = 0.2, end = 0.8,
                           labels = c("Quadratic HSUV", "RCS-7 HSUV")) +
    scale_linetype_manual(name = "", values = c(2, 3),
                          labels = c("Discount Factor", "Survival Rate")) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank()
    ) +
    if (a != 30) {
      theme(
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.y = element_blank()
      )
    }
})
ggarrange(
  plotlist = plots, 
  labels = paste("Age", ages),
  hjust = c(-2.5, rep(-1.7, 3)),
  vjust = 2.2,
  font.label = list(size = 10),
  common.legend = TRUE, 
  nrow = 1,
  widths = c(1.3, rep(1, 3)),
  legend = "bottom"
)

ggsave("output/3-results/fig-13--qale.png", height = 3, width = 6)
