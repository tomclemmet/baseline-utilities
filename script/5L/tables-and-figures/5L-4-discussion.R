# DISCUSSION TABLES AND FIGURES -----------------------------------------------

# Appendices D1-2, referenced in the discussion

# Loading packages and data ---------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rms)
theme_set(theme_bw())

hse <- read_csv("data/hse-5L.csv", show_col_types = FALSE)

# Appendix D1: RCS-7 Predictions Table ----------------------------------------
rcs7 <- lm(index ~ rcs(age, 7)*sex, data = hse)
quad <- lm(index ~ poly(age, 2, raw = TRUE)*sex, data = hse)

target <- as_tibble(expand.grid(age = c(16:100), sex = c("Male", "Female")))
pred_rcs <- predict(rcs7, target, se.fit = TRUE)
pred_quad <- predict(quad, target, se.fit = TRUE)
preds <- target |> 
  mutate(
    rcs7 = pred_rcs$fit,
    rcs7_se = pred_rcs$se.fit,
    quad = pred_quad$fit,
    quad_se = pred_quad$se.fit
  )
write_csv(preds, "output-5L/4-discussion/app-d1--rcs7.csv")


# Appendix D2: RCS-7 Predictions Graph ------------------------------------
preds |> 
  pivot_longer(c(rcs7, quad), names_to = "Model", values_to = "Fit") |> 
  rename(rcs7 = rcs7_se, quad = quad_se) |> 
  pivot_longer(c(rcs7, quad), names_to = "Model1", values_to = "SE") |> 
  filter(Model == Model1) |> select(-Model1) |> 
  ggplot(aes(x = age, linetype = Model, alpha = Model)) +
  facet_grid(rows = vars(sex)) +
  
  geom_ribbon(aes(ymin = Fit - 1.96 * SE, ymax = Fit + 1.96 * SE, fill = Model), 
              alpha = 0.2) +
  geom_line(aes(y = Fit, colour = Model)) +

  scale_linetype_manual(values = c(2, 1), labels = c("Quadratic", "RCS-7")) +
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_fill_viridis_d(option = "E", begin = 0.2, end = 0.6, direction = -1,
                       labels = c("Quadratic", "RCS-7")) +
  scale_colour_viridis_d(option = "E", begin = 0.2, end = 0.6, direction = -1,
                         labels = c("Quadratic", "RCS-7")) +
  guides(alpha = "none") +
  labs(
    caption = "Source: Pooled HSE 2017-18",
    y = "HSUV"
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
ggsave("output-5L/4-discussion/app-d2--rcs7.png", height = 6, width = 6)        