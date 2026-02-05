# LITERATURE REVIEW FIGURES ---------------------------------------------------

# Code for figures 1 and 2, contained in the literature review

# Loading packages and data ---------------------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
theme_set(theme_bw())

nonpara_baselines <- read_csv("data/npara-baselines.csv", show_col_types = FALSE) 
para_baselines <- read_csv("data/para-baselines.csv", show_col_types = FALSE) |> 
  pivot_longer(HSE0306:HSE14, names_to = "Source", values_to = "Index")

# Figure 1: Non-Parametric Baselines ------------------------------------------
nonpara_baselines |>
  ggplot(aes(x = Ages, y = HSUV, fill = forcats::fct_rev(Source))) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = Min, ymax = Max),
                position = position_dodge(0.9), width = 0.2) +
  facet_grid(rows = vars(Gender)) +
  
  coord_cartesian(ylim = c(0.5, 1)) +
  scale_fill_viridis_d(
    name = "Source",
    begin = 0.2, end = 0.8, option = "E",
    labels = c("Kind et al. (1999)", "Szende et al. (2014)")
    ) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        legend.position = "bottom")

ggsave("output/1-literature/fig-01--baselines.png", height = 5, width = 7)

# Figure 2: Parametric Baselines ------------------------------------------------
para_baselines |> 
  ggplot(aes(x=Age, y=Index, colour=Source)) + 
  geom_line(linewidth = 0.7) +
  facet_grid(rows = vars(Sex)) +

  theme_bw() +
  scale_colour_viridis_d(
    begin = 0.2, end = 0.8, option = "E",
    labels = c("Ara & Brazier (2010)", "Hernandez Alava et al. (2022)")
  ) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  ) +
  coord_cartesian(ylim = c(0.6, 0.95)) +
  labs(y = "HSUV")
  
ggsave("output/1-literature/fig-02--baselines.png", height = 5, width = 7)
