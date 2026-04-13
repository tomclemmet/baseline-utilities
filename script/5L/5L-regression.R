# This files fits ordered multinomial logistic models to each of the EQ-5D
# dimensions and uses the predictions to generate baseline utilities
# (work in progress)

library(dplyr)
library(readr)
library(ggplot2)
library(MASS)
library(aldvmm)
library(broom)
library(tidyr)
theme_set(theme_classic())

value_set <- list(
  mobility = c(0, 0.032, 0.058, 0.179, 0.279),
  selfcare = c(0, 0.038, 0.060, 0.162, 0.206),
  usualact = c(0, 0.049, 0.086, 0.184, 0.212),
  paindis = c(0, 0.056, 0.066, 0.371, 0.479),
  anxdep = c(0, 0.041, 0.126, 0.313, 0.391)
)

hse <- read_csv("Data/hse-5L.csv", show_col_types = FALSE) |> 
  mutate(across(mobil17:anxiet17, \(x) factor(x, levels = seq(1, 5))))

male <- filter(hse, sex == "Male")
female <- filter(hse, sex == "Female")

models <- list(
  m_mo = polr(mobil17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = male),
  m_ua = polr(usuala17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = male),
  m_sc = polr(selfca17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = male),
  m_pd = polr(pain17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = male),
  m_ad = polr(anxiet17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = male),
  f_mo = polr(mobil17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = female),
  f_ua = polr(usuala17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = female),
  f_sc = polr(selfca17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = female),
  f_pd = polr(pain17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = female),
  f_ad = polr(anxiet17 ~ I(age/10) + I((age/10)^2) + I((age/10)^3), data = female)
)

ages <- data.frame(age = seq(16, 100))
decs <- data.frame(age = ages)

for (i in 1:5) {
  decs[names(models)[i]] <- predict(models[[i]], newdata = decs, type = "probs") %*% value_set[[i]]
  decs[names(models)[i + 5]] <- predict(models[[i + 5]], newdata = ages, type = "probs") %*% value_set[[i]]
}

decs_agg <- decs |> 
  mutate(
    age = age,
    male = 1 - rowSums(pick(starts_with("m_"))),
    female = 1 - rowSums(pick(starts_with("f_"))),
    .keep = "none"
    ) |> 
  pivot_longer(male:female, names_to = "sex", values_to = "utility")

ggplot(decs_agg, aes(x = age, y = utility, colour = sex)) +
  geom_line()

decs |> 
  pivot_longer(m_mo:f_ad, names_to = "group", values_to = "decrement") |> 
  mutate(sex = if_else(substr(group, 1, 2) == "m_", "male", "female"), dim = substr(group, 3, 4)) |> 
  ggplot(aes(x = age, y = decrement)) +
  geom_line(aes(group = group, colour = sex, linetype = dim))
