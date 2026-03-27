# LOADING THE HSE -------------------------------------------------------------

# This script loads the .tab files for each desired HSE round, selects the
# relevant variables, joins the datasets together, and adds the TTO scores.

# Loading packages ------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
source("script/5L/eq5d5l.R")
# Loading 2017 and 2018 HSE ----------------------------------------------------

hse <- bind_rows(
  `2017` = read_tsv("raw-data/hse17i_eul_v3.tab", name_repair = tolower),
  `2018` = read_tsv("raw-data/hse_2018_eul_15082022.tab", name_repair = tolower)
) |> 
  select(hseyr, age16g5, mobil17, selfca17, usuala17, pain17, anxiet17, sex) |> 
  filter(! if_any(c(mobil17, selfca17, usuala17, pain17, anxiet17, sex, age16g5), ~. %in% c(-9, -8, -1))) |> 
  mutate(age = recode_values(
      age16g5,
      1 ~ mean(c(16, 17)),
      2 ~ mean(c(18, 19)),
      3 ~ mean(c(20, 24)),
      4 ~ mean(c(25, 29)),
      5 ~ mean(c(30, 34)),
      6 ~ mean(c(35, 39)),
      7 ~ mean(c(40, 44)),
      8 ~ mean(c(45, 49)),
      9 ~ mean(c(50, 54)),
      10 ~ mean(c(55, 59)),
      11 ~ mean(c(60, 64)),
      12 ~ mean(c(65, 69)),
      13 ~ mean(c(70, 74)),
      14 ~ mean(c(75, 79)),
      15 ~ mean(c(80, 84)),
      16 ~ mean(c(85, 89)),
      17 ~ 92.5 # Following approach of Hernandez Alava et al. (2022), using the average age for over 90s in the 2013 HSE
  ),
  age16g5 = recode_values(
    age16g5,
    1 ~ "16-17",
    2 ~ "18-19",
    3 ~ "20-24",
    4 ~ "25-29",
    5 ~ "30-34",
    6 ~ "35-39",
    7 ~ "40-44",
    8 ~ "45-49",
    9 ~ "50-54",
    10 ~ "55-59",
    11 ~ "60-64",
    12 ~ "65-69",
    13 ~ "70-74",
    14 ~ "75-79",
    15 ~ "80-84",
    16 ~ "85-89",
    17 ~ "90+"
  ),
  sex = recode_values(sex, 1 ~ "Male", 2 ~ "Female"),
  index = eq5d5l(mobil17, selfca17, usuala17, pain17, anxiet17)
)

write_csv(hse, "data/hse-5L.csv")
