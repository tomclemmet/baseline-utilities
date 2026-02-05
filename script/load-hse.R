# LOADING THE HSE -------------------------------------------------------------

# This script loads the .tab files for each desired HSE round, selects the
# relevant variables, joins the datasets together, and adds the TTO scores.

# Loading packages ------------------------------------------------------------
library(readr)
library(dplyr)
library(tidyr)
library(eq5d)

# Loading each HSE round, cleaning and combining ------------------------------

yrs <- c("03", "04", "05", "06", "08", "10", "11", "12", "14")

hse <- tibble()
for (i in yrs) {
  df <- read_tsv(paste("raw-data/hse", i, "ai.tab", sep = ""), 
                 show_col_types = FALSE)
  names(df) <- tolower(names(df))
  if (i == "14")  {
    df <- df |> 
      mutate(age = case_match(age90, 90 ~ 92.5, .default = age90))
  }
  
  df <- df |> 
    rename(Sex = sex, Age = age, MO = mobility, SC = selfcare, 
           UA = usualact, PD = pain, AD = anxiety) |> 
    select(Sex, Age, MO, SC, UA, PD, AD) |> 
    mutate(
      across(c(MO, SC, UA, PD, AD), as.factor),
      Year = as.numeric(paste("20", i, sep="")),
      Sex = as.factor(case_match(Sex, 2 ~ "Female", 1 ~ "Male"))
    ) |> 
    filter(! if_any(everything(), ~. %in% c(-9, -8, -2, -1)))

    hse <- hse |> bind_rows(df)
    message(paste(i, ""))
}
rm(df, i, yrs)

# Adding HSUVs using the UK's Dolan (1997) value set and saving --------------
hse$Index <- eq5d::eq5d(
  scores = hse[,3:7],
  version = "3L",
  country = "UK",
  type = "TTO"
)

write_csv(hse, "data/hse.csv")