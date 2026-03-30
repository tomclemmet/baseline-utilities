rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
library(aldvmm)
theme_set(theme_classic())

# Load data
hse <- read_csv("Data/hse-5L.csv", show_col_types = FALSE)
hse_by_sex <- list(
  male = filter(hse, sex == "Male"),
  female = filter(hse, sex == "Female")
)

# Define ALDVMM options
optims <- c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "nlminb", "Rcgmin", "Rvmmin", "hjn")
inits <- c("zero", "constant", "sann")
sex <- c("male", "female")

# ALDVMM test function, returning table of AICs
aldvmm_test <- function(sex, n_comp) {
  aics <- data.frame(matrix(ncol = 4, nrow = 8))
  colnames(aics) <- c("method", inits)
  aics$method <- optims
  for (i in 1:length(optims)) {
    for (j in 1:length(inits)) {
      model <- aldvmm(
        index ~ I(age/10) + I((age/10)^2) | I(age/10),
        data = hse_by_sex[[sex]],
        psi = c(0.968, -0.567),
        ncmp = n_comp,
        optim.method = optims[i],
        init.method = inits[j]
        )
      aics[i, j+1] <- model$gof$aic[[1]]
      message(".", appendLF = FALSE)
    }
  }
  export <- list()
  export[[paste(sex, n_comp, "component")]] <- aics
  write_xlsx(export, "output-5L/2-data/aldvmm-checks.xlsx")
}

for (sex in 1:2) {
  for (n in 1:3) {
    aldvmm_test(sex, n)
  }
}
