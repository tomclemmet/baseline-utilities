rm(list = ls())
library(dplyr)
library(readr)
library(ggplot2)
library(aldvmm)
library(writexl)

# Load data
hse <- read_csv("Data/hse-5L.csv", show_col_types = FALSE)
hse_by_sex <- list(
  male = filter(hse, sex == "Male"),
  female = filter(hse, sex == "Female")
)

# Define ALDVMM options
optims <- c("Nelder-Mead", "BFGS", "L-BFGS-B", "nlminb", "Rcgmin", "Rvmmin")
inits <- c("zero", "constant", "custom")
sex <- c("male", "female")

# Custom starting values taken from Hernandez Alava et al. (2022) NICE DSU report model
coefs <- list(
  c(0.5660, -0.1609, 0.0129, log(0.1140)),
  c(0.5660, -0.1609, 0.0129, 0.8502, -0.0080, -0.0007, -4.3366, 0.2804, log(0.1140), log(0.0694)),
  c(0.5660, -0.1609, 0.0129, 0.8502, -0.0080, -0.0007, 1.8194, -0.0316, -0.0051, -4.3366, 0.2804, -1.5277, 0.1445, log(0.1140), log(0.0694), log(0.5402))
  )


# ALDVMM test function, returning table of AICs
aldvmm_test <- function(sex, n_comp) {
  aics <- data.frame(matrix(ncol = length(inits) + 1, nrow = length(optims)))
  colnames(aics) <- c("method", inits)
  aics$method <- optims
  for (i in 1:length(optims)) {
    for (j in 1:length(inits)) {
      tic()
      if (j == 3) {
        model <- aldvmm(
          index ~ I(age/10) + I((age/10)^2) | I(age/10),
          data = hse_by_sex[[sex]],
          psi = c(0.968, -0.567),
          ncmp = n_comp,
          optim.method = optims[i],
          init.est = coefs[[n_comp]]
        ) |> suppressMessages()
      } else {
        model <- aldvmm(
          index ~ I(age/10) + I((age/10)^2) | I(age/10),
          data = hse_by_sex[[sex]],
          psi = c(0.968, -0.567),
          ncmp = n_comp,
          optim.method = optims[i],
          init.method = inits[j]
          ) |> suppressMessages()
      }
      aics[i, j+1] <- model$gof$aic[[1]]
      message <- paste(optims[i], inits[j], toc(quiet = TRUE)$callback_msg)
      message(message)
    }
  }
  print(aics)
  export <- list()
  export[[paste(sex, n_comp, "component")]] <- aics
  write_xlsx(export, "output-5L/2-data/aldvmm-checks.xlsx")
}

for (sex in 1:2) {
  for (n in 1:3) {
    aldvmm_test(sex, n)
  }
}
