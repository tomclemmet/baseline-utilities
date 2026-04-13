# K FOLD CROSS VALIDATION -----------------------------------------------------

# This file implements k-fold cross validation for the selected models. The
# output is a table with predictions errors for every observation for every
# model, giving maximum flexibility for analysing the results. The output file
# is saved as `kfold-errs.csv` in the `output/3-results` folder so that results 
# can be loaded without re-running the analysis.
# Note that this file may take some time to run due to the complexity of 
# fitting the ALDVMM model.

# Loading packages/data and other setup  --------------------------------------
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(rsample)
library(rms)
library(aldvmm)


hse <- read_csv("Data/hse-5L.csv", show_col_types = FALSE) |> 
  mutate(
    agegrp = cut(
      age, 
      breaks = c(0, 30, 50, 70, Inf), 
      labels = c("Age under 30 (n = 2,078)", "Age 30 to 49 (n = 4,692)", "Age 50 to 69 (n = 4,897)", "Age 70 and over (n = 2,746)"),
      right = FALSE,
      include.lowest = TRUE
    )
  )

theme_set(theme_bw())
set.seed(64813465)

# Defining the list of models to test -----------------------------------------
models <- list(
  Linear = \(x) lm(index ~ age, data = x),
  Quadratic = \(x) lm(index ~ poly(age, 2, raw = TRUE), data = x),
  `Poly-3` = \(x) lm(index ~ poly(age, 3, raw = TRUE), data = x),
  `Poly-4` = \(x) lm(index ~ poly(age, 4, raw = TRUE), data = x),
  `Poly-5` = \(x) lm(index ~ poly(age, 5, raw = TRUE), data = x),
  `Poly-6` = \(x) lm(index ~ poly(age, 6, raw = TRUE), data = x),
  `Poly-7` = \(x) lm(index ~ poly(age, 7, raw = TRUE), data = x),
  `Poly-8` = \(x) lm(index ~ poly(age, 8, raw = TRUE), data = x),
  `Poly-9` = \(x) lm(index ~ poly(age, 9, raw = TRUE), data = x),
  `Poly-10` = \(x) lm(index ~ poly(age, 10, raw = TRUE), data = x),
  `ALDVMM-1` = \(x) suppressMessages(aldvmm(index ~ I(age/10) + I((age/10)^2) | I(age/10), data = x,
                       psi = c(0.968, -0.567), ncmp = 1, optim.method = "Rvmmin")),
  `ALDVMM-2` = \(x) aldvmm(index ~ I(age/10) + I((age/10)^2) | I(age/10), data = x,
                       psi = c(0.968, -0.567), ncmp = 2, optim.method = "Rvmmin"),
  `RCS-3` = \(x) lm(index ~ rcs(age, 3), data = x),
  `RCS-4` = \(x) lm(index ~ rcs(age, 4), data = x),
  `RCS-5` = \(x) lm(index ~ rcs(age, 5), data = x),
  `RCS-6` = \(x) lm(index ~ rcs(age, 6), data = x),
  `RCS-7` = \(x) lm(index ~ rcs(age, 7), data = x),
  `RCS-8` = \(x) lm(index ~ rcs(age, 8), data = x),
  `RCS-9` = \(x) lm(index ~ rcs(age, 9), data = x),
  `RCS-10` = \(x) lm(index ~ rcs(age, 10), data = x)
)

# K-fold cross validation -----------------------------------------------------
k <- 10

results <- list(
  errors = tibble(),
  folds = list(),
  models = list()
)

for (s in c("Male", "Female")) {
  
  results$folds[[s]] <- vfold_cv(
    filter(hse, sex == s), 
    v = k, 
    strata = agegrp
  )
  
  for (label in names(models)) {
    r <- lapply(
      results$folds[[s]]$splits,
      function (x) {
        train <- analysis(x)
        test <- assessment(x)
        
        model <- models[[label]](train)
        
        if (substr(label, 1, 6) == "ALDVMM") {
          message(paste("\nALDVMM fitted, AIC =", model$gof$aic))
          train$Predicted <- model$pred$yhat
        } else {
          train$Predicted <- predict(model, train)
        }
        
        test <- train |> 
          group_by(age) |> 
          summarise(Predicted = mean(Predicted)) |> 
          right_join(test, by = "age")
        
        list(
          act = test$index,
          pred = test$Predicted,
          errs = test$index - test$Predicted,
          age = test$age,
          mod = summary(model)
        )
      }
    )

    for (i in 1:k) {
      results$errors <- results$errors |> 
        bind_rows(
          tibble(
            Model = as.factor(label),
            Fold = as.factor(paste(s, i, sep = "-")),
            sex = as.factor(s),
            age = r[[i]]$age,
            Actual = r[[i]]$act,
            Predicted = r[[i]]$pred,
            Error = r[[i]]$errs
          )
        )
      
      results$models[[paste(label, s, i, sep = "-")]] <- r[[i]]$mod
    }
    
    message(".", appendLF = FALSE)
  }
}

write_csv(results$errors, "output-5L/3-results/kfold-results.csv")
sink("output-5L/3-results/models.txt")
results$models
sink(NULL)
