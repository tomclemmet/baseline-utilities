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


hse <- read_csv("Data/hse-5L.csv", show_col_types = FALSE)

theme_set(theme_bw())
set.seed(84848484)
aldvmm.coefs <- c(
  1, -0.01, -0.001, 1, -0.01, -0.001, 1, -0.01, -0.001,
  0, 0, 0, 0, -2, -2, -2
)

# Defining the list of models to test -----------------------------------------
models <- list(
  Ones = \(x) NULL,
  Mean = \(x) lm(index ~ 1, data = x),
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
  ALDVMM = \(x) aldvmm(index ~ I(age/10) + I((age/10)^2) | I(age/10), data = x,
                       psi = c(1, -0.285), ncmp = 3,
                       init.est = aldvmm.coefs, optim.method = "nlminb"),
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
    strata = age, 
    breaks = 10
  )
  
  for (label in names(models)) {
    lapply(
      results$folds[[s]]$splits,
      function (x) {
        train <- analysis(x)
        test <- assessment(x)
        
        model <- models[[label]](train)
        
        if (label == "ALDVMM") {
          message(paste("\nALDVMM fitted, AIC =", model$gof$aic))
          train$Predicted <- model$pred$yhat
        } else if (label == "Ones") {
          train$Predicted <-  1
        }
        else {
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
    ) -> r

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
