# Runtime approx 1 hr 20 mins

rm(list = ls())

files <- c(
  "script/5L/tables-and-figures/5L-1-literature.R",
  "script/5L/5L-load-hse.R",
  "script/5L/tables-and-figures/5L-2-data.R",
  "script/5L/5L-kfold.R",
  "script/5L/tables-and-figures/5L-3-results.R",
  "script/5L/tables-and-figures/5L-4-discussion.R"
)

for (name in files) {
  
  message(paste(name, "..."))
  source(name)
  
}

rm(list = ls())