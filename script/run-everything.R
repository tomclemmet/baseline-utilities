# Runtime approx 1 hr 20 mins
# Intel i3, 4GB RAM

rm(list = ls())

if (! require("pacman")) install.packages("pacman")

files <- c(
  "script/tables-and-figures/1-literature.R",
  "script/load-hse.R",
  "script/tables-and-figures/2-data.R",
  "script/kfold.R",
  "script/tables-and-figures/3-results.R",
  "script/tables-and-figures/4-discussion.R"
)

for (name in files) {
  
  message(paste(name, "..."))
  source(name)
  
}

rm(list = ls())