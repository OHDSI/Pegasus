library(testthat)
library(shiny)

for (file in list.files("../R", pattern = "\\.R$", full.names = TRUE)) {
  source(file, local = FALSE)
}

test_dir("testthat")
