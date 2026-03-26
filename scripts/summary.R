library(dplyr)
library(glue)
library(kableExtra)

source("R/helpers.R")

devtools::source_gist("https://gist.github.com/nt-williams/3afb56f503c7f98077722baf9c7eb644")

truth <- 0.524

summary <- function(n) {
  res <-
    read_zip_rds(glue("data/sim_{n}.zip")) |>
    bind_rows()

  group_by(res, alg) |>
    summarise(bias = abs(mean(estimate) - truth),
              mse = mean((estimate - truth)^2),
              coverage = coverage(.data, truth)) |>
    mutate(n = n, .before = "alg") |>
    mutate(nmse = n * mse)
}

res <- 
  lapply(c(250, 500, 1000, 5000, 10000), summary) |>
  bind_rows()

select(res, alg, bias, mse, nmse, coverage) |> 
  mutate(across(c(bias, mse, nmse), \(x) round(x, 3))) |> 
  kbl(format = "latex", booktabs = TRUE, 
      align = c("lcccc"), 
      col.names = c("Estimator", "$\\left|\\text{Bias}\\right|$", "MSE", "$n \\times \\text{MSE}$", "95\\% Cov."), 
    escape = FALSE) |> 
  pack_rows("N = 250", 1, 2) |> 
  pack_rows("N = 500", 3, 4) |> 
  pack_rows("N = 1,000", 5, 6) |> 
  pack_rows("N = 5,000", 7, 8) |> 
  pack_rows("N = 10,000", 9, 10)
