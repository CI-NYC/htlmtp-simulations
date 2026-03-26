library(purrr)
library(glue)

# Source path depending whether or not on computing cluster
# Relies on the `history-of-treatment` branch
if (Sys.info()["sysname"] == "Darwin") {
  library(lmtp, lib.loc = ".")
  source("R/simdata.R")
} else {
  library(lmtp, lib.loc = "..")
  source("../R/simdata.R")
}

id <- Sys.getenv("SLURM_ARRAY_TASK_ID")
if (id == "undefined" || id == "") id <- 1

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  args <- list(250)
}

# [250, 500, 1000, 5000]
n <- as.numeric(args[[1]])

data <- simdata(n)

# Parametric models so don't need cross-fitting
folds <- 1

set.seed(id)

sdr <- htlmtp_sdr(
  data,
  trt = paste0("a", 1:5),
  outcome = paste0("y", 1:5),
  baseline = "l0",
  time_vary = lapply(1:5, \(t) paste0("l", t)),
  k = Inf,
  outcome_type = "survival",
  learners_outcome = "SL.glm.interaction",
  learners_trt = "SL.glm",
  learners_cens = "SL.glm",
  folds = folds
)

set.seed(id)

tmle <- htlmtp_tmle(
  data,
  trt = paste0("a", 1:5),
  outcome = paste0("y", 1:5),
  baseline = "l0",
  time_vary = lapply(1:5, \(t) paste0("l", t)),
  k = Inf,
  outcome_type = "survival",
  learners_outcome = "SL.glm.interaction",
  learners_trt = "SL.glm",
  learners_cens = "SL.glm",
  folds = folds
)


res <- purrr::map_dfr(list("TMLE" = tmle, "SDR" = sdr), tidy, .id = "alg")

saveRDS(res, glue("../data/sim_{n}_{id}.rds"))
