simdata <- function(n0) {
  n_times <- 5

  data <- data.frame(matrix(NA, nrow = n0, ncol = 3 * n_times))
  names(data) <- c(paste0('l', 1:n_times), paste0('a', 1:n_times), paste0('y', 1:n_times))

  l0 <- rnorm(n0)

  for (t in 1:n_times) {
    if (t == 1) {
      data[, 'l1'] <- 0.5 * l0 + rnorm(n0)
      p_a1 <- plogis(-1.5 + 0.3 * data$l1)
      data[, 'a1'] <- rbinom(n0, 1, p_a1)
      p_y1 <- plogis(-2 + 0.4 * data$l1 - 0.8 * data$a1)
      data[, 'y1'] <- rbinom(n0, 1, p_y1)
    } else {
      vaccinated <- rowSums(data[, paste0('a', 1:(t-1)), drop = FALSE]) > 0
      alive <- rowSums(data[, paste0('y', 1:(t-1)), drop = FALSE]) == 0
      at_risk <- !vaccinated & alive

      data[alive, paste0('l', t)] <- 0.5 * data[alive, paste0('l', t-1)] + rnorm(sum(alive))
      # data[!alive, paste0('l', t)] <- data[!alive, paste0('l', t-1)]

      data[vaccinated, paste0('a', t)] <- 1
      data[!alive, paste0('a', t)] <- vaccinated[!alive]
      if (sum(at_risk) > 0) {
        p_at <- plogis(-1.5 + 0.3 * data[at_risk, paste0('l', t)])
        data[at_risk, paste0('a', t)] <- rbinom(sum(at_risk), 1, p_at)
      }

      vaccinated_now <- rowSums(data[, paste0('a', 1:t), drop = FALSE]) > 0
      data[!alive, paste0('y', t)] <- 1
      if (sum(alive) > 0) {
        p_yt <- plogis(-2 + 0.4 * data[alive, paste0('l', t)] - 0.8 * vaccinated_now[alive])
        data[alive, paste0('y', t)] <- rbinom(sum(alive), 1, p_yt)
      }
    }
  }

  data$l0 <- l0
  data
}
