# Relies on the `history-of-treatment` branch
library(lmtp, lib.loc = ".")
library(dplyr)

source("R/simdata.R")

data <- simdata(1e6)

task <- lmtp:::LmtpTask$new(
  data = data,
  shifted = NULL,
  A = paste0("a", 1:5),
  Y = paste0("y", 1:5),
  L = lapply(1:5, \(t) paste0("l", t)),
  W = "l0",
  C = NULL,
  D = NULL,
  k = Inf, id = NULL,
  outcome_type = "survival",
  folds = 1,
  weights = NULL,
  bounds = NULL
)

at_risk_time_5 <- task$natural$y4 == 1
m5 <- glm(y5 ~ a5 + l5, data = task$natural, subset = at_risk_time_5, family = "binomial")

augmented <- lmtp:::delay_augment(task$natural, task$sequences(4))

augmented$q5 <-
  predict(m5, mutate(augmented, a5 = lmtp:::one_time_delay(augmented, c("..i..lmtp_tmp_s4", "a5"))),
          type = "response")

augmented$q5[!at_risk_time_5] <- 0

augmented <- lmtp:::subset_augmented(augmented, 4, 5)

at_risk_time_4 <- augmented$y3 == 1
m4 <- glm(q5 ~ ..i..lmtp_tmp_s4*(a4 + l4), data = augmented, subset = at_risk_time_4)

augmented$q4 <- predict(m4, mutate(augmented,
                                   ..i..lmtp_tmp_s4 = a4,
                                   a4 = lmtp:::one_time_delay(augmented, c("..i..lmtp_tmp_s3", "a4"))))

augmented$q4[!at_risk_time_4] <- 0

augmented <- lmtp:::subset_augmented(augmented, 3, 5)

at_risk_time_3 <- augmented$y2 == 1
m3 <- glm(q4 ~ ..i..lmtp_tmp_s3*(a3 + l3), data = augmented, subset = at_risk_time_3)

augmented$q3 <- predict(m3, mutate(augmented,
                                   ..i..lmtp_tmp_s3 = a3,
                                   a3 = lmtp:::one_time_delay(augmented, c("..i..lmtp_tmp_s2", "a3"))))
augmented$q3[!at_risk_time_3] <- 0

augmented <- lmtp:::subset_augmented(augmented, 2, 5)

at_risk_time_2 <- augmented$y1 == 1
m2 <- glm(q3 ~ ..i..lmtp_tmp_s2*(a2 + l2), data = augmented, subset = at_risk_time_2)
augmented$q2 <- predict(m2, mutate(augmented,
                                   ..i..lmtp_tmp_s2 = a2,
                                   a2 = lmtp:::one_time_delay(augmented, c("..i..lmtp_tmp_s1", "a2"))))
augmented$q2[!at_risk_time_2] <- 0

augmented <- lmtp:::subset_augmented(augmented, 1, 5)
m1 <- glm(q2 ~ ..i..lmtp_tmp_s1*(a1 + l1 + l0), data = augmented)
augmented$q1 <- predict(m1, mutate(augmented,
                                   ..i..lmtp_tmp_s1 = a1,
                                   a1 = 0))
mean(augmented$q1)
