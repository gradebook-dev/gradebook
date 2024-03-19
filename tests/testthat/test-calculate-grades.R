test_that("score function - raw_over_max", {
  gs <- tibble::tibble(`Lab 1` = 1:5,
             `Lab 1 - Max Points` = rep(5,5)
             )
  grades_mat <- data.matrix(gs)
  
  actual <- raw_over_max(grades_mat, assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Lab 1"] <- expected[, "Lab 1"]/5
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n < num of assignments", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 2, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, 0.8, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, 0.9, 0.95, 0.9),
                             `Lab 4` = c(0.2, 0.4, 1, NA, 0.9)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n == num of assignments", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 4, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 4` = c(NA, NA, 1, NA, 0.9)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n > num of assignments", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 6, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 4` = c(NA, NA, 1, NA, 0.9)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - one assignment", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 4, category = "Labs", 
                          assignments = c("Lab 1"))
  expected <- grades_mat
  
  expect_equal(actual, expected)
})


test_that("aggregation function - equally_weighted", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- equally_weighted(grades_mat, category = "Labs", 
                               assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(31, 32, 33, 34, 35)/60
  
  expect_equal(actual, expected)
})

test_that("aggregation function - weighted_by_points", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                   `Lab 1 - Max Points` = rep(5,5),
                   `Lab 2` = (5:1)/6,
                   `Lab 2 - Max Points` = rep(6,5),
                   Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- weighted_by_points(grades_mat, category = "Labs", 
                         assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- 6/11
  
  expect_equal(actual, expected)
})

test_that("aggregation function - weighted_mean", {
  gs <- tibble::tibble(`Labs` = c(0.5, 0.4, 0.3, 0.8),
                       `Final_Exam` = c(0.8, 1.0, 0.2, 0.9),
                       Overall_Grade = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- weighted_mean(grades_mat, category = "Overall_Grade", 
                          assignments = c("Labs", "Final_Exam"), weights = c(0.3, 0.7))
  expected <- grades_mat
  expected[, "Overall_Grade"] <- c(.71, .82, .23, .87)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - weighted_mean if wrong number of weights", {
  gs <- tibble::tibble(`Labs` = c(0.5, 0.4, 0.3, 0.8),
                       `Final_Exam` = c(0.8, 1.0, 0.2, 0.9),
                       Overall_Grade = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- weighted_mean(grades_mat, category = "Overall_Grade", 
                          assignments = c("Labs", "Final_Exam"), weights = c(0.3, 0.5, 0.2))
  expected <- grades_mat
  expected[, "Overall_Grade"] <- c(0.65, 0.7, 0.25, 0.85)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - max_score", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_score(grades_mat, category = "Labs", 
                               assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(5/6, 4/6, 3/5, 4/5, 1)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - min_score", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- min_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <- c(1/5, 2/5, 3/6, 2/6, 1/6)
  
  expect_equal(actual, expected)
})

test_that("aggregation function - min_score with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- min_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})

test_that("aggregation function - max_score with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_score(grades_mat, category = "Labs", 
                      assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})

test_that("aggregation function - none with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- none(grades_mat, category = "Labs", 
                               assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
  expect_equal(actual, expected)
})


test_that("aggregation function - none with more than one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = (5:1)/6,
                       `Lab 2 - Max Points` = rep(6,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- none(grades_mat, category = "Labs", 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs"] <-c(31, 32, 33, 34, 35)/60
  
  expect_equal(actual, expected)
})

test_that("aggregation for max pts - sum_max_pts", {
  gs <- tibble::tibble(`Lab 1 - Max Points` = rep(5,5),
                       `Lab 2 - Max Points` = rep(6,5),
                       `Labs - Max Points`= NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- sum_max_pts(grades_mat, category = "Labs", 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs - Max Points"] <- rep(11, 5)
  
  expect_equal(actual, expected)
  
})

test_that("aggregation for max pts - sum_max_pts with only one assignment", {
  gs <- tibble::tibble(`Lab 1 - Max Points` = rep(5,5),
                       `Labs - Max Points`= NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- sum_max_pts(grades_mat, category = "Labs", 
                        assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs - Max Points"] <- rep(5, 5)
  
  expect_equal(actual, expected)
  
})

test_that("aggregation for max pts - mean_max_pts", {
  gs <- tibble::tibble(`Lab 1 - Max Points` = rep(5,5),
                       `Lab 2 - Max Points` = rep(6,5),
                       `Labs - Max Points`= NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- mean_max_pts(grades_mat, category = "Labs", 
                        assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs - Max Points"] <- rep(5.5, 5)
  
  expect_equal(actual, expected)
  
})

test_that("aggregation for max pts - mean_max_pts with one assignment", {
  gs <- tibble::tibble(`Lab 1 - Max Points` = rep(5,5),
                       `Labs - Max Points`= NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- mean_max_pts(grades_mat, category = "Labs", 
                         assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs - Max Points"] <- rep(5, 5)
  
  expect_equal(actual, expected)
  
})

test_that("lateness function - until", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- as.matrix(gs)
  
  actual <- until(grades_mat, late_policy = "01:00:00", original_late_mat = grades_mat, 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(1, 1, 1, 0, 0)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(1, 1, 1, 0, 0)
  
  expect_equal(actual, expected)
})

test_that("lateness function - between", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- as.matrix(gs)
  
  actual <- between(grades_mat, late_policy = list("00:30:00", "01:00:00"), original_late_mat = grades_mat, 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(0, 1, 1, 0, 0)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(0, 1, 1, 0, 0)
  
  expect_equal(actual, expected)
})

test_that("lateness function - between with backwards from and to", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- as.matrix(gs)
  
  actual <- between(grades_mat, late_policy = list("01:00:00", "00:30:00"), original_late_mat = grades_mat, 
                    assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(0, 1, 1, 0, 0)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(0, 1, 1, 0, 0)
  
  expect_equal(actual, expected)
})

test_that("lateness function - after", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- as.matrix(gs)
  
  actual <- after(grades_mat, late_policy = "01:00:00", original_late_mat = grades_mat, 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(0, 0, 1, 1, 1)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(0, 0, 0, 1, 1)
  
  expect_equal(actual, expected)
})

test_that("lateness function - add", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- tibble::tibble(`Lab 1` = c(0.75, 0.85, 0.9, 1.0 , 1.0),
                               `Lab 1 - Lateness (H:M:S)` = c(0, 0, 1, 1, 1),
                               `Lab 2` = c(0.70, 0.80, 0.95, 0.95 , 1.0),
                               `Lab 2 - Lateness (H:M:S)` = c(0, 0, 0, 1, 1)
  )
  grades_mat <- as.matrix(grades_mat)
  
  actual <- add(grades_mat, late_policy = 0.03, 
                  original_late_mat = as.matrix(gs), 
                  assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1"] <- c(0.75, 0.85, 0.93, 1.03 , 1.03)
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  expected[, "Lab 2"] <- c(0.70, 0.80, 0.95, 0.98, 1.03)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(10, 30, 50, 70, 90)
  
  expect_equal(actual, expected)
})

test_that("lateness function - scale_by", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- tibble::tibble(`Lab 1` = c(0.75, 0.85, 0.9, 1.0 , 1.0),
                               `Lab 1 - Lateness (H:M:S)` = c(0, 0, 1, 1, 1),
                               `Lab 2` = c(0.70, 0.80, 0.95, 0.95 , 1.0),
                               `Lab 2 - Lateness (H:M:S)` = c(0, 0, 0, 1, 1)
  )
  grades_mat <- as.matrix(grades_mat)
  
  actual <- scale_by(grades_mat, late_policy = 0.5, 
                original_late_mat = as.matrix(gs), 
                assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1"] <- c(0.75, 0.85, 0.45, 0.5 , 0.5)
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  expected[, "Lab 2"] <- c(0.70, 0.80, 0.95, 0.475, 0.5)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(10, 30, 50, 70, 90)
  
  expect_equal(actual, expected)
})

test_that("lateness function - set_to", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90)
  )
  grades_mat <- tibble::tibble(`Lab 1` = c(0.75, 0.85, 0.9, 1.0 , 1.0),
                               `Lab 1 - Lateness (H:M:S)` = c(0, 0, 1, 1, 1),
                               `Lab 2` = c(0.70, 0.80, 0.95, 0.95 , 1.0),
                               `Lab 2 - Lateness (H:M:S)` = c(0, 0, 0, 1, 1)
  )
  grades_mat <- as.matrix(grades_mat)
  
  actual <- scale_by(grades_mat, late_policy = 0, 
                     original_late_mat = as.matrix(gs), 
                     assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Lab 1"] <- c(0.75, 0.85, 0, 0 , 0)
  expected[, "Lab 1 - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  expected[, "Lab 2"] <- c(0.70, 0.80, 0.95, 0, 0)
  expected[, "Lab 2 - Lateness (H:M:S)"] <- c(10, 30, 50, 70, 90)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - sum_lateness", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- sum_lateness(grades_mat, category = "Labs", 
                     assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(30, 70, 110, 150, 190)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - sum_lateness with one assignment", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- sum_lateness(grades_mat, category = "Labs", 
                         assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - mean_lateness", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- mean_lateness(grades_mat, category = "Labs", 
                         assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(15, 35, 55, 75, 95)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - mean_lateness with one assignment", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- mean_lateness(grades_mat, category = "Labs", 
                          assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - max_lateness", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Lab 2 - Lateness (H:M:S)` = c(10, 30, 50, 70, 90),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_lateness(grades_mat, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  
  expect_equal(actual, expected)
})

test_that("aggregation for lateness function - max_lateness with one assignment", {
  gs <- tibble::tibble(`Lab 1 - Lateness (H:M:S)` = c(20, 40, 60, 80, 100),
                       `Labs - Lateness (H:M:S)` = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- max_lateness(grades_mat, category = "Labs", 
                          assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs - Lateness (H:M:S)"] <- c(20, 40, 60, 80, 100)
  
  expect_equal(actual, expected)
})

