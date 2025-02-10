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
                       `Lab 1 - Max Points` = rep(5, 5),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 2 - Max Points` = rep(10, 5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 3 - Max Points` = rep(15, 5),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9),
                       `Lab 4 - Max Points` = rep(20, 5)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 2, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, 0.8, NA),
                             `Lab 1 - Max Points` = c(NA, NA, NA, 5, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, 0.9, 0.95, 0.9),
                             `Lab 3 - Max Points` = c(NA, 15, 15, 15, 15),
                             `Lab 4` = c(0.2, 0.4, 1, NA, 0.9),
                             `Lab 4 - Max Points` = c(20, 20, 20, NA, 20),
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n == num of assignments", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 1 - Max Points` = rep(5, 5),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 2 - Max Points` = rep(10, 5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 3 - Max Points` = rep(15, 5),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9),
                       `Lab 4 - Max Points` = rep(20, 5)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 4, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, NA, NA),
                             `Lab 1 - Max Points` = c(NA, NA, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 3 - Max Points` = c(NA, 15, NA, 15, NA),
                             `Lab 4` = c(NA, NA, 1, NA, 0.9),
                             `Lab 4 - Max Points` = c(NA, NA, 20, NA, 20)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n > num of assignments", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 2 - Max Points` = rep(10,5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 3 - Max Points` = rep(15,5),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9),
                       `Lab 4 - Max Points` = rep(20,5)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 6, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, NA, NA),
                             `Lab 1 - Max Points` = c(NA, NA, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 3 - Max Points` = c(NA, 15, NA, 15, NA),
                             `Lab 4` = c(NA, NA, 1, NA, 0.9),
                             `Lab 4 - Max Points` = c(NA, NA, 20, NA, 20)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - one assignment", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 1 - Max Points` = rep(10,5)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 4, category = "Labs", 
                          assignments = c("Lab 1"))
  expected <- grades_mat
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - zero drops", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, 0.6, 0.8, 0.9),
                       `Lab 1 - Max Points` = rep(5,5),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, 0.9),
                       `Lab 2 - Max Points` = rep(10,5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.9),
                       `Lab 3 - Max Points` = rep(15,5),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9),
                       `Lab 4 - Max Points` = rep(20,5)
  )
  grades_mat <- data.matrix(gs)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 0, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- grades_mat
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n < num of assignments w/excused", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, NA, 0.8, 0.9),
                       `Lab 1 - Max Points` = rep(5, 5),
                       `Lab 2` = c(0.9, 0.4, 0.6, 0.7, NA),
                       `Lab 2 - Max Points` = rep(10, 5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, 0.8),
                       `Lab 3 - Max Points` = rep(15, 5),
                       `Lab 4` = c(0.2, NA, 1, 0.7, 0.7),
                       `Lab 4 - Max Points` = rep(20, 5)
  )
  grades_mat <- data.matrix(gs)
  assignments <- c("Lab 1", "Lab 2", "Lab 3", "Lab 4")
  grades_mat <- handle_excused(grades_mat, assignments)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 2, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, 0.8, 0.9),
                             `Lab 1 - Max Points` = c(NA, NA, NA, 5, 5),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 3 - Max Points` = c(NA, 15, NA, 15, NA),
                             `Lab 4` = c(0.2, NA, 1, NA, NA),
                             `Lab 4 - Max Points` = c(20, NA, 20, NA, NA),
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n == num of assignments w/ excused", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, NA, 0.8, 0.7),
                       `Lab 1 - Max Points` = rep(5, 5),
                       `Lab 2` = c(0.9, NA, 0.6, NA, 0.8),
                       `Lab 2 - Max Points` = rep(10, 5),
                       `Lab 3` = c(0.2, 0.5, 0.9, 0.95, NA),
                       `Lab 3 - Max Points` = rep(15, 5),
                       `Lab 4` = c(0.2, 0.4, 1, 0.7, 0.9),
                       `Lab 4 - Max Points` = rep(20, 5)
  )
  grades_mat <- data.matrix(gs)
  assignments <- c("Lab 1", "Lab 2", "Lab 3", "Lab 4")
  grades_mat <- handle_excused(grades_mat, assignments)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 3, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, NA, NA, NA, NA),
                             `Lab 1 - Max Points` = c(NA, NA, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, NA),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, NA),
                             `Lab 3` = c(NA, 0.5, NA, 0.95, NA),
                             `Lab 3 - Max Points` = c(NA, 15, NA, 15, NA),
                             `Lab 4` = c(NA, NA, 1, NA, 0.9),
                             `Lab 4 - Max Points` = c(NA, NA, 20, NA, 20)
  )
  
  expected <- data.matrix(expected)
  
  expect_equal(actual, expected)
})

test_that("drop_n_lowest function - n == num of assignments w/ excused 2", {
  gs <- tibble::tibble(`Lab 1` = c(0.2, 0.4, NA, 0.8, NA),
                       `Lab 1 - Max Points` = rep(5, 5),
                       `Lab 2` = c(0.9, NA, 0.6, NA, 0.8),
                       `Lab 2 - Max Points` = rep(10, 5),
                       `Lab 3` = c(0.2, NA, 0.9, 0.95, NA),
                       `Lab 3 - Max Points` = rep(15, 5),
                       `Lab 4` = c(0.2, NA, 1, 0.7, NA),
                       `Lab 4 - Max Points` = rep(20, 5)
  )
  grades_mat <- data.matrix(gs)
  assignments <- c("Lab 1", "Lab 2", "Lab 3", "Lab 4")
  grades_mat <- handle_excused(grades_mat, assignments)
  
  actual <- drop_n_lowest(grades_mat, policy_line = 3, category = "Labs", 
                          assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4"))
  expected <- tibble::tibble(`Lab 1` = c(NA, 0.4, NA, NA, NA),
                             `Lab 1 - Max Points` = c(NA, 5, NA, NA, NA),
                             `Lab 2` = c(0.9, NA, NA, NA, 0.8),
                             `Lab 2 - Max Points` = c(10, NA, NA, NA, 10),
                             `Lab 3` = c(NA, NA, NA, 0.95, NA),
                             `Lab 3 - Max Points` = c(NA, NA, NA, 15, NA),
                             `Lab 4` = c(NA, NA, 1, NA, NA),
                             `Lab 4 - Max Points` = c(NA, NA, 20, NA, NA)
  )
  
  expected <- data.matrix(expected)
  
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

test_that("aggregation function - equally_weighted with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- equally_weighted(grades_mat, category = "Labs", 
                             assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
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

test_that("aggregation function - weighted_by_points with one assignment", {
  gs <- tibble::tibble(`Lab 1` = (1:5)/5,
                       `Lab 1 - Max Points` = rep(5,5),
                       Labs = NA
  )
  grades_mat <- as.matrix(gs)
  
  actual <- weighted_by_points(grades_mat, category = "Labs", 
                             assignments = c("Lab 1"))
  expected <- grades_mat
  expected[, "Labs"] <- (1:5)/5
  
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
  
  expect_error(weighted_mean(grades_mat, category = "Overall_Grade", 
                             assignments = c("Labs", "Final_Exam"), weights = c(0.3, 0.5, 0.2)))
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

test_that("Testing handle_excused", {
  gs <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 5, 6, NA, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, 2, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 12, NA),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, 45, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  grades_mat <- gs |>
    # only assignment info
    select(-get_id_cols(gs)) |> 
    # convert lateness into minutes
    mutate_at(vars(ends_with(" - Lateness (H:M:S)")), convert_to_min) |> 
    # convert to matrix
    data.matrix()
  raw_cols <- get_assignments(gs)
  
  # rownames are SID of student
  rownames(grades_mat) <- gs$SID
  
  actual <- handle_excused(grades_mat, raw_cols)
  
  expected <- grades_mat
  
  expected["888763", "Midterm (867589) - Max Points"] <- NA
  expected["567812", "HW 1 (867568) - Max Points"] <- NA
  
 
  expect_equal(actual, expected)
})

test_that("Testing calculate_grades with UID in SID col", {
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c("98657", "UID:12345", "76589", "44567"),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  cats <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Project",
      assignments = "Project 1"
    )
  )
  
  pol <- list(categories = cats)
  
  expect_no_error(calculate_grades(gs, pol))
  
})

test_that("Testing calculate_grades without UID in SID col", {
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  cats <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Project",
      assignments = "Project 1"
    )
  )
  
  pol <- list(categories = cats)
  
  expect_no_error(calculate_grades(gs, pol))
  
})

test_that("Testing calculate_grades without UID in SID col", {
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c("98657", "12345", "76589", "44567"),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  cats <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Project",
      assignments = "Project 1"
    )
  )
  
  pol <- list(categories = cats)
  
  expect_no_error(calculate_grades(gs, pol))
  
})
test_that("Testing calculate_grades without UID in SID col", {
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, "12345", "76589", 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  cats <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Project",
      assignments = "Project 1"
    )
  )
  
  pol <- list(categories = cats)
  
  expect_no_error(calculate_grades(gs, pol))
  
})

test_that("Testing get_grades without UID in SID col", {
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, "12345", "76589", 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, 0.5),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  cats <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Project",
      aggregation = "equally_weighted",
      assignments = "Project 1"
    )
  )
  
  pol <- list(categories = cats)
  
  actual <- get_grades(gs, pol)
  
  expect_s3_class(actual, "tbl_df")
  
})


test_that("assignment exists in 2 categories -- compute correct score", {
 
  gs <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, "12345", "76589", 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    
    `Midterm` = c(40, 40, 40, 40),
    `Midterm - Max Points` = c(50, 50, 50, 50),
    `Midterm - Submission Time` = c("1/21/2023 9:25:00 AM", "1/21/2023 9:25:00 AM", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Midterm - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Final Exam` = c(90, 90, 90, 90),
    `Final Exam - Max Points` = c(100, 100, 100, 100),
    `Final Exam - Submission Time` = c("1/22/2023 9:25:00 AM", "1/22/2023 9:25:00 AM",
                                       "1/22/2023 10:00:00 AM", "1/22/2023 9:25:00 AM"),
    `Final Exam - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  cats <- list(
    
    coursewide = list(
      course_name = "Stat 101",
      description = "Fall Semester"
    ),
    categories = list(
      list(
        category = "Overall Grade",
        weight = 1.0,
        aggregation = "weighted_mean",
        assignments = list(
          list(
            category = "Midterm Clobbered",
            weight = 0.50,  
            aggregation = "max_score",
            assignments = c("Midterm", "Final Exam")
          ),
          list(
            category = "Final",
            weight = 0.50,
            aggregation = "equally_weighted",
            assignments = "Final Exam"
          )
        )
      )
    ),
    exceptions = list()
  )
  
  
  gs <- gs |> 
    process_gs()
  
  policy <- cats |>
    process_policy(verbose = TRUE) |>
    reconcile_policy_with_gs(gs = gs, verbose = TRUE)
  
  actual <- get_grades(gs=gs, policy=policy)
  actual_subset_of_clumns <- actual |>
    select('First Name','Last Name', 'SID', 'Email', 'Midterm Clobbered', 
           'Final', 'Overall Grade')
  
  expected <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    `SID` = c(98657, "12345", "76589", 44567),
    `Email` = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Midterm Clobbered` = c(0.9, 0.9, 0.9, 0.9),
    `Final` = c(0.9, 0.9, 0.9, 0.9),
    `Overall Grade` = c(0.9, 0.9, 0.9, 0.9)
  )
  
  expect_equal(actual_subset_of_clumns, expected)
  
})