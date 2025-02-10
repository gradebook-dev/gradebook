test_that("reconcile policy with gs - correct format", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- flatten_policy(policy)
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - drop last category", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- policy
  expected$categories <- expected$categories[1:3]
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - drop two assignments, not whole category", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- policy
  expected[["categories"]][[2]][["assignments"]] <- c("Lab 2.1")
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - drop one nested category", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- policy
  expected$categories[[2]] <- NULL
  expected[["categories"]][[2]][["assignments"]] <- c("Lab 1")
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - two dropped cats with weighted_mean", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Quiz 1")
    ),
    list(
      category = "Overall Grade",
      score = "raw_over_max",
      aggregation = "weighted_mean",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weights = c(0.5, 0.3, 0.2),
      assignments = c("Labs", "Quizzes", "Final Exam")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- flatten_policy(policy)
  expected$categories[[4]] <- NULL
  expected[["categories"]][[2]][["assignments"]] <- c("Lab 2.1")
  expected[["categories"]][[4]][["weights"]] <- 0.5
  expected[["categories"]][[4]][["assignments"]] <- "Labs"
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - one dropped cats with weighted_mean", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Quiz 1")
    ),
    list(
      category = "Overall Grade",
      score = "raw_over_max",
      aggregation = "weighted_mean",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weights = c(0.5, 0.3, 0.2),
      assignments = c("Labs", "Quizzes", "Final Exam")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0, 0.4, 0.95, 0.65),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- policy
  expected[["categories"]][[2]][["assignments"]] <- c("Lab 2.1")
  expected[["categories"]][[5]][["weights"]] <- c(.5, .3)
  expected[["categories"]][[5]][["assignments"]] <- c("Labs", "Quizzes")
  expect_equal(actual, expected)
  
})

test_that("reconcile policy with gs - drop two nested categories", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  
  actual <- reconcile_policy_with_gs(policy, gs)
  expected <- list(
    categories = list(
      list(
        category = "Quizzes",
        score = "raw_over_max",
        aggregation = "weighted_by_points",
        aggregation_max_pts = "sum_max_pts",
        aggregation_lateness = "max_lateness",
        weight = 0.50,
        assignments = c("Quiz 1")
      )
    )
  )
  expect_equal(actual, expected)
})

test_that("reconcile policy with gs - no assignments in gs", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  gs <- data.frame()
  
  expect_error(supressWarnings(reconcile_policy_with_gs(policy, gs)))
  
})

test_that("reconcile policy with gs - add defaults with equally_weighted/weighted_by_points",{
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  
  actual <- reconcile_policy_with_gs(policy, gs)
  expected_cat <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 1.1", "Lab 1.2"),
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3"),
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      assignments = c("Lab 1", "Lab 2"),
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      assignments = "Quiz 1",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    )
  )
  
  expect_equal(actual$categories, expected_cat)
})

test_that("reconcile policy with gs - add defaults with min_score and max_score",{
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "max_score",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "max_score",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  
  actual <- reconcile_policy_with_gs(policy, gs)
  expected_cat <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "max_score",
      assignments = c("Lab 1.1", "Lab 1.2"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "max_score",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = c("Lab 1", "Lab 2"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      assignments = "Quiz 1",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    )
  )
  
  expect_equal(actual$categories, expected_cat)
})

test_that("reconcile policy with gs - add score key to categories with gs assignments",{
  categories <- list(
    list(
      category = "Lab 1",
      aggregation = "max_score",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      aggregation = "max_score",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(gs, "source") <- "Gradescope"
  
  actual <- reconcile_policy_with_gs(policy, gs)
  expected_cat <- list(
    list(
      category = "Lab 1",
      aggregation = "max_score",
      assignments = c("Lab 1.1", "Lab 1.2"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Lab 2",
      aggregation = "max_score",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = c("Lab 1", "Lab 2"),
      aggregation_max_pts = "mean_max_pts",
      aggregation_lateness = "max_lateness"
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      assignments = "Quiz 1",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness"
    )
  )
  
  expect_equal(actual$categories, expected_cat)
})

test_that("flatten policy - works correctly", {
  categories <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "equally_weighted",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "equally_weighted",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  actual <- flatten_policy(policy)
  
  expected <- list(
    list(
      category = "Lab 1",
      aggregation = "equally_weighted",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      aggregation = "equally_weighted",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  expect_equal(actual$categories, expected)
  
})

test_that("flatten policy - unnested policy", {
  categories <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2", "Lab 3")
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      weight = 0.50,
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  actual <- flatten_policy(policy)
  
  expect_equal(actual, policy)
  
})

test_that("Canvas Data and Lateness Policy", {
  data <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 5, 6, 7, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, 2, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 12, 31),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, 45, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  
  attr(data, "source") <- "Canvas"
  
  categories <- list(
    list(
      category = "Homework",
      assignments = c("HW 1 (867568)", "HW 2 (867573)"),
      aggregation = "equally_weighted",
      lateness = list(
        until = "02:10:00",
        scale_by = 0.85,
        after = "22:10",
        set_to = 0.4
      )
    ),
    list(
      category = "Exams",
      assignments = c("Midterm (867589)", "Final (345678)"),
      aggregation = "weighted_by_points"
    ),
    list(
      category = "Overall Grade",
      assignments = c("Homework", "Exams"),
      aggregation = "equally_weighted"
    )
  )
  
  pol <- list(categories = categories)
  
  expect_error(reconcile_policy_with_gs(pol, data))
  
  
})

test_that("Canvas Data with No Lateness", {
  data <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 5, 6, 7, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, 2, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 12, 31),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, 45, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  
  attr(data, "source") <- "Canvas"
  
  categories <- list(
    list(
      category = "Homework",
      assignments = c("HW 1 (867568)", "HW 2 (867573)"),
      aggregation = "equally_weighted"
    ),
    list(
      category = "Exams",
      assignments = c("Midterm (867589)", "Final (345678)"),
      aggregation = "weighted_by_points"
    ),
    list(
      category = "Overall Grade",
      assignments = c("Homework", "Exams"),
      aggregation = "equally_weighted"
    )
  )
  
  pol <- list(categories = categories)
  
  expect_no_error(reconcile_policy_with_gs(pol, data))
  
})


test_that("reconcile policy with gs - correct format with no source attr", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(1, 0, 0.9, 0.5),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(1, 0, 0.9, 0.5),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  expect_error(expect_warning(reconcile_policy_with_gs(policy, gs)))
  
  
})


test_that("Gradescope Data and Lateness Policy", {
  data <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 5, 6, 7, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, 2, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 12, 31),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, 45, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  attr(data, "source") <- "Gradescope"
  
  categories <- list(
    list(
      category = "Homework",
      assignments = c("HW 1 (867568)", "HW 2 (867573)"),
      aggregation = "equally_weighted",
      lateness = list(
        until = "02:10:00",
        scale_by = 0.85,
        after = "22:10",
        set_to = 0.4
      )
    ),
    list(
      category = "Exams",
      assignments = c("Midterm (867589)", "Final (345678)"),
      aggregation = "weighted_by_points"
    ),
    list(
      category = "Overall Grade",
      assignments = c("Homework", "Exams"),
      aggregation = "equally_weighted"
    )
  )
  
  pol <- list(categories = categories)
  
  expect_no_error(reconcile_policy_with_gs(pol, data))
  
 
})

test_that("Test reconcile when a student has all excused for some category - multiple students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(NA, 0, 0.9, NA),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs), 
               regexp = "Student(s) 3032412514, 3032412517 have no unexcused assignments in category Lab 1.
Manually set grades for any effected students.",
               fixed = TRUE
  )
})

test_that("Test reconcile_policy_with_gs when a student has all excused for some category - one students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(NA, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs),
               regexp = "Student(s) 3032412514 have no unexcused assignments in category Lab 1.
Manually set grades for any effected students.",
               fixed = TRUE
  )
})

test_that("Test reconcile_policy_with_gs when a student has all excused for some category - one students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.7, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(0.3, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(NA, NA, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(NA, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(NA, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs),
               regexp = "Student(s) 3032412514 have no unexcused assignments in category Lab 2.
Manually set grades for any effected students.",
               fixed = TRUE
  )
})

test_that("Test reconcile_policy_with_gs when a student has all excused for some category - one students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.2, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(NA, NA, NA, NA),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, NA, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs),
               regexp = "Student(s) 3032122516 have no unexcused assignments in category Quizzes.
Manually set grades for any effected students.",
               fixed = TRUE
  )
})

test_that("Test reconcile_policy_with_gs when a student has all gs assignments excused for some category but not the subcats", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Wonky Cat",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2", "Lab 2.3")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.2, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(NA, NA, NA, NA),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0.2, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_no_warning(reconcile_policy_with_gs(policy, gs))
})

test_that("Test reconcile_policy_with_gs - clean", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.2, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(0.9, 0, 0.9, 1),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(1, 1, 0, 0.2),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0.6, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_no_warning(reconcile_policy_with_gs(policy, gs))
})

test_that("Test reconcile when a student has less un-excused than drops for some category - multiple students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      drop_n_lowest = 1,
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      drop_n_lowest = 0,
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.1, 0, 0.9, NA),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, 0.7),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(NA, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, 0, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs), 
                 regexp = "Student(s) 3032412514, 3032412517 will not receive full drops in category Lab 1.
Manually set grades for any effected students.",
                 fixed = TRUE
  )
})

test_that("Test reconcile when a student has less un-excused than drops for 2 categories - multiple students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      drop_n_lowest = 1,
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      drop_n_lowest = 1,
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.1, 0, 0.9, NA),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, 0.7),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0.8, NA, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, NA),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(0, NA, 0.9, NA),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs), 
                 regexp = "Student(s) 3032412514, 3032412517 will not receive full drops in category Lab 1.
Student(s) 3032122516, 3032412517 will not receive full drops in category Lab 2.
Manually set grades for any effected students.",
                 fixed = TRUE
  )
})

test_that("Test reconcile when a student has all excused for two category - multiple students", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories)
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(NA, 0, 0.9, NA),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(NA, 0, 0.9, NA),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(NA, NA, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(NA, NA, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(NA, NA, 0.9, 0.5),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_warning(reconcile_policy_with_gs(policy, gs), 
                 regexp = "Student(s) 3032412514, 3032412517 have no unexcused assignments in category Lab 1.
Student(s) 3032412514, 3032122516 have no unexcused assignments in category Lab 2.
Manually set grades for any effected students.",
                 fixed = TRUE
  )
})

test_that("Test reconcile_policy_with_gs - clean for slip days", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories, slip_days = list(
    name = "Slip Days 1",
    num_slip_days = 5,
    order = "chronological",
    assignments = c("Lab 1", "Lab 2", "Lab 3")
  ))
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.2, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(0.9, 0, 0.9, 1),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(1, 1, 0, 0.2),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0.6, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Gradescope"
  
  expect_no_error(reconcile_policy_with_gs(policy, gs))
})

test_that("Test reconcile_policy_with_gs - clean for slip days", {
  categories <- list(
    list(
      category = "Lab 1",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 1.1", "Lab 1.2")
    ),
    list(
      category = "Lab 2",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
    ),
    list(
      category = "Labs",
      score = "raw_over_max",
      aggregation = "equally_weighted",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quizzes",
      score = "raw_over_max",
      aggregation = "weighted_by_points",
      aggregation_max_pts = "sum_max_pts",
      aggregation_lateness = "max_lateness",
      weight = 0.50,
      assignments = c("Quiz 1")
    )
  )
  
  policy <- list(categories = categories, slip_days = list(
    name = "Slip Days 1",
    num_slip_days = 5,
    order = "chronological",
    assignments = c("Lab 1", "Lab 2", "Lab 3")
  ))
  
  gs <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1.1` = c(0.2, 0, 0.9, 0.4),
    `Lab 1.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                    "1/19/2023 10:00:00 AM", "0"),
    `Lab 1.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 1.2` = c(0.9, 0, 0.9, 1),
    `Lab 1.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 1.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 1.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.1` = c(0, 0, 0.9, 0.5),
    `Lab 2.1 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.1 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.2` = c(1, 0, 0.9, 0.5),
    `Lab 2.2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                    "1/20/2023 10:00:00 AM", "0"),
    `Lab 2.2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2.3` = c(1, 1, 0, 0.2),
    `Lab 2.3 - Max Points` = c(1, 1, 1, 1),
    `Lab 2.3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                    "1/21/2023 9:50:00 AM"),
    `Lab 2.3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Quiz 1` = c(0.9, 0.6, 0.4, 0),
    `Quiz 1 - Max Points` = c(1, 1, 1, 1),
    `Quiz 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                   "1/22/2023 10:00:00 AM", "0"),
    `Quiz 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(gs, "source") <- "Canvas"
  
  expect_error(reconcile_policy_with_gs(policy, gs))
})
