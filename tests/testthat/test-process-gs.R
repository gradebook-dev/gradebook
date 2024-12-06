test_that("drop ungraded assignments - no ungraded", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
  
  attr(data, "source") <- "Gradescope"
  actual <- drop_ungraded_assignments(data)
  
  expect_equal(actual, data)
})

test_that("drop ungraded assignments - works correctly", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Lab 3` = c(NA, NA, NA, NA),
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
  attr(data, "source") <- "Gradescope"
  actual <- drop_ungraded_assignments(data)
  
  expected <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  attr(expected, "source") <- "Gradescope"
  expect_equal(actual, expected)
})

test_that("Test for warning if no source attr set", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Lab 3` = c(NA, NA, NA, NA),
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
  
  actual <- suppressWarnings({process_gs(data)})
  
  expected <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  expect_equal(actual, expected)
  
  expect_warning(process_gs(data))

})

test_that("Test for warning when source attr set", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Lab 3` = c(NA, NA, NA, NA),
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
  attr(data, "source") <- "Gradescope"
  actual <- process_gs(data)
  
  expected <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
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
    
    `Project 1` = c(0.9, 0, 0.4, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(expected, "source") <- "Gradescope"
  
  expect_equal(actual, expected)
  
  expect_no_warning(process_gs(data))
  
})