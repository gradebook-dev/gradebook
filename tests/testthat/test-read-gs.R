test_that("get assignments - several assignments", {
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
  
  actual <- get_assignments(data)
  
  expect_equal(actual, c("Lab 1", "Lab 2", "Lab 3", "Project 1"))
})

test_that("get assignments - one assignment", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Lab 1` = c(1, 0, 0.9, 0.5),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00")
  )
  
  actual <- get_assignments(data)
  
  expect_equal(actual, "Lab 1")
})

test_that("get assignments - no assignments", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Sections` = c("Class 001", "Class 001", "Class 001", "Class 001"),
    
    `Names` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson"),
    `Email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
                "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu")
  )
  
  actual <- get_assignments(data)
  
  expect_equal(actual, character(0))
})

test_that("get assignments - several assignments", {
  data <- tibble::tibble(
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
  
  actual <- get_assignments(data)
  
  expect_equal(actual, c("Lab 1", "Lab 2", "Lab 3", "Project 1"))
})

test_that("get id cols - several id cols", {
  data <- tibble::tibble(
    `SID` = c(3032412514, 3032122516, 3032412516,3032412517),
    `Sections` = c("Class 001", "Class 001", "Class 001", "Class 001"),
    
    `Names` = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson"),
    `Email` = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu",
                "robert.brown@berkeley.edu", "emily.johnson@berkeley.edu"),
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
  
  actual <- get_id_cols(data)
  
  expect_equal(actual, c("SID", "Sections", "Names", "Email"))
})

test_that("get id cols - one id cols", {
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
  
  actual <- get_id_cols(data)
  
  expect_equal(actual, "SID")
})

test_that("get id cols - no id cols", {
  data <- tibble::tibble(
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
  
  actual <- get_id_cols(data)
  
  expect_equal(actual, character(0))
  
})

test_that("Test Determine Grade Source - Canvas", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Porch, Holden", "Student, Test"),
    ID = c(NA, 546798, 454573, 789807),
    `SIS User ID` = c(NA, 86949302, 56483932, NA),
    `SIS Login ID` = c(NA, 72546, 85940, 0xef6798020)
  )
  
  actual <- determine_grade_source(data)
  expected <- "Canvas"
  
  expect_equal(actual, expected)
})

test_that("Test Determine Grade Source - Gradescope", {
  data <- tibble::tibble(
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
  
  actual <- determine_grade_source(data)
  
  expect_equal(actual, "Gradescope")
})

test_that("Test Determine Grade Source - Nonsense", {
  
  data <- tibble::tibble(
    Student = c("Bill", "Henry", "Juliette", "Barry"),
    `Berk ID` = c(90909, 80085, 13459, 88768),
    Homework = c(0.7, 0.1, 0.4, 0.9)
  )
  
  actual <- determine_grade_source(data)
  
  expect_equal(actual, "Unrecognized")
  
})

test_that("Test read_canvas_grades", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry"),
    ID = c(NA, 989786, 453657, 123786, 876345),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345),
    Section = c(NA, "1", "1", "2", "3"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4),
    `Midterm (867589)` = c(50, 34, 46, 12, 31),
    `Final (345678)` = c(100, 34, 45, 65, 87),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2)
    )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})


test_that("Test read_canvas_grades With Test Student", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Test Student and Cols including (...)", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Final Grade)` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Unposted Final Grade 321)` = c(NA, 6, 7, 3, 2, NA),
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Test Student and Cols including ( )", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Final Grade)` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Unposted Final Grade 321)` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category ( )` = c(NA, 6, 7, 3, 2, NA),
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Test Student and Cols including ()", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Final Grade)` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category (Unposted Final Grade 321)` = c(NA, 6, 7, 3, 2, NA),
    `Beep Boop Category ()` = c(NA, 6, 7, 3, 2, NA),
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Test Student and whitespace trimmed Points Possible", {
  data <- tibble::tibble(
    Student = c("Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With UID", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, "456789", "UID:768596", "567812", "UID:888763", NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c("456789", "UID:768596", "567812", "UID:888763"),
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
    `Midterm (867589) - Lateness (H:M:S)` =  c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, 45, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Excused assignments", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, 5, 6, "EX", 8, 0),
    `HW 2 (867573)` = c(5, 1, "EX", 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(100, 34, "EX", 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 5, 6, NA, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, NA, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 12, 31),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, NA, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_canvas_grades With Excused and missing assignments", {
  data <- tibble::tibble(
    Student = c("    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c(10, NA, 6, "EX", 8, 0),
    `HW 2 (867573)` = c(5, 1, "EX", 3, 4, 0),
    `Midterm (867589)` = c(50, 34, 46, NA, 31, 155),
    `Final (345678)` = c(100, 34, "EX", 65, 87, 0),
    `Beep Boop Category` = c(NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Adam", "John", "Stephanie", "Henry"),
    `Last Name` = c("Smith", "Rock", "Porch", "Pai"),
    SID = c(456789, 768596, 567812, 888763),
    Sections = c("1", "1", "2", "3"),
    `HW 1 (867568)` = c( 0, 6, NA, 8),
    `HW 1 (867568) - Max Points` = c( 10, 10, 10, 10),
    `HW 1 (867568) - Submission Time` = c(NA, NA, NA, NA),
    `HW 1 (867568) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `HW 2 (867573)` = c( 1, NA, 3, 4),
    `HW 2 (867573) - Max Points` = c( 5, 5, 5, 5),
    `HW 2 (867573) - Submission Time` = c(NA, NA, NA, NA),
    `HW 2 (867573) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    
    `Midterm (867589)` = c(34, 46, 0, 31),
    `Midterm (867589) - Max Points` = c(50, 50, 50, 50),
    `Midterm (867589) - Submission Time` = c(NA, NA, NA, NA),
    `Midterm (867589) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00"),
    `Final (345678)` = c( 34, NA, 65, 87),
    `Final (345678) - Max Points` = c( 100, 100, 100, 100),
    
    `Final (345678) - Submission Time` = c(NA, NA, NA, NA),
    
    `Final (345678) - Lateness (H:M:S)` = c("00:00:00", "00:00:00", "00:00:00", "00:00:00")
    
  )
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})



test_that("Test read_canvas_grades With Notes Row", {
  data <- tibble::tibble(
    Student = c(NA , "    Points Possible", "Smith, Adam", "Rock, John", "Porch, Stephanie", "Pai, Henry",
                "Student, Test"),
    ID = c(NA, NA, 989786, 453657, 123786, 876345, 670504),
    `SIS User ID` = c(NA, NA, 456789, 768596, 567812, 888763, NA),
    `SIS Login ID` = c(NA, NA, 46574, 75685, 64573, 12345, 0xeff85769),
    Section = c(NA, NA, "1", "1", "2", "3", "1"),
    `HW 1 (867568)` = c("Manually Entered" ,10, 5, 6, 7, 8, 0),
    `HW 2 (867573)` = c(NA, 5, 1, 2, 3, 4, 0),
    `Midterm (867589)` = c("Some text", 50, 34, 46, 12, 31, 155),
    `Final (345678)` = c(NA, 100, 34, 45, 65, 87, 0),
    `Beep Boop Category` = c(NA, NA, 6, 7, 3, 2, NA)
  )
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Canvas"
  
  actual <- read_canvas_grades(data)
  
  expect_equal(actual, expected)
})



test_that("Test read_gradescope_grades", {
  data <- tibble::tibble(
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
  
  expected <- tibble::tibble(
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
  
  attr(expected, "source") <- "Gradescope"
  
  actual <- read_gradescope_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_gradescope_grades with missing assignments", {
  data <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, NA),
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
    
    `Project 1` = c(0.9, 0, NA, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, 0, 0.9, 0),
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
    
    `Project 1` = c(0.9, 0, 0, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(expected, "source") <- "Gradescope"
  
  actual <- read_gradescope_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Test read_gradescope_grades with missing and excused assignments", {
  data <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, "EX", 0.9, NA),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, "EX"),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, NA, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, NA, 0.9, 0),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, NA),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  attr(expected, "source") <- "Gradescope"
  
  actual <- read_gradescope_grades(data)
  
  expect_equal(actual, expected)
})

test_that("Error with column of all NA in impute_missing", {
  data <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, "EX", 0.9, NA),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, "EX"),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 4` = c(NA, NA, NA, NA),
    `Lab 4 - Max Points` = c(1, 1, 1, 1),
    `Lab 4 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 4 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 5` = c(NA, NA, NA, 0.5),
    `Lab 5 - Max Points` = c(1, 1, 1, 1),
    `Lab 5 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 5 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 6` = c(NA, NA, "EX", "EX"),
    `Lab 6 - Max Points` = c(1, 1, 1, 1),
    `Lab 6 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 6 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, NA, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  expected <- tibble::tibble(
    `First Name` = c("Joe", "Harrison", "Don", "Kevin"),
    `Last Name` = c("Oneida", "Eagle", "Torrensen", "Falcon"),
    SID = c(98657, 12345, 76589, 44567),
    Email = c("joe@on.com", "he@eagle.net", "don@gmail.com", "kev@berkeley.edu"),
    `Lab 1` = c(1, NA, 0.9, 0),
    `Lab 1 - Max Points` = c(1, 1, 1, 1),
    `Lab 1 - Submission Time` = c("1/19/2023 9:25:00 AM", "0",
                                  "1/19/2023 10:00:00 AM", "0"),
    `Lab 1 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 2` = c(1, 0, 0.9, NA),
    `Lab 2 - Max Points` = c(1, 1, 1, 1),
    `Lab 2 - Submission Time` = c("1/20/2023 9:25:00 AM", "0",
                                  "1/20/2023 10:00:00 AM", "0"),
    `Lab 2 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Lab 3` = c(0, 0, 0.9, 0.5),
    `Lab 3 - Max Points` = c(1, 1, 1, 1),
    `Lab 3 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 3 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 4` = c(0, 0, 0, 0),
    `Lab 4 - Max Points` = c(1, 1, 1, 1),
    `Lab 4 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 4 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 5` = c(0, 0, 0, 0.5),
    `Lab 5 - Max Points` = c(1, 1, 1, 1),
    `Lab 5 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 5 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    `Lab 6` = c(0, 0, NA, NA),
    `Lab 6 - Max Points` = c(1, 1, 1, 1),
    `Lab 6 - Submission Time` = c("0", "0", "1/21/2023 10:00:00 AM",
                                  "1/21/2023 9:50:00 AM"),
    `Lab 6 - Lateness (H:M:S)` = c("0:00:00", "0:00:00", "0:00:00", "0:00:00"),
    
    `Project 1` = c(0.9, 0, 0, 0),
    `Project 1 - Max Points` = c(1, 1, 1, 1),
    `Project 1 - Submission Time` = c("1/22/2023 9:25:00 AM", "0",
                                      "1/22/2023 10:00:00 AM", "0"),
    `Project 1 - Lateness (H:M:S)` = c("0:00:00","0:00:00","0:00:00","0:00:00")
  )
  
  assignments <- get_assignments(data)
  
  # missing assignments become 0s
  
  actual <- data |> 
    dplyr::mutate_at(assignments, impute_missing_excused_input)
  
  expect_equal(actual, expected)
})