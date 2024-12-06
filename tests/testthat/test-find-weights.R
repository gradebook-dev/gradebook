test_that("Extracting Weights", {
  cats <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = 0.1,
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        )
      )
    )
  ))
  pol <- list(categories = cats)
  actual <- find_weights(pol)
  
  cats2 <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = 0.1,
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        ),
        weights = c(0.4, 0.6))
    ),
    weights = c(0.3, 0.1, 0.6)
  ))
  expected <- list(categories = cats2)
  expect_equal(actual, expected)
})

test_that("Parallel Grading Trees", {
  cats <- list(list(
    category = "Overall Grade Undergrads",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Homework",
        aggregation = "equally_weighted",
        weight = 0.25,
        assignments = c("Homework 1", "Homework 2", "HW3", "HW4")
      ),
      list(
        category = "Midterms",
        aggregation = "weighted_by_points",
        weight = 0.3,
        assignments = c("Midterm 1", "Midterm 2", "Midterm 3")
      ),
      list(
        category = "Final",
        weight = 0.45,
        assignments = "Final Exam"
      )
    )
  ),
  list(
    category = "Overall Grade Grad",
    aggregation = "weighted_mean",
    weight = 0.1,
    assignments = list(
      list(
        category = "Homework",
        aggregation = "equally_weighted",
        weight = 0.25,
        assignments = c("Homework 1", "Homework 2", "HW3", "HW4")
      ),
      list(
        category = "Midterms",
        aggregation = "weighted_by_points",
        weight = 0.3,
        assignments = c("Midterm 1", "Midterm 2", "Midterm 3")
      ),
      list(
        category = "Final",
        weight = 0.45,
        assignments = "Final Exam"
      ),
      list(
        category = "Final project",
        weight = 0.8,
        aggregation = "weighted_mean",
        assignments = list(
          list(
            category = "Coding",
            weight = -0.2,
            assigments = "Final project coding"
          ),
          list(
            category = "Report",
            weight = -0.4,
            assignments = "Final Report"
          )
        )
      )
    )
  ),
  list(
    category = "Beep boop",
    assignments = "Fizz Buzz"
  ),
  list(
    category = "Alt Grades",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Exams",
        aggregation = "weighted_by_points",
        assignments = c("Final Exam", "Midterm 2"),
        weight = 0.95
      ),
      list(
        category = "Alt HWs",
        aggregation = "equally_weighted",
        weight = 0.05,
        assignments = c("Homework 2, Homework 7")
      )
    )
  )
  )
  pol <- list(categories = cats)
  
  exp_cats <- list(list(
    category = "Overall Grade Undergrads",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Homework",
        aggregation = "equally_weighted",
        weight = 0.25,
        assignments = c("Homework 1", "Homework 2", "HW3", "HW4")
      ),
      list(
        category = "Midterms",
        aggregation = "weighted_by_points",
        weight = 0.3,
        assignments = c("Midterm 1", "Midterm 2", "Midterm 3")
      ),
      list(
        category = "Final",
        weight = 0.45,
        assignments = "Final Exam"
      )
    ),
    weights = c(0.25, 0.3, 0.45)
  ),
  list(
    category = "Overall Grade Grad",
    aggregation = "weighted_mean",
    weight = 0.1,
    assignments = list(
      list(
        category = "Homework",
        aggregation = "equally_weighted",
        weight = 0.25,
        assignments = c("Homework 1", "Homework 2", "HW3", "HW4")
      ),
      list(
        category = "Midterms",
        aggregation = "weighted_by_points",
        weight = 0.3,
        assignments = c("Midterm 1", "Midterm 2", "Midterm 3")
      ),
      list(
        category = "Final",
        weight = 0.45,
        assignments = "Final Exam"
      ),
      list(
        category = "Final project",
        weight = 0.8,
        aggregation = "weighted_mean",
        assignments = list(
          list(
            category = "Coding",
            weight = -0.2,
            assigments = "Final project coding"
          ),
          list(
            category = "Report",
            weight = -0.4,
            assignments = "Final Report"
          )
        ),
        weights = c(1/3, 2/3)
      )
    ),
    weights = c(0.25/1.8, 0.3/1.8, 0.45/1.8, 0.8/1.8)
  ),
  list(
    category = "Beep boop",
    assignments = "Fizz Buzz"
  ),
  list(
    category = "Alt Grades",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Exams",
        aggregation = "weighted_by_points",
        assignments = c("Final Exam", "Midterm 2"),
        weight = 0.95
      ),
      list(
        category = "Alt HWs",
        aggregation = "equally_weighted",
        weight = 0.05, 
        assignments = c("Homework 2, Homework 7")
      )
    ),
    weights = c(0.95, 0.05)
  )
  )
  
  exp <- list(categories = exp_cats)
  actual <- find_weights(pol)
  expect_equal(exp, actual)
})

test_that("If some weighted_mean category has no weights", {
  cats <- list(
    list(
      category = "Overall Grade",
      aggregation = "weighted_mean",
      assignments = list(
        list(
          category = "Homework",
          aggregation = "equally_weighted",
          assignments = c("Homework 1", "Homework 2")
        ),
        list(
          category = "Exams",
          aggregation = "weighted_by_points",
          assignments = c("Final", "Midterm")
        )
      ), 
      weights = c(0.5, 0.5)
    )
  )
  pol <- list(categories = cats)
  act <- suppressWarnings(find_weights(pol))
  suppressWarnings(expect_warning(find_weights(pol)))
  expect_equal(pol, act)
})

test_that("Some cats missing weights", {
  cats <- list(
    list(
      category = "Overall Grade",
      aggregation = "weighted_mean",
      assignments = list(
        list(
          category = "Homework",
          aggregation = "equally_weighted",
          weight = 0.15,
          assignments = c("Homework 1", "Homework 2")
        ),
        list(
          category = "Exams",
          weight = 0.5,
          aggregation = "weighted_by_points",
          assignments = c("Final", "Midterm")
        ),
        list(
          category = "Discussion",
          aggregation = "weighted_by_points",
          assignments = c("Discussion 1", "Discussion 2")
          
        ),
        list(
          category = "Likeable",
          aggregation = "equally_weighted",
          assignments = "Likeable points"
        )
      )
    )
  )
  
  pol <- list(categories = cats)
  cats2 <-  list(
    list(
      category = "Overall Grade",
      aggregation = "weighted_mean",
      assignments = list(
        list(
          category = "Homework",
          aggregation = "equally_weighted",
          weight = 0.15,
          assignments = c("Homework 1", "Homework 2")
        ),
        list(
          category = "Exams",
          weight = 0.5,
          aggregation = "weighted_by_points",
          assignments = c("Final", "Midterm")
        ),
        list(
          category = "Discussion",
          aggregation = "weighted_by_points",
          assignments = c("Discussion 1", "Discussion 2")
          
        ),
        list(
          category = "Likeable",
          aggregation = "equally_weighted",
          assignments = "Likeable points"
        )
      ),
      weights = c(0.15/.65, 0.5/.65, 0, 0)
    )
  )
  exp <- list(categories = cats2)
  suppressWarnings(expect_warning(find_weights(pol)))
  
  actual <- suppressWarnings(
    find_weights(pol)
  )
  
  expect_equal(exp, actual)
})

test_that("Extracting Weights with length > 1", {
  cats <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = c(0.1, 0.7),
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        )
      )
    )
  ))
  pol <- list(categories = cats)
  expect_warning(find_weights(pol))
  
  expect_warning(find_weights(pol))
  
  actual <- suppressWarnings(find_weights(pol))
  
  cats2 <- list(list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    
    assignments = list(
      list(
        category = "Labs",
        aggregation = "equally_weighted",
        weight = 0.3,
        assignments = c("Lab 1", "Lab 2", "Lab 3", "Lab 4", 
                        "Lab 5", "Lab 6")
      ), 
      list(
        category = "Discussions",
        aggregation = "weighted_by_points",
        weight = c(0.1, 0.7),
        assignments = c("Discussion 1", "Discussion 2")
      ),
      list(
        category = "Exams",
        aggregation = "weighted_mean",
        weight = 0.6,
        
        assignments = list(
          list(
            category = "Midterm Exam",
            weight = 0.2,
            assignment = "Midterm"
          ),
          list(
            category = "Final Exam",
            weight = 0.3,
            assignment = "Final"
          )
        ),
        weights = c(0.4, 0.6))
    ),
    weights = c(0.3, 0.1, 0.6)
  ))
  expected <- list(categories = cats2)
  expect_equal(actual, expected)
 
})