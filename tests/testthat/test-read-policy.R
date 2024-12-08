test_that("check_keys - correct format",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quiz",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_equal(check_keys(policy, gs), policy)
})

test_that("check_keys - missing category argument",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_error(check_keys(policy, gs))
})

test_that("check_keys - incorrect key",{
  categories <- list(
    list(
      category = "Labs",
      agg = "min_score", #wrong key here
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_error(check_keys(policy, gs))
})

test_that("check_keys - weights key",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "equally_weighted", 
      weights = 0.3, #should throw a warning
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quizzes",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_warning(check_keys(policy, gs))
})

test_that("get_categories - one category",{
  categories <- list(
    list(
      category = "Quiz",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_equal(get_categories(policy), c("Quiz"))
})

test_that("get_categories - several categories",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = c("Lab 1", "Lab 2")
    ),
    list(
      category = "Quiz",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_equal(get_categories(policy), c("Labs", "Quiz"))
})

test_that("get_categories - several nested categories",{
  categories <- list(
    list(
      category = "Labs",
      aggregation = "min_score",
      assignments = list(
        list(
          category = "Lab 1",
          aggregation = "max_score",
          assignments = c("Lab 1.1", "Lab 1.2")
        ),
        list(
          category = "Lab 2",
          aggregation = "max_score",
          assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
        )
      )
    ),
    list(
      category = "Quiz",
      aggregation = "weighted_by_points",
      assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
    )
  )
  
  policy <- list(categories = categories)
  
  expect_equal(get_categories(policy), c("Lab 1", "Lab 2", "Labs", "Quiz"))
})

test_that("get_nested_assignments - one level of nesting",{
  category <- list(
    category = "Lab 1",
    aggregation = "max_score",
    assignments = c("Lab 1.1", "Lab 1.2")
  )
  
  expect_equal(get_nested_assignments(category), c("Lab 1.1", "Lab 1.2"))
})

test_that("get_nested_assignments - two levels of nesting",{
  category <- list(
    category = "Labs",
    aggregation = "min_score",
    assignments = list(
      list(
        category = "Lab 1",
        aggregation = "max_score",
        assignments = c("Lab 1.1", "Lab 1.2")
      ),
      list(
        category = "Lab 2",
        aggregation = "max_score",
        assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
      )
    )
  )
  expected_assignments <- c("Lab 1.1", "Lab 1.2", "Lab 2.1", "Lab 2.2", "Lab 2.3")
  expect_equal(get_nested_assignments(category), expected_assignments)
})

test_that("get_nested_assignments - three levels of nesting",{
  category <- list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "min_score",
        assignments = list(
          list(
            category = "Lab 1",
            aggregation = "max_score",
            assignments = c("Lab 1.1", "Lab 1.2")
          ),
          list(
            category = "Lab 2",
            aggregation = "max_score",
            assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
          )
        )
      ),
      list(
        category = "Quiz",
        aggregation = "weighted_by_points",
        assignments = c("Quiz 1", "Quiz 2", "Quiz 3")
      )
    )
  )
  expected_assignments <- c("Lab 1.1", "Lab 1.2", "Lab 2.1", "Lab 2.2", "Lab 2.3",
                            "Quiz 1", "Quiz 2", "Quiz 3")
  expect_equal(get_nested_assignments(category), expected_assignments)
})

test_that("get_nested_assignments - three levels of nesting across two categories",{
  category <- list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "min_score",
        assignments = list(
          list(
            category = "Lab 1",
            aggregation = "max_score",
            assignments = c("Lab 1.1", "Lab 1.2")
          ),
          list(
            category = "Lab 2",
            aggregation = "max_score",
            assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
          )
        )
      ),
      list(
        category = "Quiz",
        aggregation = "weighted_by_points",
        assignments = list(
          list(
            category = "Quiz 1",
            aggregation = "max_score",
            assignments = c("Quiz 1 Individual", "Quiz 1 Group")
          ),
          list(
            category = "Quiz 2",
            aggregation = "max_score",
            assignments = c("Quiz 2 Individual", "Quiz 2 Group")
          )
        )
      )
    )
  )
  expected_assignments <- c("Lab 1.1", "Lab 1.2", "Lab 2.1", "Lab 2.2", "Lab 2.3",
                            "Quiz 1 Individual", "Quiz 1 Group",
                            "Quiz 2 Individual", "Quiz 2 Group")
  expect_equal(get_nested_assignments(category), expected_assignments)
})

test_that("get_nested_assignments - four levels of nesting across two categories",{
  category <- list(
    category = "Overall Grade",
    aggregation = "weighted_mean",
    assignments = list(
      list(
        category = "Labs",
        aggregation = "min_score",
        assignments = list(
          list(
            category = "Lab 1",
            aggregation = "max_score",
            assignments = c("Lab 1.1", "Lab 1.2")
          ),
          list(
            category = "Lab 2",
            aggregation = "max_score",
            assignments = c("Lab 2.1", "Lab 2.2", "Lab 2.3")
          )
        )
      ),
      list(
        category = "Quiz",
        aggregation = "weighted_by_points",
        assignments = list(
          list(
            category = "Quiz 1",
            aggregation = "equally_weighted",
            assignments = list(
              list(
                category = "Quiz 1 Individual",
                aggregation = "max_score",
                assignments = c("Quiz 1 Individual A", "Quiz 1 Individual B",
                                "Quiz 1 Individual C", "Quiz 1 Individual D")
              ),
              list(
                category = "Quiz 1 Group",
                aggregation = "max_score",
                assignments = c("Quiz 1 Group A", "Quiz 1 Group B",
                                "Quiz 1 Group C", "Quiz 1 Group D")
              )
            )
          ),
          list(
            category = "Quiz 2",
            aggregation = "equally_weighted",
            assignments = list(
              list(
                category = "Quiz 2 Individual",
                aggregation = "max_score",
                assignments = c("Quiz 2 Individual A", "Quiz 2 Individual B",
                                "Quiz 2 Individual C", "Quiz 2 Individual D")
              ),
              list(
                category = "Quiz 2 Group",
                aggregation = "max_score",
                assignments = c("Quiz 2 Group A", "Quiz 2 Group B",
                                "Quiz 2 Group C", "Quiz 2 Group D")
              )
            )
          )
        )
      )
    )
  )
  expected_assignments <- c("Lab 1.1", "Lab 1.2", "Lab 2.1", "Lab 2.2", "Lab 2.3",
                            "Quiz 1 Individual A", "Quiz 1 Individual B","Quiz 1 Individual C", "Quiz 1 Individual D",
                            "Quiz 1 Group A", "Quiz 1 Group B", "Quiz 1 Group C", "Quiz 1 Group D",
                            "Quiz 2 Individual A", "Quiz 2 Individual B", "Quiz 2 Individual C", "Quiz 2 Individual D",
                            "Quiz 2 Group A", "Quiz 2 Group B", "Quiz 2 Group C", "Quiz 2 Group D"
                            )
  expect_equal(get_nested_assignments(category), expected_assignments)
})
