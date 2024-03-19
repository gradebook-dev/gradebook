#' Calculate Grades
#' This function calculates all grades based on the policy file.
#'
#' @param gs A processed data in wide format 
#' @param policy A policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr select
#' 
#' @export

get_grades <- function(gs, policy){
  
  # flatten policy file
  policy <- flatten_policy(policy)
  
  # convert gs into a matrix with only assignment info
  grades_mat <- gs |>
    #only assignment info
    select(-get_id_cols(gs)) |> 
    #convert lateness into minutes
    mutate_at(vars(ends_with(" - Lateness (H:M:S)")), convert_to_min) |> 
    #convert to matrix
    data.matrix()
  #rownames are SID of student
  rownames(grades_mat) <- gs$SID
  #pre_allotted cols
  categories <- unlist(map(policy$categories, "category"))
  empty <- matrix(nrow = length(gs$SID), ncol = length(categories)*3)
  colnames(empty) <- paste0(rep(categories, each = 3), c("", " - Max Points", " - Lateness (H:M:S)"))
  grades_mat <- cbind(grades_mat, empty)
  
  
  #iterate through each policy item
  for (policy_item in policy$categories){
    #compute calculations for each policy file and save into grades matrix
    grades_mat <- get_category_grade(grades_mat, policy_item)
  }
  
  grades <- grades_mat |>
    data.frame()
  names(grades) <- colnames(grades_mat)
  grades$SID <- rownames(grades_mat)
  #add back ID cols
  
  return (grades)
}

#' Calculate Single Category Grade
#' This function calculates all grades based on the policy file.
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param policy_item An item from policy file
#'
#' @return A data frame
#'
#' 
#' @export

get_category_grade <- function(grades_mat, policy_item){
  #get all keys except category and assignments (which are not functions)
  keys <- names(policy_item)[-which(names(policy_item) %in% c("category", "assignments", "weights"))]
  #all assignments + their associated cols for this category
  for (key in keys){
    grades_mat <- get(key)(grades_mat, policy_item[[key]], policy_item$category, 
                           policy_item$assignments, policy_item$weights)
  }
  
  ## insert code to re-add new/editted columns from grades_mat into grades_mat
  return (grades_mat)
}


#' Key Functions
#'
#' @description
#' 
#' A collection of functions to refer to correct classification of grading functions using policy file keys.
#'
#' * `score()` computes the percentage score and saves back into raw points col
#' 
#' * `aggregation()` computes score for category
#'
#' * `lateness()` applies any relevant lateness penalty.
#' 
#' * `drop_n_lowest()` drops n lowest assignment scores; if n >= num of assignments, return top assignment
#' 
#' * `aggregation_max_pts()` computes max points for category.
#' 
#' * `aggregation_lateness()` computes max points for category.
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param policy_line Policy list item for that key
#' @param category Category name
#' @param assignments Assignment names for this category
#' @param weights Weights for `weighted_mean`
#' 
#' @return A matrix
#'
#' @family {Key Functions}
#' 
#' @export
score <- function(grades_mat, policy_line, category, assignments, weights = c()){
  get(policy_line)(grades_mat, assignments)
}

#' @rdname score
#' @export
aggregation <- function(grades_mat, policy_line, category, assignments, weights = c()){
  get(policy_line)(grades_mat,category, assignments, weights)
}

#' @rdname score
#' @export
lateness <- function(grades_mat, policy_line, category, assignments, weights = c()){
  original_late_mat <- grades_mat[, paste0(assignments, " - Lateness (H:M:S)")]
  for (late_policy in policy_line){
   grades_mat <- get(names(late_policy))(grades_mat, late_policy, original_late_mat, assignments)
  }
  return (grades_mat)
}

#' @rdname score
#' @export
drop_n_lowest <- function(grades_mat, policy_line, category, assignments, weights = c()){
  n <- policy_line
  grades_mat[, assignments] <- t(apply(t(grades_mat[, assignments]), 2, function(assign){
    if (length(assignments) == 1) return (assign)
    lowest_indices <- order(assign)[1:n]
    if (n >= length(assignments)) {
      lowest_indices <- order(assign)[1:(length(assignments)-1)]
    }
    assign[lowest_indices] <- NA
    return(assign)
  }))
  return (grades_mat)
}

#' @rdname score
#' @export
aggregation_max_pts <- function(grades_mat, policy_line, category, assignments, weights = c()){
  get(policy_line)(grades_mat, category, assignments)
}

#' @rdname score
#' @export
aggregation_lateness <- function(grades_mat, policy_line, category, assignments, weights = c()){
  get(policy_line)(grades_mat, category, assignments)
}


#' Score Functions
#'
#' @description
#' 
#' A collection of functions to calculate the percentage score and save back into raw points column.
#'
#' * `raw_over_max()` computes score by dividing raw points by max points.
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param assignments Assignment names for this category
#' 
#' @return A matrix
#'
#' @family {Score Functions}
#' 
#' @export
raw_over_max <- function(grades_mat, assignments){
  grades_mat[,assignments] <- grades_mat[, assignments] / grades_mat[, paste0(assignments, " - Max Points")]
  return(grades_mat)
}

#' Aggregation Functions
#'
#' @description
#' 
#' A collection of functions to compute scores for categories.
#'
#' * `equally_weighted()` computes a mean.
#' 
#' * `weighted_by_points()` computes a weighted mean using a weights vector.
#'
#' * `max_score()` computes the max score.
#' 
#' * `min_score()` computes the min score after optionally dropping lowest scores.
#' 
#' * `none()` if there is only 1 score, returns it, otherwise defaults to `equally_weighted` 
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param category Category name
#' @param assignments Assignment names for this category
#' @param weights Weights for `weighted_mean`, default to NULL with other aggregation method
#' 
#' @return A matrix
#'
#' @family {Aggregation Functions}
#' 
#' @export
equally_weighted <- function(grades_mat, category, assignments, weights = c()){
  grades_mat[,category] <- rowMeans(grades_mat[, assignments], na.rm = TRUE)
  return(grades_mat)
}

#' @rdname equally_weighted
#' @export
weighted_by_points <- function(grades_mat, category, assignments, weights = c()){
  max_cols <- paste0(assignments, " - Max Points")
  grades_mat[,category] <- rowSums(grades_mat[, assignments] * grades_mat[, max_cols], na.rm = TRUE) / rowSums(grades_mat[, max_cols])
  return(grades_mat)
}

#' @rdname equally_weighted
#' @export
max_score <- function(grades_mat, category, assignments, weights = c()){
  if (length(assignments) == 1){
    grades_mat <- none(grades_mat, category, assignments)
    return (grades_mat)
  }
  grades_mat[,category] <- apply(grades_mat[, assignments],1,function(x) max(x,na.rm=T))
  return(grades_mat)
}

#' @rdname equally_weighted
#' @export
min_score <- function(grades_mat, category, assignments, weights = c()){
  if (length(assignments) == 1){
    grades_mat <- none(grades_mat, category, assignments)
    return (grades_mat)
  }
  grades_mat[,category] <- apply(grades_mat[, assignments],1,function(x) min(x,na.rm=T))
  return(grades_mat)
}

#' @rdname equally_weighted
#' @export
weighted_mean <- function(grades_mat, category, assignments, weights){
  if (length(assignments) != length(weights)){
    grades_mat <- equally_weighted(grades_mat, category, assignments)
    return (grades_mat)
  }
  grades_mat[, category] <- rowSums(t(t(grades_mat[, assignments]) * weights), na.rm = TRUE)/ sum(weights)
  return (grades_mat)
}

#' @rdname equally_weighted
#' @export
none <- function(grades_mat, category, assignments, weights = c()){
  if (length(assignments) == 1){
    grades_mat[,category] <- grades_mat[, assignments]
    return (grades_mat)
  }
  equally_weighted(grades_mat, category, assignments)
}

#' Aggregation for Max Points Functions
#'
#' @description
#' 
#' A collection of functions to computes max points for category.
#'
#' * `sum_max_pts()` computes a sum.
#' 
#' * `mean_max_pts()` computes a mean.
#'
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param category Category name
#' @param assignments Assignment names for this category
#' 
#' @return A matrix
#'
#' @family {Aggregation for Max Points Functions}
#' 
#' @export
sum_max_pts <- function(grades_mat, category, assignments){
  if (length(assignments) == 1){
    grades_mat[,paste0(category, " - Max Points")] <- grades_mat[, paste0(assignments, " - Max Points")]
  } else {
    grades_mat[,paste0(category, " - Max Points")] <- rowSums(grades_mat[, paste0(assignments, " - Max Points")], na.rm = TRUE)
  }
  return (grades_mat)
}
#' @rdname sum_max_pts
#' @export
mean_max_pts <- function(grades_mat, category, assignments){
  if (length(assignments) == 1){
    grades_mat[,paste0(category, " - Max Points")] <- grades_mat[, paste0(assignments, " - Max Points")]
  } else {
    grades_mat[,paste0(category, " - Max Points")] <- rowMeans(grades_mat[, paste0(assignments, " - Max Points")], na.rm = TRUE)
  }
  return (grades_mat)
}

#' Lateness Functions
#'
#' @description
#' 
#' A collection of functions to apply lateness policies.
#'
#' * `until()` determines if lateness for `assignments` is less than or equal to `late_policy`.
#' 
#' * `add()` adds `late_policy` to score if `assignments` is determined as late.
#' 
#' * `between()` determines if lateness for `assignments` is between `late_policy`.
#' 
#' * `scale_by()` scales score by `late_policy` if `assignments` is determined as late.
#' 
#' * `after()` determines if lateness for `assignments` is more than or equal to `late_policy`.
#' 
#' * `set_to()` sets score to `late_policy` if `assignments` is determined as late.
#'
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param late_policy Relevant threshold/scalar for lateness policy
#' @param original_late_mat Matrix that saves original lateness values
#' @param assignments Assignment names for this category
#' 
#' @return A matrix
#'
#' @family {Lateness Functions}
#' 
#' @export
until <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  grades_mat[, late_cols] <- grades_mat[, late_cols] <= convert_to_min(late_policy)
  return (grades_mat)
}

#' @rdname until
#' @export
add <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  grades_mat[, assignments] <- grades_mat[, assignments] + (grades_mat[, late_cols]*unlist(late_policy))
  grades_mat[, late_cols] <- original_late_mat
  return (grades_mat)
}

#' @rdname until
#' @export
between <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  from <- min(convert_to_min(unlist(late_policy)))
  to <- max(convert_to_min(unlist(late_policy)))
  grades_mat[, late_cols] <- grades_mat[, late_cols] >= from & grades_mat[, late_cols] <= to
  return (grades_mat)
}

#' @rdname until
#' @export
scale_by <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  grades_mat[, assignments] <- ifelse(grades_mat[, late_cols] == 1, 
                                      grades_mat[, assignments] * unlist(late_policy), 
                                      grades_mat[, assignments])
  grades_mat[, late_cols] <- original_late_mat
  return (grades_mat)
}

#' @rdname until
#' @export
after <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  grades_mat[, late_cols] <- grades_mat[, late_cols] >= convert_to_min(late_policy)
  return (grades_mat)
}

#' @rdname until
#' @export
set_to <- function(grades_mat, late_policy, original_late_mat, assignments){
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  grades_mat[, assignments] <- ifelse(grades_mat[, late_cols] == 1, 
                                      unlist(late_policy), 
                                      grades_mat[, assignments])
  grades_mat[, late_cols] <- original_late_mat
  return (grades_mat)
}

#' Aggregation for Lateness Functions
#'
#' @description
#' 
#' A collection of functions to computes lateness for category.
#'
#' * `mean_lateness()` computes a sum.
#' 
#' * `sum_lateness()` computes a mean.
#' 
#' * `max_lateness()` computes a max.
#'
#'
#' @param grades_mat Matrix with assignments + associated cols for that category
#' @param category Category name
#' @param assignments Assignment names for this category
#' 
#' @return A matrix
#'
#' @family {Aggregation for Lateness Functions}
#' 
#' @export
mean_lateness <- function(grades_mat, category, assignments){
    if (length(assignments) == 1){
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- grades_mat[, paste0(assignments, " - Lateness (H:M:S)")]
  } else {
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- rowMeans(grades_mat[, paste0(assignments, " - Lateness (H:M:S)")], na.rm = TRUE)
  }
  return (grades_mat)
}

#' @rdname mean_lateness
#' @export
sum_lateness <- function(grades_mat, category, assignments){
  if (length(assignments) == 1){
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- grades_mat[, paste0(assignments, " - Lateness (H:M:S)")]
  } else {
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- rowSums(grades_mat[, paste0(assignments, " - Lateness (H:M:S)")], na.rm = TRUE)
  }
  return (grades_mat)
}

#' @rdname mean_lateness
#' @export
max_lateness <- function(grades_mat, category, assignments){
  if (length(assignments) == 1){
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- grades_mat[, paste0(assignments, " - Lateness (H:M:S)")]
  } else {
    grades_mat[,paste0(category, " - Lateness (H:M:S)")] <- apply(grades_mat[, paste0(assignments, " - Lateness (H:M:S)")],
                                                                  1,function(x) max(x,na.rm=T))
  }
  return (grades_mat)
}


#' @importFrom lubridate hms period_to_seconds 
convert_to_min <- function(hms){
  save <- lubridate::hms(hms) |>
    lubridate::period_to_seconds()
  save <- save/60
  return (save)
}
