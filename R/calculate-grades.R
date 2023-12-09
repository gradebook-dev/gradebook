#' Aggregate assignment scores
#'
#' @description
#' 
#' A collection of functions to aggregate assignment scores. Their s
#'
#' * `equally_weighted()` computes a mean with the option to drop lowest scores.
#' 
#' * `weighted_by_points()` computes a weighted mean using a weights vector after
#' optionally dropping the lowest scores.
#'
#' * `max_score()` computes the max score.
#' 
#' * `min_score()` computes the min score after optionally dropping lowest scores.
#' 
#' * `none()` if there is only 1 score, returns it, otherwise throws an error. 
#' Serves as a passthrough aggregation for categories with only 1 assignment.
#'
#' @param scores A vector of assignment scores.
#' @param weights A vector of weights of the same length as `scores`. Weights
#' that do not sum to one are ok - they get normalized.
#' @param n_drops The number of lowest scores to drop before aggregating.
#' 
#' @return A single aggregated score (a vector of length 1). 
#'
#' @family {Aggregation functions}
#' @examples
#' my_scores <- c(.7, .9, .1)
#' my_weights <- c(15, 10, 10)
#' 
#' equally_weighted(scores = my_scores, weights = my_weights, n_drops = 1)
#' weighted_by_points(scores = my_scores, weights = my_weights, n_drops = 1)
#' max_score(my_scores, weights = my_weights)
#' min_score(my_scores,weights = my_weights, n_drops = 1)
#' my_score <- c(.7)
#' my_weight <- c(10)
#' none(my_score, weights = my_weight)
#' 
#' @export
equally_weighted <- function(scores, weights, n_drops = 0, ...) {
  if (n_drops > 0) {scores[order(scores)[1:n_drops]] <- NA}
  
  c(mean(scores, na.rm =TRUE), sum(weights, na.rm = TRUE))
}

#' @rdname equally_weighted
#' @export
weighted_by_points <- function(scores, weights, n_drops = 0, ...) {
  
  if (n_drops > 0) {
    drop_idx <- order(scores)[1:n_drops]
    weights[drop_idx] <- NA
    scores[drop_idx] <- NA
  }
  
  c(sum(scores * (weights / sum(weights, na.rm = TRUE)), na.rm =TRUE),
    sum(weights, na.rm = TRUE))
}

#' @rdname equally_weighted
#' @export
max_score <- function(scores, weights, n_drops = 0, ...) {
  c(max(scores, na.rm = TRUE), mean(weights, na.rm = TRUE))
}

#' @rdname equally_weighted
#' @export
min_score <- function(scores, weights, n_drops = 0, ...) {
  c(min(scores, na.rm = TRUE), mean(weights, na.rm = TRUE))
}

#' @rdname equally_weighted
#' @export
none <- function(scores, weights, n_drops = 0, ...) {
  if (length(scores) == 1) {
      c(scores, weights)
  } else {
      stop("Can only use `aggregation: none`
           if there is only 1 assignment in 
           the category.")
  }
}

#' Get one category grade
#'
#' @description
#' 
#' Applies one of the aggregation functions to grade data from one student to 
#' calculate one category grade for that student.
#'
#' @param gs_row A vector of assignment scores and weights coming from a row of the
#' gs data frame.
#' @param policy_item A single-layer list containing, at least `assignments`,
#' a vector and optionally `n_drops`, an integer.
#' 
#' @return A single aggregated category grade.
#' @export
get_one_grade <- function(gs_row, policy_item) {
  get(policy_item$aggregation)(
    scores = gs_row[policy_item$assignments],
    weights = gs_row[paste0(policy_item$assignments, " - Max Points")],
    n_drops = ifelse(is.null(policy_item$n_drops), 0, policy_item$n_drops))
}

#' Get category grades for all students
#'
#' @description
#' 
#' Iterates `get_one_grade()` across all categories of a policy file and across
#' all students present in a gs file.
#'
#' @param gs A vector of assignment scores and weights coming from a row of the
#' gs data frame.
#' @param policy A single-layer list containing, at least `assignments`,
#' a vector and optionally `n_drops`, an integer.
#' 
#' @return An extended version of the gs data frame, with columns added for each 
#' category described in the policy file containing the grades for each student.
#' @importFrom dplyr bind_cols
#' @importFrom purrr map modify_at compact
#' @export

get_category_grades <- function(gs, policy) {
  
  # pull off assignment scores and weights into matrix
  assignment_names <- get_assignments(gs)
  assignment_cols <- c(assignment_names, paste0(assignment_names, " - Max Points"))
  assignments_mat <- data.matrix(gs[assignment_cols])
  if (!is.numeric(assignments_mat)) {stop("Cannot calculate category grades. Assignment columns contain non-numeric values.")}
  
  # prune unnecessary data from policy file
  assgns_and_cats <- c(assignment_names, purrr::map(policy$categories, "category"))
  policy$categories <- policy$categories |>
    # remove ungraded assignments
    purrr::map(\(item) purrr::modify_at(item, "assignments", ~ .x[.x %in% assgns_and_cats])) |>
    # remove categories with no assignments from policy file
    purrr::discard(\(item) length(item$assignments) == 0)
  
  category_grades_mat <- matrix(nrow = nrow(assignments_mat), ncol = 0)

  # for every category in the policy file...
  for (policy_item in policy$categories) {

    # and for every row in the matrix, get a grade and a total weight (max points)
    grades_weights_mat <- t(apply(cbind(assignments_mat, category_grades_mat), 1,
                              get_one_grade, 
                              policy_item = policy_item))
    colnames(grades_weights_mat) <- c(policy_item$category,
                                  paste0(policy_item$category, " - Max Points"))
    category_grades_mat <- cbind(category_grades_mat, grades_weights_mat)
  }
  
  gs_w_cats <- bind_cols(gs, category_grades_mat) |>
    apply_clobber(policy)
  
  # extract main categories and weights
  main_cat_weights <- policy$categories |>
      purrr::map(\(x) get_main_cat_weights(x)) |>
      purrr::compact() |>
      unlist()
  
  # normalize weights
  main_cat_weights <- main_cat_weights/sum(main_cat_weights)
  
  # calculate overall score
  overall_score <- gs_w_cats[names(main_cat_weights)] |>
      data.matrix() |>
      t() |>
      crossprod(main_cat_weights) |>
      as.vector()
  
  gs_w_cats <- cbind(gs_w_cats, `Overall Score` = overall_score)
  
  return(gs_w_cats)
}

get_main_cat_weights <- function(x) {
    out <- list()
    if (exists("weight", x)) {
        out[[x[["category"]]]] <- x[["weight"]]
        return(out)
    } else {
        return(NULL)
        }
}

#' Get letter grades for all students
#'
#'
#' @param gs A vector of assignment scores and weights coming from a row of the
#' gs data frame with all category and overall grades
#' @param policy A policy file with course description, grading policy and 
#' bounds for letter grades
#' 
#' @return An extended version of the gs data frame, with a column added for  
#' letter grades, allotted as described in the policy file 
#' 
#' @importFrom dplyr mutate
#' 
#' @export
get_letter_grades <- function(gs, policy){
  
  gs <- gs |> mutate(`Letter Grade` = NA)
  
  for (letter in policy$letter_grades){
    within_bounds <- gs$`Overall Score` >= letter$lower_bound/100 & 
                      gs$`Overall Score`  < letter$upper_bound/100
    
    if (is.null(letter$lower_bound)){
      within_bounds <- gs$`Overall Score`  < letter$upper_bound/100
      
    } else if (is.null(letter$upper_bound)){
      within_bounds <- gs$`Overall Score` >= letter$lower_bound/100
    }
    
    gs$`Letter Grade`[within_bounds] <- letter$letter
  }
  
  return (gs)
}

#' @importFrom lubridate hms period_to_seconds 
convert_to_min <- function(hms){
  save <- lubridate::hms(hms) |>
    lubridate::period_to_seconds()
  save <- save/60
  return (save)
  
  # placeholder for apply_clobbers()
}

#' Apply Clobber Policy
#'
#' @description
#' This computes any clobber policy, where the category grade is determined by the
#' maximum score between two categories.
#'
#' @param gs A vector of assignment scores and weights coming from a row of the
#' gs data frame.
#' @param policy A single-layer list containing, at least `assignments`,
#' a vector and optionally `n_drops`, an integer.
#' 
#' @return A gs dataframe with clobber policies applied to relevant categories
#' @export
apply_clobber <- function(gs, policy){
  clobber_policy <- purrr::map(policy$categories, function(x){
    return (c(x$category, x$clobber))
  }) |>
    purrr::discard(function(p){
      (length(p) != 2)
    }) 
  
  if (length(clobber_policy) == 0){
    return (gs)
  }
  #apply clobber
  for (clobber in clobber_policy){
    gs[[clobber[1]]] <- pmax(gs[[clobber[1]]], gs[[clobber[2]]])
  }
  
  return (gs)
}