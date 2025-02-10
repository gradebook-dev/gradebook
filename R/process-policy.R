#' Process Policy File
#' 
#' This function processes the policy file in order to compute grades and flattens
#' the nested structure of the file.
#' 
#' @param policy R list of a valid policy file
#' @param verbose Whether or not to print messages and warnings
#' 
#' @examples
#' process_policy(policy_demo, verbose = TRUE)
#' 
#' 
#' @return a (un-nested) flattened policy R list
#' @importFrom purrr map discard
#' @export
process_policy <- function(policy, verbose = FALSE){
  policy <- policy |>
    find_weights() |>
    flatten_policy()
  
  return (policy)
}

#' @importFrom purrr map
find_weights <- function(policy){
  # This function will return a policy file where the weights have been extracted from 
  # assignments into the above category featuring aggregation "weighted_mean", 
  # where the returned policy file is otherwise identical to the input. 
  policy$categories <- policy$categories |>
    purrr::map(extract_weights)
  return(policy)
}


#'@importFrom purrr map map_dbl
extract_weights <- function(category){
  # If there's no more nesting, return the category as a list
  if (!("assignments" %in% names(category) && is.list(category$assignments)
  )) {
    # remove the weight from the individual category it is in for cleanliness
    # category$weight <- NULL (under debate; may use in the future)
    return(category)
  }
  
  # potential problems encoded as warnings in order to allow execution to continue for app
  if ( category$aggregation == "weighted_mean"){
    
    category$weights <- purrr::map_dbl(category$assignments, 
                                       function(x){
                                         weight <- x$weight
                                         if (is.null(weight) ){
                                           weight = 0
                                           warning(paste0(
                                             "Category ", x$category,
                                             " is missing a weight"
                                           ))
                                         }
                                         if(length(weight) > 1){
                                           weight <- weight[[1]]
                                           warning(paste0(
                                             
                                               "Category ", x$category,
                                               " has a weight attribute of length > 1.",
                                               " Weight can only be of length 1.",
                                               " Check for unintended consequences."
                                           ))
                                         }
                                         return(weight)
                                       })
    # normalize weights
    # policy under such conditions can be a subject of refinement
    if (sum(category$weights) == 0){
      category$weights <- rep.int(1/length(category$weights), length(category$weights))
    } else {
      category$weights <- category$weights / sum(category$weights)
    }
    
    
  
  } 
  
  category$assignments <- purrr::map(category$assignments, extract_weights)
  
  return(category)
  
}

#' Reconcile Policy File with Gradescope Data
#' 
#' This function drops any assignments present in the policy file that are not 
#' in the Gradescope data, making sure that both the policy file and Gradescope 
#' data are compatible. This function also sets any default values not explicitly 
#' specified in the policy file but required for grading.
#'
#' @param policy R list of a valid policy file
#' @param gs Gradescope data
#' @param verbose Whether or not to print messages; if FALSE, still throws error if no assignments found in gs
#' 
#' @examples
#' reconcile_policy_with_gs(policy = policy_demo, gs = gs_demo, verbose = TRUE)
#' 
#' 
#' @return A policy list
#'
#' @importFrom dplyr select rowwise summarise pull
#' @importFrom purrr map list_flatten map_lgl discard
#' @export
reconcile_policy_with_gs <- function(policy, gs, verbose = FALSE){
  # check if grades has source attr set
  if (is.null(attr(gs, "source"))){
    warning("Grades do not indicate the source. Unexpected behavior could arise. ",
                   "Reload grades from csv using read_files to prevent such behavior.")
  }
  
  
  #set_default uses gs to determine if there is a "score" key for categories made up of gs assignments
  policy <- policy |>
    set_defaults(gs, verbose = verbose)
  prev_length <- 0
  current_length <- length(policy$categories)
  #keep dropping until no more drops necessary
  while (prev_length != current_length){
    prev_length <- length(policy$categories)
    categories <- map(policy$categories, "category") |> unlist()
    assignments <- c(get_assignments(gs), categories)
    policy$categories <- map(policy$categories, function(cat){

      # drop categories with unavailable assignments/nested categories
      remaining_assignments <- cat$assignments %in% assignments
      cat$assignments <- cat$assignments[remaining_assignments]
      cat$weights <- cat$weights[remaining_assignments]
      if (length(cat$assignments) == 0){
        # if category has no assignments, drop
        return (NULL)
      }
      return (cat)
    }) |>
      purrr::discard(is.null)
    current_length <- length(policy$categories)
    
  }
  
  if (length(policy$categories) == 0){
    if (verbose){
      return (NULL)
    } 
    stop("None of the assignments in policy file are found in gs.")
  }

  # check if slip days are attempted to be used with non Gradescope data
  if ( 
    # first check to ensure source attr exists to prevent errors
    (is.null(attr(gs, "source"))) ||
    # check if grades came from Gradescope
    ((attr(gs, "source") != "Gradescope") && 
    # check if lateness is being used in the policy (flattened)
      ("slip_days" %in% names(policy)))
    ) {
      stop("Slip days are only allowed to be used with grades sourced from Gradescope.")
    }
  
  # check if lateness is attempted to be used with non Gradescope data
  if ( 
    # first check to ensure source attr exists to prevent errors
    (is.null(attr(gs, "source"))) ||
    # check if grades came from Canvas
    ((attr(gs, "source") != "Gradescope") && 
    # check if lateness is being used in the policy (flattened)
      (any(purrr::map_lgl(policy$categories, 
                        function(x){
                          "lateness" %in% names(x)
                        }))))
    ) {
    stop("Lateness calculations are only allowed with data sourced from Gradescope.")
  }
  
  gs_assignments <- get_assignments(gs)
  
  
  stu_wo_full_drop <- character(0)
  
  # determine if any students have all assignments excused in a category
  all_excused_assignments <- purrr::map(policy$categories, function(policy_item){
      # only need to test categories 100% sourced from grade assignments per my theorem
      # thm: any category ungradeable -> there exists some category with only assignments from gs that is ungradeable
      if (all(policy_item$assignments %in% gs_assignments)){
        
        stu_with_all_ex <- gs |>
          dplyr::select(policy_item$assignments) |>
          dplyr::rowwise() |>
          dplyr::summarise(nans = all(is.na(across(policy_item$assignments)))) |>
          dplyr::pull(nans)
        
        if (any(stu_with_all_ex)){
          # if any students have all assignments excused in a category, raise error detailing students and category
          return (paste0("Student(s) ", paste0(
            gs$SID[stu_with_all_ex], collapse = ", "), 
            " have no unexcused assignments in category ", policy_item$category, 
            "."))
        }
        return (NULL)
      }
    }
  ) |>
    purrr::discard(is.null) |>
    as.character()
  
  stu_wo_full_drop <- purrr::map(policy$categories, function(policy_item){
    
    if ("drop_n_lowest" %in% names(policy_item)) {
     if (all(policy_item$assignments %in% gs_assignments)){
        stu_num_na <- gs |>
          dplyr::select(policy_item$assignments) |>
          dplyr::rowwise() |>
          dplyr::summarise(nans = sum(is.na(across(policy_item$assignments)))) |>
          dplyr::pull(nans)
        
        not_full_drops <- (length(policy_item$assignments) - stu_num_na - 1) < policy_item$drop_n_lowest
        
        if (any(not_full_drops)){
          return (paste0("Student(s) ", paste0(
            gs$SID[not_full_drops], collapse = ", "), 
            " will not receive full drops in category ", policy_item$category, 
            "."))
        }
        return(NULL)
        
      }
    }
    return(NULL)
  }) |>
    purrr::discard(is.null) |>
    as.character()
  
  if (length(all_excused_assignments) > 0){
    warning(paste0(all_excused_assignments, collapse = "\n"), 
            "\nManually set grades for any effected students.")
  }
  
  if (length(stu_wo_full_drop) > 0){
    warning(paste0(stu_wo_full_drop, collapse = "\n"), 
            "\nManually set grades for any effected students.")
  }
  
  policy
}

set_defaults <- function(policy, gs, verbose = FALSE){
  default_cat <- list(
    aggregation = "equally_weighted",
    aggregation_max_pts = "sum_max_pts",
    aggregation_lateness = "max_lateness"
  ) 
  
  # add default values if missing
  policy$categories <- map(policy$categories, function(cat){
    # if min_score/max_score aggregation
    if ("aggregation" %in% names(cat) & cat[["aggregation"]] %in% c("min_score", "max_score")){
      # default for max pts aggregation is "mean_max_pts"
      default_cat[["aggregation_max_pts"]] = "mean_max_pts"
    } else {
      default_cat[["aggregation_max_pts"]] = "sum_max_pts"
    }
    
    # merge default_cat to category
    for (default_name in names(default_cat)){
      if (!(default_name %in% names(cat))){
        default <- list(default_cat[[default_name]])
        names(default) <- default_name
        cat <- append(cat, default)
      }
    }
    # # if all assignments are in gs (i.e. there are no nested categories)
    # if (!("score" %in% names(cat)) & sum(cat[["assignments"]] %in% get_assignments(gs)) != 0){
    #   # default score is raw_over_max
    #   score <- list(
    #     category = cat[["category"]],
    #     score = "raw_over_max")
    #   cat[["category"]] <- NULL #to make sure "category" is still first item
    #   cat <- append(score, cat)
    # }
    
    return (cat)
  })
  return (policy)
}

#' Flatten Policy File ("Un-nest" the Nested Structure of Policy File)
#' 
#' This function reshapes the policy file from nested to flat structure by cycling 
#' through the top-level categories of a policy file and un-nesting all 
#' subcategories to create a single level list of all categories and
#' subcategories.
#'
#' @param policy A valid policy file stored as a list.
#'
#' @return A single level list of all categories and subcategories ordered where 
#' all leaves will precede the parent category in the list order.
#'
#' @examples
#' flatten_policy(policy_demo)
#' 
#' @importFrom purrr map list_flatten
#' @export
flatten_policy <- function(policy) {
  policy$categories <- policy$categories |>
    purrr::map(extract_nested) |> 
    purrr::list_flatten()
  
  return(policy)
}

#' @importFrom purrr map list_flatten
extract_nested <- function(category) {
  
  # If there's no more nesting, return the category as a list
  if (!("assignments" %in% names(category) && is.list(category$assignments)
  )) {
    return(list(category))
  }
  
  
  # Otherwise, get nested categories
  nested_categories <- purrr::map(category$assignments, extract_nested)
  
  # Flatten the nested categories
  nested_categories_flattened <- list()
  nested_categories_flattened <- c(nested_categories_flattened, purrr::list_flatten(nested_categories))
  
  # Modify the current category's assignments
  category$assignments <- sapply(category$assignments, function(x) x$category)
  
  # Return the flattened nested categories followed by the current category
  c(nested_categories_flattened, list(category))

}

