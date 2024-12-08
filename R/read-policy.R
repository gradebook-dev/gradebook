#' Read Grading Policy .yml
#'
#' This functions reads the grading policy YAML file and checks for correct format
#'  
#'
#' @param path Path to YAML policy file
#' @param verbose Whether or not to print messages and warnings
#'
#' @examples
#' path <- system.file("extdata", "policy_demo.yaml", package = "gradebook")
#' read_policy(path = path, verbose = TRUE)
#' 
#' @return R list of the policy file, if no errors
#' @importFrom yaml read_yaml
#' @export
read_policy <- function(path, verbose = FALSE){
  # read in yaml
  policy <- yaml::read_yaml(path) |>
    check_keys(verbose = verbose)
  policy
}

check_keys <- function(policy, verbose = FALSE){
  #needs to be written
  flat_policy <- flatten_policy(policy)
  #check that all categories have the "category" and "assignments" key
  purrr::walk(policy$categories, function(cat){
    if (!("category" %in% names(cat) & "assignments" %in% names(cat))){
      stop(paste0("Not all categories have a category and assignments argument"))
    }
  })
  
  #check that all keys are defined in package
  purrr::walk(policy$categories, function(cat){
    valid_keys <- c("category","score", "aggregation", "lateness", 
                    "drop_n_lowest","weight", "weights", "aggregation_max_pts", 
                    "aggregation_lateness", "assignments")
    if ("weights" %in% names(cat)){
      warning(paste0("The weights argument in Category ", cat$category, " may be overwritten.",
                     " THIS COULD RESULT IN UNINTENDED GRADES!"))
    }
    if (sum(names(cat) %in% valid_keys) != length(names(cat))){
      stop(paste0("Category ", cat$category, " has incorrect key(s) ",
                  paste(names(cat)[-(names(cat) %in% valid_keys)], 
                        collapse = ", ")))
    }
  })
  
  policy
}

#' Get the Category Names for Policy
#'
#' This function identifies the names of all categories in a policy list.
#'
#' @param policy R list of a valid policy file
#' @param verbose Whether or not to return an alert of categories
#' 
#' @examples
#' get_categories(policy_demo, verbose = TRUE)
#' 
#' @return A vector of the names of the categories in the policy R list
#' @importFrom purrr map
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_categories <- function(policy, verbose = FALSE){
  policy <- flatten_policy(policy)
  
  categories <- purrr::map_chr(policy$categories, "category")
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The categories from the policy are {categories}")
  }
  
  if (verbose){
    alert()
  }
  
  return(categories)
}

#' Get the Nested Assignments for Category
#'
#' This function identifies the nested assignments for a given category.
#'
#' @param category a category from a policy R list
#' @param verbose Whether or not to return an alert of assignments
#' 
#' @examples
#' category <- policy_demo[["categories"]][[1]]
#' get_nested_assignments(category = category, verbose = TRUE)
#' 
#' @return A vector of the names of the nested assignments in the category
#' @importFrom purrr map
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_nested_assignments <- function(category, verbose = FALSE){
  assignments <- extract_nested_assignments(category) |> 
    unlist()
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The assignments from the category {category$category} are {assignments}")
  }
  
  if (verbose){
    alert()
  }
  
  return(assignments)
}

#' @importFrom purrr map
extract_nested_assignments <- function(category){
  if (!is.list(category$assignments)){
    return(category$assignments)
  } else{
    purrr::map(category$assignments, function(nested_cat){
      get_nested_assignments(nested_cat)
    })
  }
}
