file = "stat20_s23_policy.yml"

read_policy <- function(file) {
    pol <- yaml::read_yaml(file)
    pol2 <- replace_inf(pol)
}

replace_inf <- function(lst) {
    lapply(lst, function(x) {
        if (is.list(x)) {
            replace_inf(x)
        } else if (is.character(x)) {
            ifelse(x == "Inf", Inf, x)
            ifelse(x == "-Inf", -Inf, x)
        } else {
            x}})
}

#' @importFrom purrr map list_flatten
flatten_policy <- function(policy) {
    policy$categories <- policy$categories |>
        purrr::map(\(x) copy_element_to_children(x, key = "lateness")) |>
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

copy_element_to_children <- function(category, key) {
    
    # if the category has no children, just return the category
    if (is.vector(category$assignments, mode = "character")) {
        return(category)
    }
    
    # for every child assignment...
    for (child in seq_along(category$assignments)) {
        
        # if the key isn't found in the child list, copy it there
        if (!(key %in% names(category$assignments[[child]]))) {
            category$assignments[[child]][[key]] <- category[[key]]
        }
        
        # if the child assignment has a child, call the function again
        if (is.list(category$assignments[[child]]$assignments)) {
            category$assignments[[child]] <- copy_element_to_children(category$assignments[[child]], key)
        }
    }
    
    return(category)
}
