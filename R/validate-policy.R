#' Reshape policy file from nested to flat
#' 
#' First propagates `lateness` to all child categories then cycles through the
#' top-level categories of a policy file and unnests all subcategories to create
#' a single level list of all categories and subcategories.
#'
#' @param policy A valid policy file stored as a list.
#'
#' @return A single level list of all categories and subcategories ordered where 
#' all leaves will precede the parent category in the list order.
#'
#' @examples
#' # Example
#' flatten_policy(simple_policy)
#' @importFrom purrr map list_flatten
#' @export
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


#' Copy list element to child assignment categories
#' 
#' A recursive function to copy an element of a policy file to
#' all children categories that lack that element. If a child category has that
#' element, that existing child element will not get overwritten by the parent element.
#' 
#' Can be used to propagate a field like `lateness` to all child categories of 
#' a given category.
#'
#' @param category A list from a policy file corresponding to a category like "Labs".
#' Must contain an element called `assignments`.
#' @param key A character string of the name of the element that you wish to copy.
#'
#' @return A list of the same structure as the input category, but with specified
#' element copied to all child categories that lack an element of that name.
#' 
#' @export
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
