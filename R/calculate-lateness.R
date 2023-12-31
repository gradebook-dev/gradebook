#' Compute Lateness
#' This function computes scores after lateness policy has been applied.
#'
#' @param gs A processed data in wide format
#' @param policy A flattened policy file
#'
#' @return A data frame
#'
#' @importFrom dplyr filter left_join select inner_join
#' @importFrom tidyr pivot_wider
#' @importFrom stringr str_remove_all
#' 
#' @export
calculate_lateness <- function(gs, policy){
  
  #create lateness table from flat_policy
  lateness_table <- policy |> create_lateness_table()
  
  if (is.null(lateness_table)) {
    return (gs) #if no lateness policy
  }
  
  #pivot gs into tall format
  pivotted_gs <-  gs |>
    pivot_gs()
  
  after_lateness_scores <- pivotted_gs |>
    
    #append lateness table to pivotted gs
    dplyr::inner_join(lateness_table, by = "Assignments") |>
    
    #calculate scores after lateness
    calculate_scores_after_lateness() |>
    
    #pivot dataframe back into wide format
    pivot_wider(names_from = Assignments,
                names_glue = "{Assignments} - Score",
                values_from = score_after_lateness)
  
  names(after_lateness_scores) <- str_remove_all(names(after_lateness_scores), " - Score")
  late_assigns <- names(after_lateness_scores)[(names(after_lateness_scores)) != "SID"] #all assignments with late scores
  
  #replace new lateness scores in original dataframe
  gs |>
    select(-late_assigns) |>
    left_join(after_lateness_scores, by = "SID") 
}

#' Create an Lateness Table
#' This function creates an assignment table where each row is an assignment with its category and relevant lateness information.
#'
#' @param flat_policy A validated policy file in flattened form
#'
#' @return A data frame
#'
#' @importFrom purrr map discard map_dfr
#' @importFrom data.table rbindlist
#' @importFrom dplyr mutate across starts_with left_join
#' @importFrom tibble as_tibble_row
#' @export
create_lateness_table <- function(flat_policy){

  #get lateness policies
  late_policies <- flat_policy$categories|>
    discard(function(p){
      !("lateness" %in% names(p))
    }) 
  
  if (length(late_policies) == 0){
    return (NULL)
  }
  
  names(late_policies) <- purrr::map(late_policies, "category") |> unlist()
  
  assigns_table <- map_dfr(names(late_policies), ~tibble(Assignments = late_policies[[.x]]$assignments,
                                        Category = .x
                                        ))

  late_len <- purrr::map(late_policies, function(x){
    l <- length(x$lateness)
    return (l)
  }) |> unlist() |> max()
  
  
  lateness <- map(late_policies, "lateness")
  
  lateness_by_cat <- map_dfr(names(lateness), function(x){
    individ_late_policy <- lateness[[x]] |> unlist()
    names(individ_late_policy) <- paste0(names(individ_late_policy),rep(1:late_len, each = 3))
    as_tibble_row(c(Category = x, individ_late_policy))
  })
  
  assigns_table <- left_join(assigns_table, lateness_by_cat, by = "Category") |>
    mutate(across(starts_with("from"), ~ifelse(.x %in% c(-Inf, Inf), .x,convert_to_min(.x)))) |>
    mutate(across(starts_with("to"), ~ifelse(.x %in% c(-Inf, Inf), .x,convert_to_min(.x))))
  
  return (assigns_table)
}

#' Calculate Scores after Lateness
#' This function uses the merged pivotted data + lateness table to compute
#' raw scores after lateness.
#'
#' @param lateness_table A processed pivotted data from Gradescope
#'
#' @return A data frame
#'
#' @importFrom dplyr mutate across between starts_with cur_column select
#' @importFrom stringr str_replace
#' @export
calculate_scores_after_lateness <- function(lateness_table){
  lateness_table |>
    mutate(`Lateness (H:M:S)` = as.numeric(`Lateness (H:M:S)`)) |>
    mutate(across(starts_with("to"), as.numeric),
           across(starts_with("from"), as.numeric),
           across(starts_with("scale"), as.numeric)) |> 
    mutate(across(starts_with("scale"), 
                  ~between(`Lateness (H:M:S)`,get(str_replace(cur_column(), "scale", "from")),
                           get(str_replace(cur_column(), "scale", "to")))*.x,
                  .names = '{.col}_final'
    )) |>
    mutate(final_scalar = rowSums(across(ends_with("_final")), na.rm = TRUE)) |>
    mutate(score_after_lateness = Score*final_scalar) |>
    select(SID, Assignments, score_after_lateness)
}
