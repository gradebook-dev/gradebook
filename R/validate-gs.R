#' Process Student IDs
#'
#' This function processes a dataset with student IDs. It handles 
#' erroneous student IDs by filtering out duplicates and handling NA values. 
#' The function returns a dataframe with unique student IDs  
#'
#' @param gs_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
#'
#' @return A dataframe "unique_ids": A dataframe containing unique student IDs.
#' 
#'
#' @examples
#' # Example dataframe
#' data <- data.frame(
#'   sid = c(3032412514, NA, 3032412516,
#'           3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
#'
#'   names = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'            "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
#'   email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
#'             "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
#'             "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
#'             "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
#'             )
#'  )
#' processed_data <- process_id(data)
#' 
#' @importFrom dplyr filter group_by ungroup arrange summarize everything n across group_split last mutate across everything
#' @importFrom tidyr drop_na
#' @importFrom purrr map_dfr
#' @export

process_id <- function(gs_data) {
    
    # Remove NAs
    gs_data <- gs_data |>
        tidyr::drop_na(sid)
        
    # Identify duplicated records
    dup_sids <- gs_data |>
        dplyr::group_by(sid) |>
        dplyr::mutate(n_sid = n()) |>
        dplyr::filter(n_sid > 1) |>
        dplyr::select(-n_sid)
  
    # Apply merge_replicated_records to each duplicated sid
    de_duped_sids <- dup_sids |>
        dplyr::group_split() |>
        purrr::map_dfr(merge_replicated_records)
    
    # Attach merged rows to bottom of gs with no dups
    gs_data |>
        dplyr::group_by(sid) |>
        dplyr::mutate(n_sid = n()) |>
        dplyr::ungroup() |>
        dplyr::filter(n_sid == 1) |>
        dplyr::bind_rows(de_duped_sids)
}



#' Return a Dataframe with Duplicate Student IDs
#'
#'  Returns all the duplicate Student IDs in the data. 
#'  
#'  Must pre-process the dataframe with `check_colnames()` from gradebook package.
#'
#' @param gs_data A dataframe (csv from Gradescope) containing a column named "sid" which holds student IDs.
#'
#' @return A dataframe "get_duplicate_ids": A dataframe containing all duplicate students from the data.
#'
#' @examples
#' # Example dataframe
#' data <- data.frame(
#'   sid = c(3032412514, NA, 3032412516,
#'           3032412517, 3032412518, 3032412519, 3032412520, 3032412521, 3032412521),
#'
#'   names = c("John Smith", "Jane Doe", "Robert Brown", "Emily Johnson",
#'            "Michael Davis", "Linda Wilson", "James Taylor", "Patricia Anderson", "Patricia Anderson"),
#'   email = c("john.smith@berkeley.edu", "jane.doe@berkeley.edu", "robert.brown@berkeley.edu",
#'             "emily.johnson@berkeley.edu", "michael.davis@berkeley.edu",
#'             "linda.wilson@berkeley.edu", "james.taylor@berkeley.edu",
#'             "patricia.anderson@berkeley.edu", "patricia.anderson@berkeley.edu"
#'             )
#'  )
#' processed_data <- process_id(data)
#' 
#' @importFrom dplyr filter group_by ungroup arrange summarize everything n across group_split last
#' @export

#get only duplicates
get_duplicate_ids <- function(gs_data) {
  gs_data |>
    dplyr::group_by(sid) |>
    dplyr::mutate(n_records = n()) |>
    dplyr::filter(n_records > 1) |>
    dplyr::distinct() |>
    dplyr::select(-n_records)|>
    as.data.frame()
}

# internal function which merges replicated rows by several rules:
# 1) if cell is a datetime, it removes NAs and takes the last datetime
# 2) if cell is numeric, it takes the max
# 3) if cell is NA, leave as NA
merge_replicated_records <- function(single_sid_df) {
  
    new_id <- single_sid_df |>
        summarize(across(
            everything(),
            ~ if (lubridate::is.period(.) | 
                  is.character(.) |
                  is.POSIXct(.)
                  ) {
                ifelse(all(is.na(.)), NA, last(na.omit(.)))
            } else {
                ifelse(all(is.na(.)), NA, max(., na.rm = TRUE))
            }
        ))
    return(new_id)
}

#' Check Formatting of Column Names for Gradescope Data
#'
#' This functions checks the column names throughout the Gradescope data
#'
#' @param gs_data Grades data that's validated for having the right format in terms of names and nesting
#'
#' @return Outputs same dataframe if no error
#' @importFrom stringr str_c
#' @export
check_data_colnames_format <- function(gs_data){
    
    col_names <- colnames(gs_data)
    
    id_cols <- get_id_cols_unprocessed_data(gs_data)
    
    if ( !("sid" %in% id_cols | "SID" %in% id_cols) ){
        stop("There is no SID column")
    }
    
    assignment_names <- get_assignments_unprocessed_data(gs_data)
    
    if (is.null(assignment_names) | length(assignment_names) == 0){
        stop("There are no assignments in this dataframe")
    }
    
    
    return (gs_data)
}


#' Drop Ungraded Assignments
#'
#' This functions drops any assignments that have no grades for any students and replaced -Inf values
#'
#' @param gs_data A Gradescope data
#' @importFrom dplyr filter select
#' @importFrom purrr keep
#' @return same dataframe without graded assignments
#' @export
drop_ungraded_assignments<- function(gs_data, give_alert = TRUE){
    
    assignments <- get_assignments_unprocessed_data(gs_data, give_alert = FALSE)
    #These are the dropped assignments with all NAs for raw-score
    dropped <- gs_data |> keep(~all(is.na(.x))) |> names()
    dropped <- dropped[dropped %in% assignments]
    alert <- function() {
        cli::cli_div(theme = list(span.emph = list(color = "orange")))
        cli::cli_text("{.emph Important Message}")
        cli::cli_end()
        cli::cli_alert_info("These are your ungraded assignments: {dropped}")
    }
    if (give_alert){
        alert()
    }
    gs_data <- gs_data |> select(-contains(dropped))
}

#' Check Column Names for Gradescope Data
#'
#' This functions changes to lower case all column names throughout the Gradescope data
#'
#' @param gs_data A dataframe with column names.
#'
#' @examples
#' data <- data.frame(
#'   'SID' <- c(123,345),
#'   'NAME' = c('Al', 'Ben'),
#'   'EMAIL' = c('al@company.com', 'ben@company.com')
#' )
#' lower <- lower_colnames(data)
#'
#' @return same dataframe with lower case column names
#' @importFrom stringr str_c
#' @export
lower_colnames <- function(processed_data) {
    
    colnames(processed_data) <- tolower(colnames(processed_data))
    return (processed_data)
}
