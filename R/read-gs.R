#' Read Gradescope CSV File
#'
#' This functions reads the Gradescope CSV file and checks for correct Gradescope
#' format. Each assignment should follow the four-column format:
#' `Assignment Name`, `Assignment Name - Max Points`, `Assignment Name - Submission Time`,
#' `Assignment Name - Lateness (H:M:S)`. All other columns are designed as ID columns.
#'  
#' Now it is favored to use read_files. 
#' This function is left for backwards-compatibility.
#' @param path Path to Gradescope CSV
#' @param verbose whether or not to print messages
#'
#' @return A dataframe of Gradescope CSV file, if no errors
#' @importFrom readr read_csv
#' @importFrom dplyr mutate across cur_column mutate_at vars all_of ends_with
#' @examples
#' path <- system.file("extdata", "gs_demo.csv", package = "gradebook")
#' read_gs(path = path)
#' @export
read_gs <- function(path, verbose = FALSE){
  # read in csv
  gs <- read_csv(path, trim_ws = FALSE) |>
    read_gradescope_grades()
}


#' Read in File(s) from Grading Platform(s)
#'
#' This function will read in all input data to Gradebook.
#' @param grades_path Is the path to the csv containing graded assignments.
#' @param other_file_paths Is a list containing any other filepaths desired to be read in. Each name should be a designated type of file. For example, "lateness" and "roster" and the value should be the filepath.
#' @param source Determines how to read the grade file. By default, it is auto so that Gradebook can determine what format the file is in. Alternatively, users can specify "Gradescope" or "Canvas" to force certain interpretations.
#' @return A dataframe of grades. Later support for other files will be returned as a list as well.
#' @importFrom readr read_csv
#' @export
read_files <- function(grades_path, 
                       other_file_paths = list(), 
                       source = "auto"){
  # first read in all of the files
  
  # beginning with the grades csv
  
  
  grades <- readr::read_csv(grades_path, trim_ws = FALSE)
    
  
  # now read in other supplied data 
  for (data_type in names(other_file_paths)){
    warning(paste0(data_type, 
                  " is not currently supported as an extra file source."))
  }
  
  # now determine how to read in data
  
  auto_determine <- determine_grade_source(grades)
  
  if (source == "auto"){
    
    
    if (auto_determine == "Gradescope"){
      
      
      return(read_gradescope_grades(grades))
    } else if (auto_determine == "Canvas"){
      # the read_canvas_grades does all of the lifting
      return(read_canvas_grades(grades))
    } else {
      stop("Data source is unrecognized.")
    }
    
  } else if (source == "Gradescope"){
    if (source != auto_determine){
      warning(paste0(
        "Grade data will be processed as ", source, 
        " data due to value of source parameter. ",
        "However, the data appears to be from source ",
        auto_determine, "."
      ))
    }
    
    
    
    return(read_gradescope_grades(grades))
      
  } else if (source == "Canvas"){
    if (source != auto_determine){
      warning(paste0(
        "Grade data will be processed as ", source, 
        " data due to value of source parameter. ",
        "However, the data appears to be from source ",
        auto_determine, "."
      ))
    }
    # the read_canvas_grades does all of the lifting
    return(read_canvas_grades(grades))
    
  } else {
    stop(paste0(
      "Specified source type ", source,
      " is not currently supported."
    ))
  }
  
  
 
}


#' @importFrom dplyr mutate_at if_else
#' @importFrom tidyr replace_na
read_gradescope_grades <- function(df){
  # almost identity function when reading in gradescope grades 
  # to create symmetry with read_canvas_grades
  
  attr(df, "source") <- "Gradescope"
  
  assignments <- get_assignments(df)
  
  # missing assignments become 0s
  
  df |> 
    dplyr::mutate_at(assignments, impute_missing_excused_input) |>
    check_data_format()
}



#' @importFrom stringr str_extract str_match
#' @importFrom dplyr filter select slice rename_with rename bind_cols mutate_at
#' @importFrom purrr discard
#' 
#' 
read_canvas_grades <- function(grades){
  # Convert Canvas grade dataframe into standard format
  #
  # Takes in a dataframe uploaded from Canvas. 
  # Returns a dataframe in our standardized format (ie like Gradescope).
  #
  #
  
  # first determine which columns are assignments
  
  # canvas categories will not be kept
  
  assignments <- stringr::str_extract(names(grades),
                                      ".+\\s*\\(\\d+\\)") |>
                purrr::discard(is.na)
  
  # coerce into numerical and convert excused assignments into set value
  grades <- grades |>
    dplyr::filter(!is.na(Student)) |>
    dplyr::mutate_at(assignments, impute_missing_excused_input)
  
 
  # now reconfigure the points possible info
  max_points <- dplyr::filter(grades, Student %in% c( "    Points Possible", "Points Possible")) |>
    dplyr::select(all_of(assignments)) |>
    dplyr::slice(rep.int(1, nrow(grades))) |>
    dplyr::rename_with(paste0, everything(), " - Max Points")
  
  
  # remove unneeded cols, add max_point column, and remove rows that are not students
  grades <- grades |>
    dplyr::select(c(all_of(assignments), "ID", "Student", 
                    "SIS User ID", "Section")) |>
    dplyr::bind_cols(max_points) |>
    dplyr::filter(!(Student %in% c("    Points Possible", "Student, Test", "Points Possible")))
  
  # add columns for submission time and lateness
  
  sub_cols <- paste0(assignments, " - Submission Time")
  
  late_cols <- paste0(assignments, " - Lateness (H:M:S)")
  
  grades[sub_cols] <- NA
  
  grades[late_cols] <- "00:00:00"
  
  # extract student name data
  
  grades$`First Name` <- (stringr::str_match(grades$Student, 
                                             ".+,\\s(.+)"))[, 2]
  
  grades$`Last Name` <- (stringr::str_match(grades$Student, 
                                            "(.+),.+"))[, 2]
  
  # change names to match Gradescope convention
  grades <- grades |>
    dplyr::rename(SID = `SIS User ID`,
                  Sections = Section) |>
    # reorder columns to match Gradescope
    dplyr::select(c("First Name", "Last Name", "SID", "Sections",
                    as.vector(
                            mapply(c,
                                   assignments, 
                                    paste0(assignments, " - Max Points"), 
                                    sub_cols,
                                    late_cols)
                              )
                    )
                  ) 
  
  attr(grades, "source") <- "Canvas"
  
  check_data_format(grades)
  
}


#' @importFrom tidyr replace_na
#' @importFrom dplyr if_else
impute_missing_excused_input <- function(x) {
  # this function will impute missing and excused assignments upon read-in
  # it is a helper function for read_gradescope_grades and read_canvas_grades
  # Logic: Missing assignments (read in as NAs) get 0 score
  # Excused assignments (read in as "EX") get NA value
  if(is.character(x)){
    # if character, we also have to deal with "EX"
    x <- tidyr::replace_na(x, "0") 
    out <- dplyr::if_else(x == "EX", NA, x) |>
      as.numeric()
    return(out)
  } else if (is.logical(x)) {
    replace_val <- as.logical(0)
  } else if (is.numeric(x)) {
    replace_val <- 0
  } else {
    stop("Unsupported Datatype")
  }
  # now we do not need to deal with "EX"
  tidyr::replace_na(x, replace_val) |>
    as.numeric()
}

#' Predict the source of grade dataframe.
#'
#' This function performs the auto-determination of where the grades dataframe was sourced.
#' @param grades_df This is the input Dataframe
#' @returns A string "Canvas", "Gradescope", or "Unrecognized" of the determination.
#' @export
determine_grade_source <- function(grades_df){
  
  gs_assignments <- get_assignments(grades_df)
  columns <- names(grades_df)
  
  if (length(gs_assignments) >= 1){
    return("Gradescope")
  } else if (all(c("Student", "ID") %in% columns)){
   return("Canvas")
  } else {
   return("Unrecognized")
  }
   
}



#' Check Formatting of Grades Data
#'
#' This functions checks the column names throughout the Grades data.
#' There must be an SID column and at least one assignment.
#' It also gives an alert for what ID columns and assignments are in the data.
#'
#' @param gs Gradescope data frame
#' @param verbose Whether or not to print messages and warnings
#'
#' @examples
#' check_data_format(gs_demo, verbose = TRUE)
#' 
#' @return Same gs dataframe if no errors.
#' @export
check_data_format <- function(gs, verbose = FALSE){
  
  col_names <- colnames(gs)
  
  id_cols <- get_id_cols(gs, verbose = verbose)
  
  if ( !("SID" %in% id_cols) ){
    stop("There is no SID column")
  }
  
  assignment_names <- get_assignments(gs, verbose = verbose)
  
  if (is.null(assignment_names) | length(assignment_names) == 0){
    stop("There are no assignments in this dataframe")
  }
  
  #if correct format, return same dataframe
  return (gs)
}

#' Get the ID Columns for Gradescope Data
#'
#' This function identifies the ID columns from Gradescope data.
#'
#' @param gs  Gradescope dataframe
#' @param verbose Whether or not to return an alert of assignments
#' 
#' @examples
#' get_id_cols(gs_demo, verbose = TRUE)
#' 
#' @return A vector of the names of the ID columns in the dataframe
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_id_cols <- function(gs, verbose = FALSE) {
  # REGEX pattern: case INsensitive, then matches the extensions
  regex <- "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  repeating <- names(base_name_counts[base_name_counts == 4])
  
  # identify columns to keep: those not repeating 4 times
  columns_to_keep <- names(gs)[!(base_names %in% repeating)]
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The ID columns from Gradescope are {columns_to_keep}")
  }
  
  if (verbose){
    alert()
  }
  
  return(columns_to_keep)
}

#' Get the Assignment Names for Gradescope Data
#'
#' This function identifies the assignments from Gradescope data and returns the assignments' names.
#'
#' @param gs Gradescope dataframe
#' @param verbose Whether or not to print assignment names
#'
#' @examples
#' get_assignments(gs_demo, verbose = TRUE)
#' 
#' @return A vector of the names of the assignments in the dataframe
#' @importFrom stringr str_replace_all regex
#' @importFrom cli cli_alert_info cli_div cli_text cli_end
#' @export

get_assignments <- function(gs, verbose = FALSE){
  # REGEX pattern: case INsensitive, then matches the extensions
  # works with untouched GS dataframe so we can match the pattern
  regex = "(?i)( - max points| - submission time| - lateness \\(h:m:s\\))"
  
  # extract base names and excludes the extensions (max points, submission time and lateness)
  base_names <- stringr::str_replace_all(names(gs), regex, "")
  
  # Count occurrences of base names
  base_name_counts <- table(base_names)
  
  # identify base names that repeat exactly 4 times
  assignment_names <- names(base_name_counts[base_name_counts == 4])
  
  alert <- function() {
    cli::cli_div(theme = list(span.emph = list(color = "orange")))
    cli::cli_text("{.emph Important Message}")
    cli::cli_end()
    cli::cli_alert_info("The assignments from Gradescope are {assignment_names}")
  }
  
  if (verbose){
    alert()
  }
  
  return (assignment_names)
}