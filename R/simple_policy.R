#' Simple Policy Demo File
#'
#' A demo policy file that mimics a hypothetical class syllabus
#' 
#'
#' @format A list of three nested lists
#' \describe{
#'   \item{COURSEWIDE}{first nested list with general information about the class}
#'   \item{course_name}{name of the course}
#'   \item{description}{any relevant course description, purely for the user}
#'   \item{CATEGORIES}{second nested list where syllabus is broken down into categories with their respective grading criteria}
#'   \item{name}{category name}
#'   \item{slipdays}{number of slip days}
#'   \item{late_time1, late_time2}{cutoffs for lateness penalty}
#'   \item{late_scale1, late_scale2}{lateness penalty for respective lateness cutoffs}
#'   \item{after}{if TRUE, late_scale1 is applied AFTER late_time1 has passed; if FALSE, late_scale1 is applied UNTIL late_scale2 has passed; same for late_scale2 and late_time2}
#'   \item{weight}{weight of this category in overall grade}
#'   \item{drops}{the number of lowest-score assignments that are dropped from the grade}
#'   \item{weighted_equally}{if TRUE, all assignments are weighted by their percentages; if FALSE, all assignments are weighted by their max-point value}
#'   \item{clobber}{if the clobber-category has a higher grade than this one, the clobber-category's grade replaces the grade for this category}
#'   \item{assigns}{list of assignment names that falls into this category}
#'   \item{CUTOFF}{third and final nested list where each cutoff reflects the lower bound cutoff for each letter grade: A,B,C,D,F}
#'   ...
#' }
#' @examples
#' simple_policy
#' 
#' # yaml:as_yaml(simple_policy)
#' 
"simple_policy"
