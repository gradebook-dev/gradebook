#' Drop NA Assignments
#' 
#' This function drops any assignments that aren't assigned to a category
#'
#' @param merged_files merged file of pivot_df + policy
#'
#' @return data frame with only assigned assignments
#' @importFrom tidyr drop_na
#' @export
drop_na_assignments <- function(merged_files){
    merged_files |>
        tidyr::drop_na(category)
}

#' Intermediary Grading Computations
#' 
#' This function creates columns of computations needed for grading, like number of relevant
#' assignments and late_time1/late_time2 in terms of minutes
#'
#' @param merged_files merged file with only assigned assignments
#'
#' @return data frame with only assigned assignments and intermediary columns
#' @importFrom dplyr group_by mutate ungroup case_when
#' @export
intermediary_computations <- function(merged_files){
    #num_assigns
    merged_files |>
        dplyr::group_by(sid, category) |>
        dplyr::mutate(num_assigns = n()) |>
        dplyr::ungroup() |>
        dplyr::mutate(relevant_assigns = dplyr::case_when(
            is.numeric(drops) ~ num_assigns-drops,
            !is.numeric(drops) ~ 1), #if drop is "min" or "max"
        lateness_min1 = convert_to_min(late_time1),
        lateness_min2 = convert_to_min(late_time2),
        assign_lateness = convert_to_min(`lateness_(h_m_s)`)
        )
    
}

#' Convert to Min
#' 
#' Converted a time in HH:MM:SS format to minutes
#'
#' @param hms time in HH:MM:SS format
#'
#' @return number of minutes
#' @importFrom lubridate hms period_to_seconds 
#' @export
convert_to_min <- function(hms){
    save <- lubridate::hms(hms)
    save <- lubridate::period_to_seconds(save)
    save <- save/60
    return (save)
}


# - points/score after lateness
calculate_score_after_lateness <- function(computed_files) {
    df_after <- computed_files |>
        filter(after) |>
        mutate(points_after_lateness = case_when(
            #if late_time1 < lateness <= late_time2, scale by late_scale1
            assign_lateness > lateness_min1 & assign_lateness <= lateness_min2 ~ raw_points*as.numeric(late_scale1),
            #if late_time2 < lateness, scale by late_scale2
            assign_lateness > lateness_min2 ~ raw_points*as.numeric(late_scale2),
            #if lateness <= late_time1, scale by 1
            TRUE ~ raw_points
        )) |>
        mutate(score_after_lateness = points_after_lateness/max_points)
    
    df_until <- computed_files |>
        filter(!after) |>
        mutate(points_after_lateness = case_when(
            #if lateness <= late_time1, scale by late_scale2
            assign_lateness >= lateness_min1 ~ raw_points*as.numeric(late_scale1),
            #if late_time1 < lateness <= late_time2, scale by late_scale2
            assign_lateness <= lateness_min2 ~ raw_points*as.numeric(late_scale2),
            #if late_time2 < lateness, scale by 0
            TRUE ~ 0
        ))
    
    df_final <- rbind(df_after, df_until) |>
        arrange(sid)
}

# - drops (can do min/max too??)

drop_assignments <- function(computed_files_after_lateness){
    df_with_drops <- df_with_lateness |>  # default if no drops in class
        mutate(dropped = FALSE) #all assignments are defaulted to no drops
    
    #if drops = 0 OR num_assigns = 1, NO drops
    
    #if min, do min calculations
    
    #if relevant_assigns <= 0, (i.e. more drops than assignments, OR drops = num_assigns), take max
    #if max OR relevant_assigns <= 0, relevant_assigns = 1
    #if relevant_assigns > 0, no change to relevant_assigns
    
    

    # df_with_drops <- df_with_drops |>
    #     mutate(relevant_assigns = case_when(
    #         relevant_assigns <=0 ~ 1,
    #         true ~ relevant_assigns
    #     ))
    # 
    # #if drop = "min", take min
    # drop_min <- df_with_drops |>
    #     filter(drops = "min") |>
    #     group_by(sid, category) |>
    #     arrange(desc(score_after_lateness)) |> #arrange in descending order based on group_by
    #     slice(1: as.numeric(num_assigns-1)) |> #drop everything except min
    #     mutate(dropped = TRUE) #these are DROPPED
    # 
    # keep_min <- df_with_drops |>
    #     filter(drops = "min") |>
    #     group_by(sid, category) |>
    #     arrange(score_after_lateness) |> #arrange in ascending order based on group_by
    #     slice(1) |> #keep minimum value
    #     mutate(dropped = FALSE) #these are KEPT
    # 
    # drop_rest <- df_with_drops |>
    #     filter(drops != "min") |>
    #     group_by(sid, category) |>
    #     arrange(score_after_lateness) |> #arrange in ascending order based on group_by
    #     slice(1) |> #keep minimum value
    #     mutate(dropped = TRUE) #these are DROPPED
    # 
    # keep_rest <- df_with_drops |>
    #     filter(drops != "min") |>
    #     group_by(sid, category) |>
    #     arrange(score_after_lateness) |> #arrange in ascending order based on group_by
    #     slice(1) |> #keep minimum value
    #     mutate(dropped = FALSE) #these are KEPT
    
    
}
# - do category grading by weights
# 
# - calculate overall grades

