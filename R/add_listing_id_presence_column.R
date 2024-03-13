#' Add Boolean Column Indicating Presence of listing_id in Previous DataFrame
#'
#' This function adds a boolean column to indicate whether the 'listing_id' in each dataframe is
#' present in the previous dataframe in a list of dataframes.
#'
#' @param df_list A list of dataframes where each dataframe contains a 'listing_id' column.
#'
#' @return A modified list of dataframes with an additional boolean column indicating presence of 'listing_id'
#' in the previous dataframe.
#'
#' @details This function iterates through each dataframe in the provided list. For each dataframe except the
#' first one, it compares the 'listing_id' column of the current dataframe with the previous one. If the 'listing_id' is
#' found in the previous dataframe, the new column 'present_listing_id_in_previous' is set to TRUE, otherwise, it is set to FALSE.
#' For the first dataframe, the column is set to FALSE since there's no previous dataframe to compare against.
#'
#' @examples
#' df1 <- data.frame(listing_id = c(1, 2, 3))
#' df2 <- data.frame(listing_id = c(2, 3, 4))
#' df_list <- list(df1, df2)
#' modified_df_list <- add_listing_id_presence_column(df_list)
#'
#' @export
add_listing_id_presence_column <- function(df_list) {
  for (i in 1:length(df_list)) {
    df <- df_list[[i]]
    if (i > 1) {
      previous_df <- df_list[[i - 1]]
      df$present_listing_id_in_previous <- df$listing_id %in% previous_df$listing_id
    } else {
      df$present_listing_id_in_previous <- FALSE
    }
    df_list[[i]] <- df
  }
  return(df_list)
}
