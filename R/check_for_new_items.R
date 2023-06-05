#' Check for new items in the 'url' variable and add 'is_new_listing' column
#'
#' This function compares the current dataset with previous datasets, checks for new items in the 'url' variable,
#' and adds a new boolean column 'is_new_listing' indicating whether each record is a new item.
#'
#' @param current_data The current dataset that contains the 'url' variable.
#' @param previous_data A list of previous datasets, where each element is a dataset with the same structure and 'url' variable.
#' @param new_column_name The name of the new column indicating whether each record is a new item (default: 'is_new_listing').
#' @return The updated dataset with the new 'is_new_listing' column.
#'
check_for_new_items <- function(current_data, previous_data, new_column_name = 'is_new_listing') {
  current_data[[new_column_name]] <- TRUE

  for (data in previous_data) {
    current_urls <- current_data[["url"]]
    previous_urls <- previous_data[["url"]]
    new_items <- !(current_urls %in% previous_urls)
    if (length(new_items) < nrow(current_data)) {
      new_items <- c(new_items, rep(FALSE, nrow(current_data) - length(new_items)))
    }
    current_data[[new_column_name]] <- current_data[[new_column_name]] & new_items
  }

  return(current_data)
}
