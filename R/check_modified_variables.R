#' Compare datasets and identify modified variables
#'
#' This function compares a dataset 'current_data' with one or more datasets 'data' based on the 'url' variable.
#' It identifies the variables that have been modified for each 'url' in 'current_data' and adds a new variable
#' indicating the modified variables. Additionally, it can generate a count variable for the modified variables.
#'
#' @param current_data The current dataset to compare.
#' @param data A list of datasets to compare with 'current_data'.
#' @param count_modified Logical indicating whether to generate a count variable for the modified variables.
#' @return The updated 'current_data' dataset with new variables indicating the modified variables and, if specified, the count of modified variables.
#'
#' @import dplyr
#' @import purrr
#' @import tidyr
#'
#' @examples
#' # Create the current dataset
#' current_data <- data.frame(url = c('url1', 'url2', 'url3'),
#'                            var1 = c(1, 2, 3),
#'                            var2 = c(4, 5, 6),
#'                            var3 = c(7, 8, 9))
#' current_data
#'
#' # Output:
#' #   url var1 var2 var3
#' # 1 url1    1    4    7
#' # 2 url2    2    5    8
#' # 3 url3    3    6    9
#'
#' # Create the datasets to compare
#' data1 <- data.frame(url = c('url1', 'url2'),
#'                     var1 = c(1, 10),
#'                     var2 = c(4, 5),
#'                     var3 = c(7, 8))
#' data2 <- data.frame(url = c('url1', 'url3'),
#'                     var1 = c(1, 2),
#'                     var2 = c(4, 50),
#'                     var4 = c(70, 8))
#'
#' # Compare datasets using the check_modified_variables function
#' result <- check_modified_variables(current_data, list(data1, data2), count_modified = TRUE)
#' result
#'
#' # Output:
#' #   url var1 var2 var3 modified_variables modified_count
#' # 1 url1    1    4    7            var1,var3              2
#' # 2 url2    2    5    8                 <NA>              0
#' # 3 url3    3    6    9                 <NA>              0
check_modified_variables <- function(current_data, data, count_modified = FALSE) {
  # Initialize a list to store modified variables for each 'url'
  modified_variables <- vector("list", length = nrow(current_data))

  # Iterate over each dataset in 'data'
  for (i in seq_along(data)) {
    # Extract the dataset
    dataset <- data[[i]]

    # Join the datasets based on 'url'
    joined_data <- dplyr::left_join(current_data, dataset, by = "url")

    # Identify modified variables for each 'url'
    for (j in seq_len(nrow(joined_data))) {
      modified_vars <- names(joined_data)[!identical(joined_data[j, ], current_data[j, ])]
      modified_variables[[j]] <- union(modified_variables[[j]], modified_vars)
    }
  }

  # Add modified variables to 'current_data'
  current_data <- dplyr::mutate(current_data, modified_variables = purrr::map_chr(modified_variables, ~ toString(unique(.))))

  # Add count of modified variables if specified
  if (count_modified) {
    current_data <- dplyr::mutate(current_data, modified_count = purrr::map_int(modified_variables, length))
  }

  return(current_data)
}
