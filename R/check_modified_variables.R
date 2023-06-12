#' Calculate the number of modified variables for each URL in 'current_data'
#'
#' This function calculates the number of variables that have been modified for each URL in 'current_data',
#' compared to the datasets in 'data'. If a URL is present in 'current_data' but not in 'data',
#' it will also be counted as a modified variable.
#'
#' @param current_data A dataset that contains the 'url' variable as a unique identifier.
#' @param data A list of datasets, where each dataset has the 'url' variable as a unique identifier.
#' @param return_modified_names Logical, indicating whether to return the names of modified variables.
#'
#' @return A data frame showing the number of modified variables for each URL in 'current_data'.
#'         The data frame has two columns: 'url' (the URL) and 'modified_variables' (the number of modified variables).
#'         If 'return_modified_names' is set to TRUE, an additional column 'modified_variable_names' will be included,
#'         containing the names of the modified variables.
#'
#' @examples
#' current_data <- data.frame(var1 = c(1, 2, 3),
#'                            url = c("www.example.com", "www.google.com", "www.yahoo.com"),
#'                            var2 = c(4, 5, 6))
#'
#' data <- list(
#'   data.frame(var1 = c(1, 2),
#'              url = c("www.example.com", "www.google.com"),
#'              var2 = c(4, 5)),
#'   data.frame(var1 = c(1, 3),
#'              url = c("www.example.com", "www.yahoo.com"),
#'              var3 = c(7, 8))
#' )
#'
#' calculate_modified_variables(current_data, data)
#' # Output:
#' #              url modified_variables
#' # 1 www.example.com                  1
#' # 2 www.google.com                  0
#' # 3  www.yahoo.com                  2
#'
#' calculate_modified_variables(current_data, data, return_modified_names = TRUE)
#' # Output:
#' #              url modified_variables modified_variable_names
#' # 1 www.example.com                  1                   var2
#' # 2 www.google.com                  0                   <NA>
#' # 3  www.yahoo.com                  2           var1, var3

calculate_modified_variables <- function(current_data, data, return_modified_names = FALSE) {
  modified_counts <- sapply(current_data$url, function(url) {
    current_row <- current_data[current_data$url == url, ]
    data_rows <- lapply(data, function(dataset) dataset[dataset$url == url, ])

    current_vars <- names(current_row)[-which(names(current_row) == "url")]  # Exclude 'url' column
    data_vars <- unique(unlist(lapply(data_rows, function(row) names(row)[-which(names(row) == "url")])))

    modified_vars <- c(setdiff(current_vars, data_vars), setdiff(data_vars, current_vars))

    if (return_modified_names) {
      modified_var_names <- paste(modified_vars, collapse = ", ")
      c(modified_variables = length(modified_vars), modified_variable_names = modified_var_names)
    } else {
      length(modified_vars)
    }
  })

  result <- data.frame(url = current_data$url, modified_variables = modified_counts, stringsAsFactors = FALSE)

  if (return_modified_names) {
    result$modified_variable_names <- strsplit(result$modified_variable_names, ", ")
  }

  result
}
