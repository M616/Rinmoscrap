#' missing_replace
#'
#' This function replaces empty strings ('') and 'NaN' values in a data frame with NA.
#'
#' @param data A data frame or tibble.
#' @description The function takes a data frame or tibble as input and replaces empty strings ('') and 'NaN' values with NA.
#' @return A modified data frame or tibble with empty strings and 'NaN' values replaced by NA.
#' @export
#'
#' @examples
#' # Create a data frame with missing values
#' df <- data.frame(A = c('', 'value', 'NaN'), B = c('value', '', 'NaN'), stringsAsFactors = FALSE)
#' df
#'
#' # Output:
#' #      A     B
#' # 1         value
#' # 2 value
#' # 3 NaN   NaN
#'
#' # Apply the missing_replace function
#' modified_df <- missing_replace(df)
#' modified_df
#'
#' # Output:
#' #      A     B
#' # 1  <NA>  value
#' # 2 value  <NA>
#' # 3  <NA>  <NA>
missing_replace <- function(data) {
  data <- data %>% mutate(across(where(is.character), ~ na_if(., ''))) %>%
    mutate(across(where(is.character), ~ na_if(., 'NaN'))) |>
    mutate(across(where(is.numeric), ~ replace(., is.na(.), NA)))


  return(data)
}
