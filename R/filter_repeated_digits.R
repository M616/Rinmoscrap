#' Filter a dataframe based on the count of unique digits in a specified column
#'
#' This function takes a dataframe and filters the data based on the condition
#' that the count of unique digits is greater than 1 in the specified column.
#' It retains rows with NA values in the specified column.
#'
#' @param data A dataframe containing the data
#' @param column The name of the column in the dataframe to apply the function to
#' @return The filtered dataframe where the count of unique digits in the specified column is greater than 1 or NA
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(ID = 1:5, Numbers = c(123, 4567, 89, 1010, 222))
#'
#' # Apply the function to the "Numbers" column and filter the dataframe
#' filtered_df <- filter_repeated_digits(df, "Numbers")
#' print(filtered_df)
#'
#' @import dplyr
#'
#' @export
#'
filter_repeated_digits <- function(data, column) {
  filtered_data <- data %>%
    mutate(CountUniqueDigits = sapply(data[[column]], function(element) {
      if (is.na(element)) {
        return(NA)
      } else {
        element_str <- as.character(element)
        digits <- strsplit(element_str, "")[[1]]
        unique_digits <- unique(digits)
        return(length(unique_digits))
      }
    })) %>%
    filter(is.na(CountUniqueDigits) | CountUniqueDigits > 1) %>%
    select(-CountUniqueDigits)

  return(filtered_data)
}
