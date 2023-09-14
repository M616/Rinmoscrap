#' Filter a dataframe by trimming the tails of a specified column
#'
#' This function trims the tails of a distribution in a specified column of a dataframe,
#' based on the percentage to exclude from the lower and upper ends of the distribution.
#'
#' @param data The input dataframe
#' @param column The name of the column in the dataframe to be trimmed
#' @param lower_percentage The percentage of observations to exclude from the lower end of
#' the distribution
#' @param upper_percentage The percentage of observations to exclude from the upper end of
#' the distribution
#' @return The filtered dataframe after trimming the specified column
#' @export
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(ID = 1:10, Values = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
#'
#' # Filter the dataframe by trimming the "Values" column, excluding 20% from the lower end
#' #and 10% from the upper end
#' filtered_df <- trim_tails(df, "Values", 20, 10)
#' print(filtered_df)
#'
trim_tails <- function(data, column, lower_percentage, upper_percentage) {
  # Get the column from the dataframe
  variable <- data[[column]]

  # Calculate the number of observations to exclude on each side
  n <- length(variable)
  lower_exclude <- round(n * lower_percentage / 100)
  upper_exclude <- round(n * upper_percentage / 100)

  # Sort the variable in ascending order
  sorted_data <- sort(variable)

  # Trim the tails by excluding the specified percentages of observations from each side
  trimmed_data <- sorted_data[(lower_exclude + 1):(n - upper_exclude)]

  # Filter the original dataframe based on the trimmed data
  filtered_data <- data[data[[column]] >= min(trimmed_data) & data[[column]] <= max(trimmed_data), ]


  return(filtered_data)
}
