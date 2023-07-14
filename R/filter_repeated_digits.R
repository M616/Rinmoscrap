#' Filter a dataframe based on the count of unique digits in a specified column
#'
#' This function takes a dataframe and filters the data based on the condition
#' that the count of unique digits is greater than 1 in the specified column.
#'
#' @param data A dataframe containing the data
#' @param column The name of the column in the dataframe to apply the function to
#' @return The filtered dataframe where the count of unique digits in the specified column is greater than 1
#'
#' @examples
#' # Example dataframe
#' df <- data.frame(ID = 1:5, Numbers = c(123, 4567, 89, 1010, 222))
#'
#' # Apply the function to the "Numbers" column and filter the dataframe
#' filtered_df <- filter_repeated_digits(df, "Numbers")
#' print(filtered_df)
#'
#'@export
#'
filter_repeated_digits <- function(data, column) {
  # Apply the function to each element in the specified column
  result <- sapply(data[[column]], function(element) {
    # Convert the element to a character
    elemento <- as.character(element)

    # Split the character into individual digits
    digitos <- strsplit(elemento, "")[[1]]

    # Get the unique digits
    digitos_unicos <- unique(digitos)

    # Count the number of unique digits
    cantidad <- length(digitos_unicos)

    return(cantidad)
  })

  # Filter the dataframe based on the condition
  filtered_data <- data[result > 1, ]

  return(filtered_data)
}
