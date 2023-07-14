# Function to filter a dataframe based on the count of unique digits in a specified column
# Parameters:
#   - data: A dataframe containing the data
#   - column: The name of the column in the dataframe to apply the function to
# Returns:
#   The filtered dataframe where the count of unique digits in the specified column is greater than 1

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
