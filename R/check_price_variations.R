#' Check for price variations for a given URL across datasets using specified base dataset(s)
#'
#' This function checks if there are variations in the 'price' variable for a given 'url'
#' across two or more datasets, using the specified base dataset(s) for comparison.
#'
#' @param actual_base The base dataset for comparison.
#' @param bases The dataset(s) to compare against the actual base dataset.
#' @return The 'actual_base' dataset with an additional column 'has_new_price' indicating price variations.
#' @export
check_price_variations <- function(actual_base, bases) {
  # Check if at least two datasets are provided
  if (length(bases) < 1) {
    stop("At least one base dataset is required to compare prices.")
  }

  # Check if 'price' and 'url' variables exist in the datasets
  if (!all(c("price", "url") %in% names(actual_base))) {
    stop("The actual_base dataset should contain 'price' and 'url' variables.")
  }

  # Check if the specified bases are present in the datasets
  if (!all(bases %in% names(.GlobalEnv))) {
    stop("One or more specified base datasets are not present.")
  }

  # Check if 'url' variable is unique in the actual base dataset
  if (any(duplicated(actual_base$url))) {
    stop("There are duplicate 'url' values in the actual base dataset.")
  }

  # Create a lookup table for the base datasets
  base_lookup <- lapply(bases, function(base) split(base$price, base$url))

  # Function to check if there are price variations for a given 'url'
  check_price_variation <- function(url) {
    actual_price <- actual_base$price[actual_base$url == url]
    base_prices <- unlist(lapply(base_lookup, function(base) base[[url]]))

    any(base_prices != actual_price)
  }

  # Apply the function to each unique 'url' in the actual base dataset
  actual_base$has_new_price <- sapply(actual_base$url, check_price_variation)

  return(actual_base)  # Return the 'actual_base' dataset with 'has_new_price' column
}
