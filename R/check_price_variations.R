#' Check for price variations for a given URL across datasets using specified base dataset(s)
#'
#' This function checks if there are variations in the 'price' variable for a given 'url'
#' across two or more datasets, using the specified base dataset(s) for comparison.
#'
#' @param current_data The base dataset for comparison.
#' @param previous_data The dataset(s) list to compare against the actual base dataset.
#' @return A dataset with the 'current_data' data and an additional column 'has_new_price' indicating price variations.
#' @export

check_price_variations <- function(current_data, previous_data) {
  # Check if at least one base dataset is provided
  if (length(previous_data) < 1) {
    stop("At least one base dataset is required to compare prices.")
  }

  # Check if 'price' and 'url' variables exist in the datasets
  if (!all(c("price", "url") %in% names(current_data))) {
    stop("The current_data dataset should contain 'price' and 'url' variables.")
  }

  # Convert current_data to data.table format
  current_data <- as.data.table(current_data)

  # Convert previous_data to data.table format
  previous_data <- lapply(previous_data, function(base) {
    if (is.data.frame(base)) {
      as.data.table(base)
    } else if (is.data.table(base)) {
      base
    } else {
      stop("One or more specified base datasets are not valid.")
    }
  })

  # Set key for current_data and previous_data
  setkey(current_data, url)
  previous_data <- lapply(previous_data, setkey, "url")

  # Perform merge and calculate 'has_new_price'
  merged_data <- merge(current_data, rbindlist(previous_data), by = "url", all.x = TRUE)
  # Subset merged_data to unique 'url' values and calculate 'has_new_price'
  merged_data <- merged_data[, .(has_new_price = any(price.x != price.y)), by = url]
  merged_data <- merge(current_data, merged_data)

  return(as.data.frame(merged_data))
}

