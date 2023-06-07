#' Detect duplicate geographic coordinate pairs and generate segmented output files
#'
#' This function takes a dataset containing geographic coordinates and checks
#' for duplicate coordinate pairs. It then adds a new boolean column called
#' 'is_duplicated_coord' that indicates whether a row has a duplicate
#' coordinate (TRUE if it is a duplicate, FALSE otherwise).
#'
#' @param data The dataset containing the geographic coordinates.
#'        It should have at least two columns named 'latitude' and 'longitude'
#'        representing the coordinates.
#' @return The original dataset with an added 'is_duplicated_coord' column
#'         of type logical.
#'
#' @examples
#' data <- data.frame(latitude = c(37.7749, 40.7128, 37.7749, 34.0522),
#'                    longitude = c(-122.4194, -74.0060, -122.4194, -118.2437),
#'                    arba_code = c("ARBA1", "ARBA1", "ARBA2", "ARBA2"),
#'                    property_group = c("Group1", "Group2", "Group1", "Group2"))
#' data <- detect_duplicate_coords(data)
#' head(data)
#'
#' @import dplyr
#' @importFrom sf st_as_sf st_sfc st_set_crs st_write
#' @export
detect_duplicate_coords <- function(data) {
  duplicated_coords <- data %>%
    group_by(latitude, longitude) %>%
    mutate(is_duplicated_coord = n() > 1) %>%
    ungroup()


  return(duplicated_coords)
}
