#' Detect duplicate geographic coordinate pairs and generate segmented output files
#'
#' This function takes a dataset containing geographic coordinates and checks
#' for duplicate coordinate pairs. It then adds a new boolean column called
#' 'is_duplicated_coord' that indicates whether a row has a duplicate
#' coordinate (TRUE if it is a duplicate, FALSE otherwise). It also generates
#' segmented output files based on the 'arba_code' and 'property_group' variables,
#' containing the cases where 'is_duplicated_coord' is TRUE. The output files
#' are labeled with the corresponding 'arba_code' and 'property_group' values.
#' The files are saved in .gpkg format and include the columns 'geometry', 'url',
#' and the count of coordinate pair repetitions.
#'
#' @param data The dataset containing the geographic coordinates.
#'        It should have at least two columns named 'latitude' and 'longitude'
#'        representing the coordinates.
#' @param output_dir The directory path where the segmented output files will be saved.
#' @return The original dataset with an added 'is_duplicated_coord' column
#'         of type logical.
#'
#' @examples
#' data <- data.frame(latitude = c(37.7749, 40.7128, 37.7749, 34.0522),
#'                    longitude = c(-122.4194, -74.0060, -122.4194, -118.2437),
#'                    arba_code = c("ARBA1", "ARBA1", "ARBA2", "ARBA2"),
#'                    property_group = c("Group1", "Group2", "Group1", "Group2"))
#' data <- detect_duplicate_coords(data, output_dir = "output_files")
#' head(data)
#'
#' @import dplyr
#' @importFrom sf st_as_sf st_sfc st_set_crs st_write
#' @export
detect_duplicate_coords <- function(data, output_dir) {
  duplicated_coords <- data %>%
    group_by(latitude, longitude) %>%
    mutate(is_duplicated_coord = n() > 1) %>%
    ungroup()

  unique_groups <- unique(select(duplicated_coords, arba_code, property_group))

  for (i in 1:nrow(unique_groups)) {
    group <- unique_groups[i, ]
    filtered_data <- duplicated_coords %>%
      filter(arba_code == group$arba_code, property_group == group$property_group)

    duplicates <- filtered_data %>%
      filter(is_duplicated_coord) %>%
      group_by(latitude, longitude) %>%
      mutate(count = n()) %>%
      select(geometry = c(latitude, longitude), url, count) %>%
      distinct()  # Eliminar duplicados en caso de contar con más de una repetición

    duplicates_sf <- st_as_sf(duplicates,
                              coords = c("latitude", "longitude"),
                              crs = 4326)

    output_file <- file.path(output_dir, paste0(group$arba_code, "_", group$property_group, "_duplicates.gpkg"))
    st_write(duplicates_sf, output_file)
  }

  return(duplicated_coords)
}
