
#' Export Duplicate Geographic Coordinates
#'
#' This function takes a dataset containing geographic coordinates and exports the duplicated records
#' for each group to separate GeoPackage files. Each duplicated group is identified by the 'arba_code' column.
#'
#' @param data The dataset containing the geographic coordinates.
#'        It should have at least two columns named 'latitude' and 'longitude'
#'        representing the coordinates, and an 'arba_code' column for grouping.
#' @param path The path where the output files will be saved. Default is the current working directory.
#' @import sf
#' @import dplyr
#' @export
export_duplicates <- function(data, path = getwd()) {
  # Convert data frame to sf object, filtering out rows with NA coordinates
  data <- sf::st_as_sf(data %>% dplyr::filter(!is.na(latitude) & !is.na(longitude)), coords = c('longitude', 'latitude'), remove = FALSE, crs = 4326)
  
  # Split data into duplicated groups based on 'arba_code'
  duplicated_groups <- split(data, list(data$arba_code))
  
  # Iterate over each duplicated group
  for (duplicated_group_name in names(duplicated_groups)) {
    # Create a separate folder for each duplicated group
    folder_name <- file.path(path, duplicated_group_name)
    
    # Check if folder exists, create it if necessary
    if (!dir.exists(folder_name)) {
      dir.create(folder_name, recursive = TRUE)
    }
    
    # Retrieve duplicated group data from duplicated_groups list
    duplicated_group_data <- duplicated_groups[[duplicated_group_name]]
    
    # Filter duplicated data and perform necessary transformations
    duplicated_data <- duplicated_group_data %>%
      dplyr::filter(!is.na(is_duplicated_coord), is_duplicated_coord) %>%
      dplyr::select(arba_code, url, is_duplicated_coord, date_extracted) %>%
      dplyr::mutate(date_extracted = format(date_extracted, "%m-%Y")) %>%
      dplyr::group_by(arba_code, geometry) %>%
      dplyr::summarize(
        url_list = paste(url, collapse = ", "),
        duplicated_count = sum(is_duplicated_coord),
        date_extracted = date_extracted[1]
      )
    
    # Check if duplicated data has any rows
    if (nrow(duplicated_data) > 0) {
      # Construct file path and name for the duplicated GeoPackage file
      duplicated_file_name <- file.path(folder_name, paste0(duplicated_group_name, "_duplicates.gpkg"))
      
      # Write duplicated data to GeoPackage file, deleting any existing file with the same name
      sf::st_write(duplicated_data, dsn = duplicated_file_name, driver = "GPKG")
    }
  }
}