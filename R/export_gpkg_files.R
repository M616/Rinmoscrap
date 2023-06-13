#' Export GPKG files grouped by 'arba_code' and 'property_group'
#'
#' This function exports multiple GPKG files, one for each unique combination of 'arba_code' and 'property_group' in the data.
#'
#' @param data A data frame with the variables 'arba_code' and 'property_group'.
#' @param path Destination folder where the folders and GPKG files will be saved.
#' @param include_duplicates Boolean indicating whether to include a GPKG file for the duplicated records.
#' @return NULL
#'
export_gpkg_files <- function(data, path, include_duplicates = FALSE) {
  library(sf)
  library(dplyr)

  # Convert data frame to sf object, filtering out rows with NA coordinates
  data <- st_as_sf(data %>% filter(!is.na(latitude) & !is.na(longitude)), coords = c('longitude', 'latitude'), remove = FALSE, crs = 4326)

  # Split data into groups based on unique combinations of 'arba_code' and 'property_group'
  groups <- split(data, list(data$arba_code, data$property_group))

  # If include_duplicates is TRUE, split data into groups based on unique 'arba_code' values
  duplicated_groups <- NULL
  if (include_duplicates) {
    duplicated_groups <- split(data, list(data$arba_code))
  }

  # Iterate over each group
  for (group_name in names(groups)) {
    # Extract 'arba_code' from the group name
    arba_code <- unlist(strsplit(group_name, split = "\\."))

    # Create folder path using 'path' parameter
    folder_name <- file.path(path, arba_code[1])

    # Check if folder exists, create it if necessary
    if (!dir.exists(folder_name)) {
      dir.create(folder_name, recursive = TRUE)
    }

    # Retrieve group data from groups list
    group_data <- groups[[group_name]]

    # Construct file path and name for the GPKG file
    file_name <- file.path(folder_name, paste0(group_name, ".gpkg"))

    # Write group data to GPKG file
    sf::st_write(group_data, dsn = file_name, driver = "GPKG")
  }

  # If include_duplicates is TRUE, iterate over each duplicated group
  if (include_duplicates) {
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
        filter(!is.na(is_duplicated_coord), is_duplicated_coord) %>%
        select(arba_code, url, is_duplicated_coord, date_extracted) %>%
        mutate(date_extracted = format(date_extracted, "%m-%Y")) %>%
        group_by(arba_code, geometry) %>%
        summarize(
          url_list = paste(url, collapse = ", "),
          duplicated_count = sum(is_duplicated_coord),
          date_extracted = date_extracted[1]
        )

      # Check if duplicated data has any rows
      if (nrow(duplicated_data) > 0) {
        # Construct file path and name for the duplicated GPKG file
        duplicated_file_name <- file.path(folder_name, paste0(duplicated_group_name, "_duplicates.gpkg"))

        # Write duplicated data to GPKG file, deleting any existing file with the same name
        sf::st_write(duplicated_data, dsn = duplicated_file_name, driver = "GPKG")
      }
    }
  }
}
