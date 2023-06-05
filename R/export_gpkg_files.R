#' Export GPKG files grouped by 'codigo_arba' and 'property_group'
#'
#' This function exports multiple GPKG files, one for each unique combination of 'codigo_arba' and 'property_group' in the data.
#'
#' @param data A data frame with the variables 'codigo_arba' and 'property_group'.
#' @param path Destination folder where the folders and GPKG files will be saved.
#' @return NULL
#'
export_gpkg_files <- function(data, path) {
  library(sf)

  groups <- split(data, list(data$codigo_arba, data$property_group))

  for (group_name in names(groups)) {
    codigo_arba <- unlist(strsplit(group_name, split = "\\."))
    folder_name <- file.path(path, codigo_arba[1])
    if (!dir.exists(folder_name)) {
      dir.create(folder_name, recursive = TRUE)
    }
    file_name <- file.path(folder_name, paste0(group_name, ".gpkg"))
    sf::st_write(groups[[group_name]], dsn = file_name, driver = "GPKG")
  }
}




