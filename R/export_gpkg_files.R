#' Export GPKG files grouped by 'codigo_arba' and 'property_group'
#'
#' This function exports multiple GPKG files, one for each unique combination of 'codigo_arba' and 'property_group' in the data.
#'
#' @param data A data frame with the variables 'codigo_arba' and 'property_group'.
#' @param path Destination folder where the folders and GPKG files will be saved.
#' @param include_duplicates Boolean indicating whether to include a GPKG file for the duplicated records.
#' @return NULL
#'
export_gpkg_files <- function(data, path,include_duplicates = FALSE)  {
  library(sf)
  library(dplyr)

  data <- st_as_sf(data %>% filter(!is.na(latitude)&!is.na(longitude) ), coords=c('longitude','latitude'),remove=FALSE, crs=4326)
  groups <- split(data, list(data$arba_code, data$property_group))

  for (group_name in names(groups)) {
    arba_code <- unlist(strsplit(group_name, split = "\\."))
    folder_name <- file.path(path, arba_code[1])
    if (!dir.exists(folder_name)) {
      dir.create(folder_name, recursive = TRUE)
    }
    group_data <- groups[[group_name]]
    file_name <- file.path(folder_name, paste0(group_name, ".gpkg"))
    sf::st_write(groups[[group_name]], dsn = file_name, driver = "GPKG")

    if (include_duplicates) {
      duplicated_data <- data[data$is_duplicated_coord == TRUE,c("arba_code","url","is_duplicated_coord","date_extracted" )] |>
        mutate(date_extracted=format(date_extracted, "%m-%Y")) |>
        group_by(arba_code,geometry) |>
        summarize(url_list = paste(url, collapse = ", "),
                  duplicated_count=sum(is_duplicated_coord),
                  date_extracted=date_extracted[1]) |>
        slice(1)

      if (nrow(duplicated_data) > 0) {
        duplicated_file_name <- file.path(folder_name, paste0(group_name, "_duplicates.gpkg"))
        sf::st_write(duplicated_data, dsn = duplicated_file_name, driver = "GPKG" )
        }
    }
  }
}
