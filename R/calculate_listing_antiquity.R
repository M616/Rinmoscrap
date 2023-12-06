#' Calculate Listing Antiquity
#'
#' This function extracts the number of days or years from a column in a dataset,
#' converting days to years where applicable and handling specific strings like "yesterday" and "today".
#'
#' @param data The dataset containing the column of interest.
#' @param column_name The name of the column in the dataset to process.
#' @return The input dataset with an additional 'years' column that contains the calculated years.
#' @export
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
calculate_listing_antiquity <- function(data, column_name) {
  # Convertir la columna seleccionada a texto
  data[[column_name]] <- as.character(data[[column_name]])

  # Agregar un espacio después de los dígitos numéricos seguidos de 'días' o 'año(s)'
  data[[column_name]] <- gsub("(\\d+)(días)", "\\1 días", data[[column_name]], perl = TRUE)
  data[[column_name]] <- gsub("(\\d+)(año|años)", "\\1 año", data[[column_name]], perl = TRUE)

  # Reemplazar 'ayer' con 1 y 'hoy' con 0 en la columna seleccionada
  data[[column_name]] <- gsub("ayer", "1 días", data[[column_name]], ignore.case = TRUE)
  data[[column_name]] <- gsub("hoy", "0 días", data[[column_name]], ignore.case = TRUE)

  # Extraer el número de días o años de la columna seleccionada
  data$years <- as.numeric(gsub("\\D", "", data[[column_name]]))

  # Convertir días a años si es necesario (1 año = 365 días)
  data$years <- ifelse(grepl("\\d+\\s*días", data[[column_name]], ignore.case = TRUE),
                       data$years / 365, data$years)

  return(data)
}
