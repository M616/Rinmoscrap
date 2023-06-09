#' Spatial join with ARBA polygon base and attribute recoding
#'
#' This function performs a spatial join between a spatial object and the ARBA polygon base. It also recodes the resulting attributes using the official nomenclature.
#'
#' @param data A spatial object representing points, lines, or polygons.
#'
#' @return A modified spatial object with additional attributes obtained from the spatial join with the ARBA polygon base.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @importFrom utils download.file unzip

spatial_join_arba <- function(data) {
  # Convert the input object to sf and filter out rows with missing latitude or longitude values
  data <- sf::st_as_sf(data %>% filter(!is.na(latitude) & !is.na(longitude)),
                       coords = c('longitude', 'latitude'), remove = FALSE, crs = 4326)

  # Create a temporary directory for storing the ARBA polygon base
  temp_dir <- tempdir()

  # URL of the ARBA polygon base ZIP file
  arba_zip_url <- "https://catalogo.datos.gba.gob.ar/dataset/627f65de-2510-4bf4-976b-16035828b5ae/resource/2cc73f96-98f7-42fa-a180-e56c755cf59a/download/limite_partidos.zip"

  # Download the ARBA polygon base ZIP file
  arba_zip_file <- file.path(temp_dir, "arba_polygon_base.zip")
  download.file(arba_zip_url, destfile = arba_zip_file)

  # Extract the ZIP file to a temporary directory
  temp_folder <- unzip(arba_zip_file, exdir = temp_dir)

  # Read the ARBA polygon base
  partidos_pba <- sf::st_read(temp_folder[4])

  # Prepare ARBA polygons for the spatial join
  sf_use_s2(FALSE)
  partidos_pba <- sf::st_make_valid(partidos_pba)  # Ensure polygon validity

  # Perform spatial join with ARBA polygons
  data <- sf::st_join(partidos_pba[c('nam', 'cca')], data, join = sf::st_intersects)

  # Remove geometry column and rename attributes
  data$geometry <- NULL
  data$nombre_arba <- data$nam
  data$nam <- NULL
  data$arba_code <- data$cca
  data$cca <- NULL

  # Clean up the temporary directory
  unlink(temp_dir, recursive = TRUE)

  return(data)
}
