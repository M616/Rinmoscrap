#' Download and Read Soil Data from the Observatorio de Valores de Suelo (https://observatoriosuelo.gba.gob.ar/)
#'
#' This function downloads a .gpkg (Geopackage) file containing soil data from the Observatory of Soil Values
#' in the Province of Buenos Aires. It then converts the data into an 'sf' (Simple Features) object for further analysis.
#'
#' @return An 'sf' object containing soil data.
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_data_ovs()
#' head(data)
#' }
#'
#'
#' @import sf
#'
#' @param None
#'
#' @return An 'sf' object containing soil data.
#'
#' @examples
#' \dontrun{
#' data <- get_data_ovs()
#' head(data)
#' }
#'
#' @export
#'

get_data_ovs <- function() {
  # Specify the direct download link of the file
  direct_download_link <- "https://drive.google.com/uc?id=1Jql3hyztfuGKhXLzkWjYoCcXZ-fSaJ-S"

  temp <- tempfile(fileext = ".zip")

  # Download the file directly
  download.file(url = direct_download_link, destfile = temp, mode = "wb")

  # Read the downloaded .gpkg file into an 'sf' object
  data <- sf::st_read(unzip(temp, exdir = tempdir()))

  # Return the sf object
  return(data)
}
