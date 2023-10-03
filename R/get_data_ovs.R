#' Download and Read Soil Data from the Observatorio de Valores de Suelo (https://observatoriosuelo.gba.gob.ar/)
#'
#' This function downloads a .gpkg (Geopackage) file containing soil data from the Observatory of Soil Values
#' in the Province of Buenos Aires. It then converts the data into an 'sf' (Simple Features) object for further analysis.
#'
#' @return An 'sf' object containing soil data.
#'
#' @details The function relies on the 'googledrive' and 'sf' packages. If these packages are not installed, they need to be
#'   installed before using this function.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data <- get_data_ovs()
#' head(data)
#' }
#'
#' @seealso [googledrive](https://cran.r-project.org/package=googledrive)
#' @seealso [sf](https://cran.r-project.org/package=sf)
#'
#' @import googledrive
#' @import sf
#'
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
  # Create a temporary file with a .zip extension
  temp <- tempfile(fileext = ".zip")

  # Download the file from Google Drive using its ID
  # Replace "1Jql3hyztfuGKhXLzkWjYoCcXZ-fSaJ-S" with the correct ID of the file on Google Drive
  drive_download(as_id("1Jql3hyztfuGKhXLzkWjYoCcXZ-fSaJ-S"), path = temp, overwrite = TRUE)

  # Unzip the .zip file into a temporary directory
  data <- sf::st_read(unzip(temp, exdir = tempdir()))

  # Return the sf object
  return(data)
}
