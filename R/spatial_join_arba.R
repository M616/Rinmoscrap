#' Spatial join with ARBA polygon base and attribute recoding
#'
#' This function performs a spatial join between a spatial object and the base of the ARBA
#' polygon. It also recodes the resulting attributes using the official nomenclature and,
#' optionally, adds a 'corona' column for the districts of the RMBA
#' - Buenos Aires Metropolitan Region - (Fernández Leonardo, 2011. Instituto del Conurbano.
#' Universidad Nacional de General Sarmiento).
#' For the district of La Matanza, the function takes into account the criteria
#' established by INDEC (INDEC, 2003. https://www.indec.gob.ar/dbindec/folleto_gba.pdf).
#'
#' @param data A spatial object representing points, lines, or polygons.
#'
#' @param include_crown Logical, indicating whether to include the 'corona' column.
#' Default is FALSE.
#'
#' @return A modified spatial object with additional attributes obtained from the spatial
#' join with the ARBA polygon base. If 'include_crown' is TRUE, it also includes a
#' 'corona' column.
#'
#' @export
#'
#' @import sf
#' @importFrom utils download.file unzip
#'
#' @examples
#' # Example usage
#' data <- data.frame(property_group = c('Casa', 'Ph', 'Departamento', 'Casa', 'Ph',
#' 'Departamento'),
#' land_surface = c(500, 200, 0, 700, 350, 0),
#' reconstructed_land_surface = c(0, 0, 150, 0, 0, 200),
#' total_surface = c(700, 300, 800, 600, 400, 1000),
#' covered_surface = c(600, 250, 750, 550, 350, 900),
#' bed_amnt = c(5, 3, 2, 6, 4, 2),
#' bath_amnt = c(3, 2, 1, 4, 2, 1),
#' is_new_property = c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE),
#' year_built = c(1990, 2020, 1980, 2005, 2015, 1995),
#' garage_amnt = c(2, 0, 1, 3, 0, 2),
#' room_amnt = c(8, 4, 3, 7, 5, 4),
#' is_finished_property = c(TRUE, FALSE, TRUE, TRUE, TRUE, FALSE),
#' latitude = c(-34.6118, -34.6518, -34.9226, -34.5831, -34.6058, -36.7597),
#' longitude = c(-58.4173, -58.3595, -57.9500, -58.4009, -58.4403, -59.8597)
#' )
#' # Perform spatial join without 'corona' column
#' result <- spatial_join_arba(data)
#' # Perform spatial join with 'corona' column
#' result_with_crown <- spatial_join_arba(data, include_crown = TRUE)
#'
#' @references
#' More information about ARBA: https://www.arba.gov.ar/
#'
#' Fernández Leonardo (2011).Censo 2010. Somos 14.819.137 habitantes en
#' la Región Metropolitana de Buenos Aires. Instituto del Conurbano.
#' Universidad Nacional de General Sarmiento. www.urbared.ungs.edu.ar
#'
#' https://www.indec.gob.ar/dbindec/folleto_gba.pdf'
#'

spatial_join_arba <- function(data, include_crown = FALSE) {
  # Convert the input object to sf and filter out rows with missing latitude or longitude values
  data <- sf::st_as_sf(data[!is.na(data$latitude) & !is.na(data$longitude), ], coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE)

  # Create a temporary directory for storing the ARBA polygon base
  temp_dir <- tempdir()

  # URL of the ARBA polygon base ZIP file
  arba_zip_url <- "https://catalogo.datos.gba.gob.ar/dataset/627f65de-2510-4bf4-976b-16035828b5ae/resource/2cc73f96-98f7-42fa-a180-e56c755cf59a/download/limite_partidos.zip"

  # Number of maximum download attempts
  max_attempts <- 3

  for (attempt in 1:max_attempts) {
    tryCatch({
      # Download the ARBA polygon base ZIP file
      arba_zip_file <- file.path(temp_dir, "arba_polygon_base.zip")
      download.file(arba_zip_url, destfile = arba_zip_file)

      # If the download is successful, exit the loop
      break
    }, error = function(e) {
      cat("Error downloading ARBA polygon base ZIP file (Attempt", attempt, "of", max_attempts, "):", conditionMessage(e), "\n")

      # Wait for a short time before retrying (you can adjust the time as needed)
      Sys.sleep(5)
    })
  }

  # Check if the file was downloaded successfully
  if (!file.exists(arba_zip_file)) {
    cat("Failed to download the file after", max_attempts, "attempts. Please check your connection and try again later.\n")
    return(NULL)
  }

  # Extract the ZIP file to a temporary directory
  temp_folder <- unzip(arba_zip_file, exdir = temp_dir)

  # Read the ARBA polygon base
  partidos_pba <- sf::st_read(temp_folder[4])

  # Prepare ARBA polygons for the spatial join
  sf_use_s2(FALSE)
  partidos_pba <- sf::st_make_valid(partidos_pba)
  partidos_pba <- partidos_pba |> filter(cca != '070')

  # ...

  # The rest of your function implementation remains unchanged

  # Clean up the temporary directory
  unlink(temp_dir, recursive = TRUE)
  unlink(tempdir())

  return(data)
}
