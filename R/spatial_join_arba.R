#' Spatial join with ARBA polygon base and attribute recoding
#'
#' This function performs a spatial join between a spatial object and the ARBA polygon base. It also recodes the resulting attributes using the official nomenclature and optionally adds a 'corona' column based on 'arba_code'.
#'
#' @param data A spatial object representing points, lines, or polygons.
#'
#' @param include_crown Logical, indicating whether to include the 'corona' column based on 'arba_code'. Default is FALSE.
#'
#' @return A modified spatial object with additional attributes obtained from the spatial join with the ARBA polygon base. If 'include_crown' is TRUE, it also includes a 'corona' column.
#'
#' @export
#'
#' @import sf
#' @importFrom utils download.file unzip
#'
#' @examples
#' # Example usage
#' data <- read_sf("path/to/your/data.shp")
#' # Perform spatial join without 'corona' column
#' result <- spatial_join_arba(data)
#' # Perform spatial join with 'corona' column
#' result_with_crown <- spatial_join_arba(data, include_crown = TRUE)
#'
#' @references
#' More information about ARBA: https://www.arba.gov.ar/
#'
#' @seealso
#' `crown_mapping` - A vector mapping 'arba_code' to 'corona' values.

spatial_join_arba <- function(data, include_crown = FALSE) {
  # Convert the input object to sf and filter out rows with missing latitude or longitude values
  data <- sf::st_as_sf(data[!is.na(data$latitude) & !is.na(data$longitude), ], coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE)

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
  data <- sf::st_drop_geometry(data)
  data$nombre_arba <- data$nam
  data$arba_code <- data$cca
  data$nam <- NULL
  data$cca <- NULL

  # Reset index to a consecutive sequence of integers
  rownames(data) <- unname(seq_len(nrow(data)))

  # Add 'corona' column based on 'arba_code' if include_crown is TRUE
  if (include_crown) {
    # Formatting 'arba_code' to three characters with leading zeros
    data$arba_code <- sprintf("%03d", as.integer(data$arba_code))
    crown_mapping <- c(
      "136" = 1, "135" = 1, "133" = 2, "132" = 2, "131" = 2, "130" = 2, "129" = 3,
      "120" = 2, "118" = 3, "117" = 1, "115" = 3, "114" = 3, "110" = 1, "101" = 1,
      "100" = 3, "097" = 1, "096" = 1, "086" = 1, "084" = 3, "074" = 2, "072" = 2,
      "070" = 1, "068" = 3, "064" = 3, "063" = 1, "057" = 2, "055" = 3, "047" = 1,
      "046" = 3, "041" = 3, "038" = 3, "032" = 2, "031" = 3, "030" = 2, "025" = 1,
      "015" = 3, "014" = 3, "013" = 3, "004" = 1, "003" = 2
    )
    data$corona <- crown_mapping[data$arba_code]
  }

  # Clean up the temporary directory
  unlink(temp_dir, recursive = TRUE)
  unlink(tempdir())

  return(data)
}
