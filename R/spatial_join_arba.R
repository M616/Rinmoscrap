#' Spatial join with ARBA polygon base and attribute recoding
#'
#' This function performs a spatial join between a spatial object and the ARBA (Buenos Aires Revenue Agency) polygon base. It also recodes the resulting attributes using the official nomenclature.
#'
#' @param x A spatial object representing points, lines, or polygons.
#'
#' @return A modified spatial object with additional attributes obtained from the spatial join with the ARBA polygon base.
#'
#' @export
#'
#' @import dplyr
#' @import sf
#' @importFrom utils download.file unzip

spatial_join_arba <- function(x) {

  # Convert the input object to sf and filter out rows with missing latitude or longitude values
  x <- st_as_sf(x %>% filter(!is.na(latitude) & !is.na(longitude)),
                coords = c('longitude', 'latitude'), remove = FALSE, crs = 4326)

  # Download the ARBA polygon base and extract it to a temporary directory
  temp_dir <- tempdir()
  temp_zip <- file.path(temp_dir, "limite_partidos.zip")
  temp_folder <- unzip(temp_zip, exdir = temp_dir)

  # Read the ARBA polygon base
  partidos_pba <- sf::st_read(temp_folder[4])

  # Prepare ARBA polygons for the spatial join
  sf_use_s2(FALSE)
  partidos_pba <- sf::st_make_valid(partidos_pba)  # Ensure polygon validity

  # Perform spatial join with ARBA polygons
  x <- sf::st_join(partidos_pba[c('nam', 'cca')], x, join = st_intersects)

  # Remove geometry column and rename attributes
  x$geometry <- NULL
  x$nombre_arba <- x$nam
  x$nam <- NULL
  x$arba_code <- x$cca
  x$cca <- NULL

  return(x)
}
