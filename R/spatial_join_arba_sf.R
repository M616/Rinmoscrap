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
#' @note Those points located on the border of the matches can be included in one or more matches causing the
#' records to be repeated. On the other hand, the arba base has splintered polygons,
#' that is, there are often spaces between the geometries. This causes that when
#' making the spatial union with the real estate base, the points that are located
#' in those spaces are discarded.
#'
#' @param data A spatial object representing points, lines, or polygons.
#'
#' @param include_crown Logical, indicating whether to include the 'corona' column. Default is FALSE.
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
#' data <- data.frame(property_group = c('Casa', 'Ph', 'Departamento', 'Casa', 'Ph', 'Departamento'),
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
#' data <- sf::st_as_sf(data, coords = c('longitude', 'latitude'), crs = 4326, remove = FALSE)
#' # Perform spatial join without 'corona' column
#' result <- spatial_join_arba_sf(data)
#' # Perform spatial join with 'corona' column
#' result_with_crown <- spatial_join_arba_sf(data, include_crown = TRUE)
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
#'
spatial_join_arba_sf <- function(data, include_crown = FALSE) {

  #Filter out rows with missing geometry
  data <-  data[!st_is_empty(data), ]

  # Create a temporary directory for storing the ARBA polygon base
  temp_dir <- file.path(tempdir(check = TRUE))
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
  partidos_pba <-  partidos_pba |> filter(cca!='070')
  partidos_pba$cca <- sprintf("%03d", as.integer(partidos_pba$cca))
  crown_mapping <- c(
    "136" = 1, "135" = 1, "133" = 2, "132" = 2, "131" = 2, "130" = 2, "129" = 3,
    "120" = 2, "118" = 3, "117" = 1, "115" = 3, "114" = 3, "110" = 1, "101" = 1,
    "100" = 3, "097" = 1, "096" = 1, "086" = 1, "084" = 3, "074" = 2, "072" = 2,
    "070" = 1, "068" = 3, "064" = 3, "063" = 1, "057" = 2, "055" = 3, "047" = 1,
    "046" = 3, "041" = 3, "038" = 3, "032" = 2, "031" = 3, "030" = 2, "025" = 1,
    "015" = 3, "014" = 3, "013" = 3, "004" = 1, "003" = 2
  )
  partidos_pba$corona <- crown_mapping[partidos_pba$cca]
  partidos_pba <- partidos_pba |> select(cca,
                                         cde,
                                         nam,
                                         corona)
  #partidos_pba <-  st_transform(partidos_pba, crs = 4326)
  #partidos_pba <-  sf::st_crs(partidos_pba,4326)

  #La Matanza polygons
  coronas_file <- paste0(system.file(package = "Rinmoscrap"), '/data/coronas_la_matanza.gpkg')
  coronas_gpkg <- sf::st_read(coronas_file)
  coronas_gpkg <- sf::st_make_valid(coronas_gpkg)
  coronas_gpkg <-  st_transform(coronas_gpkg, crs = 4326)
  coronas_gpkg <- coronas_gpkg |> select(cca,
                                         cde,
                                         nam,
                                         corona)
  partidos_pba <-  bind_rows(partidos_pba,coronas_gpkg)
  partidos_pba <- sf::st_make_valid(partidos_pba)
  # Perform spatial join with ARBA polygons
  data <- sf::st_join(data, partidos_pba[c('nam', 'cca','corona')], join = sf::st_intersects, left=TRUE )
  # Rename attributes

  data$nombre_arba <- data$nam
  data$arba_code <- data$cca
  data$nam <- NULL
  data$cca <- NULL
  corona <- data$corona
  data <-  data |>
    select(-corona)
  # Reset index to a consecutive sequence of integers
  rownames(data) <- unname(seq_len(nrow(data)))
  # Add 'corona' column based on 'arba_code' if include_crown is TRUE
  if (include_crown) {
    data$corona <- corona
  }

  return(data)
}
