#' Download and prepare polygons for the province of Buenos Aires from the ARBA website.
#'
#' This function downloads polygon data for the province of Buenos Aires from the ARBA (Administración
#' de Ingresos Públicos de la Provincia de Buenos Aires) website. The data is processed and returned as
#' a spatial data frame suitable for spatial analysis and visualization.
#'
#' @param include_crown Logical indicating whether to include the "crown" information in the resulting data frame.
#'                      If TRUE, a "corona" column will be included to represent the administrative region.
#'                      If FALSE (default), the "corona" column will be omitted.
#'
#' @return A spatial data frame containing polygons for the province of Buenos Aires.
#'
#' @import sf
#'
#' @examples
#' arba_polygons <- get_arba_polygons()
#'
#' @export
get_arba_polygons <- function(include_crown = FALSE) {

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
  partidos_pba <- partidos_pba |> filter(cca != '070')
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

  # La Matanza polygons
  coronas_file <- paste0(system.file(package = "Rinmoscrap"), '/data/coronas_la_matanza.gpkg')
  coronas_gpkg <- sf::st_read(coronas_file)
  coronas_gpkg <- sf::st_make_valid(coronas_gpkg)
  coronas_gpkg <-  st_transform(coronas_gpkg, crs = 4326)
  coronas_gpkg <- coronas_gpkg |> select(cca,
                                         cde,
                                         nam,
                                         corona)
  partidos_pba <-  bind_rows(partidos_pba, coronas_gpkg)
  partidos_pba <- sf::st_make_valid(partidos_pba)

  partidos_pba$nombre_arba <- partidos_pba$nam
  partidos_pba$arba_code <- partidos_pba$cca
  partidos_pba$nam <- NULL
  partidos_pba$cca <- NULL
  corona <- partidos_pba$corona
  partidos_pba <-  partidos_pba |>
    select(-corona)

  # Reset index to a consecutive sequence of integers
  rownames(partidos_pba) <- unname(seq_len(nrow(partidos_pba)))

  # Add 'corona' column based on 'arba_code' if include_crown is TRUE
  if (include_crown) {
    partidos_pba$corona <- corona
  }

  return(partidos_pba)
}
