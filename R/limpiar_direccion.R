#' Clean and standardize address data for geocoding
#'
#' This function detects and standardizes potentially geocodable addresses,
#' especially street intersections and numeric formats. It identifies patterns
#' such as "Calle1 y Calle2", "Calle1 e Calle2", or "entre Calle1 y Calle2",
#' and formats them for cleaner and more consistent geocoding.
#'
#' The function also normalizes text: it removes accents, unnecessary punctuation,
#' trims whitespace, converts to lowercase, and standardizes common patterns.
#'
#' @param data A data frame containing address data.
#' @param address_col A string with the name of the address column. Default is `"address"`.
#' @param return_streets Logical. If `TRUE`, returns additional columns `calle1` and `calle2`
#'   for detected street names. Default is `FALSE`.
#'
#' @return A data frame with at least:
#' \itemize{
#'   \item \code{geocodable}: Logical flag indicating if the address matched any geocoding pattern.
#'   \item \code{direccion_limpia}: Cleaned and standardized address.
#'   \item If \code{return_streets = TRUE}, also includes:
#'     \itemize{
#'       \item \code{calle1}: First street detected from intersection patterns.
#'       \item \code{calle2}: Second street detected from intersection patterns.
#'     }
#' }
#'
#' @examples
#' library(dplyr)
#'
#' # Example addresses
#' direcciones <- tibble::tibble(
#'   address = c(
#'     "Avellaneda y Rivadavia",
#'     "Mitre e/ Belgrano y San Martín",
#'     "Ruta 2, Km 398",
#'     "Calle 45 al 1234, entre 10 y 11",
#'     "Los Talas e Los Ceibos y San Francisco"
#'   )
#' )
#'
#' # Basic usage
#' limpiar_direccion(direcciones)
#'
#' # With return_streets = TRUE
#' limpiar_direccion(direcciones, return_streets = TRUE)
#'
#' @import dplyr stringr stringi
#' @export
limpiar_direccion <- function(data, address_col = "address", return_streets = FALSE) {

  # 1. Pattern to detect potentially geocodable addresses
  patron <- regex(
    paste0(
      "\\b(?:\\d+\\s*bis?|[\\w]+)\\s+e/\\s+(?:\\d+\\s*bis?|[\\w]+)\\s+y\\s+(?:\\d+\\s*bis?|[\\w]+)|",
      "\\b(?:\\d+\\s*bis?|[\\w]+)\\s+y\\s+(?:\\d+\\s*bis?|[\\w]+)|",
      "\\b(?:\\d+\\s*bis?|[\\w]+)\\s+(al\\s+)?\\d{1,5}\\b"
    ),
    ignore_case = TRUE
  )

  # 2. Patterns for intersection detection
  pattern_interseccion_e_y <- regex(
    "\\b([a-z]+(?:\\s+[a-z]+)*)\\s+(?:e/|e)\\s+([a-z]+(?:\\s+[a-z]+)*)\\b",
    ignore_case = TRUE
  )

  pattern_interseccion_entre <- regex(
    "\\bentre\\s+([a-z]+(?:\\s+[a-z]+)*)\\s+y\\s+([a-z]+(?:\\s+[a-z]+)*)\\b",
    ignore_case = TRUE
  )

  # 3. Process and extract components
  df <- data %>%
    mutate(
      geocodable = str_detect(.data[[address_col]], patron),

      # Extract street names from different patterns
      interseccion_e_1 = str_match(.data[[address_col]], pattern_interseccion_e_y)[, 2],
      interseccion_e_2 = str_match(.data[[address_col]], pattern_interseccion_e_y)[, 3],
      interseccion_entre_1 = str_match(.data[[address_col]], pattern_interseccion_entre)[, 2],
      interseccion_entre_2 = str_match(.data[[address_col]], pattern_interseccion_entre)[, 3],

      calle1 = coalesce(interseccion_e_1, interseccion_entre_1),
      calle2 = coalesce(interseccion_e_2, interseccion_entre_2),

      # Build the cleaned address
      direccion_limpia = case_when(
        geocodable & !is.na(calle1) & !is.na(calle2) ~ paste(calle1, "y", calle2),
        geocodable ~ .data[[address_col]],
        TRUE ~ NA_character_
      )
    ) %>%

    # 4. Preserve "Ruta X, Km Y", remove everything after comma in other cases
    mutate(
      direccion_limpia = case_when(
        str_detect(direccion_limpia, regex("ruta\\s*\\d+,\\s*km", ignore_case = TRUE)) ~ direccion_limpia,
        TRUE ~ str_remove(direccion_limpia, ",.*")
      )
    ) %>%

    # 5. Remove extra "y Calle3" if multiple 'y' in the address
    mutate(
      direccion_limpia = direccion_limpia %>%
        str_replace(
          regex("^((?:[a-z0-9 ]+?)\\s+y\\s+(?:[a-z0-9 ]+?))(\\s+y\\s+.+)+$", ignore_case = TRUE),
          "\\1"
        ) %>%
        str_replace(regex("\\s+y\\s*$", ignore_case = TRUE), "")
    ) %>%

    # 6. Clean and normalize text
    mutate(
      direccion_limpia = direccion_limpia %>%
        str_squish() %>%                                # Remove extra spaces
        stringi::stri_trans_general("Latin-ASCII") %>%  # Remove accents
        str_replace_all("[^A-Za-z0-9 ]", "") %>%        # Remove punctuation/symbols
        str_to_lower()                                  # Convert to lowercase
    )

  # 7. Remove intermediate columns unless return_streets is TRUE
  if (!return_streets) {
    df <- df %>% select(-calle1, -calle2, -starts_with("interseccion_"))
  }

  return(df)
}
