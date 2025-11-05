#' Clean and standardize address data for geocoding
#'
#' This function detects and standardizes potentially geocodable addresses,
#' especially street intersections and numeric formats. It identifies patterns
#' such as "Calle1 y Calle2", "Calle1 e Calle2", "entre Calle1 y Calle2",
#' and formats them for cleaner and more consistent geocoding.
#'
#' It also removes accents, unnecessary punctuation, trims whitespace, converts
#' to lowercase, standardizes common patterns, removes points, and trims everything
#' after commas or semicolons.
#'
#' @param data A data frame containing address data.
#' @param address_col A string with the name of the address column. Default is `"address"`.
#' @param return_streets Logical. If `TRUE`, returns additional columns `calle1` and `calle2`
#'   for detected street names. Default is `FALSE`.
#'
#' @return A data frame with at least:
#' \itemize{
#'   \item \code{direccion_limpia}: Cleaned and standardized address.
#' }
#'
#' @import dplyr stringr stringi
#' @export
limpiar_direccion <- function(data, address_col = "address", return_streets = FALSE) {


  df <- data %>%
    mutate(
      direccion_limpia = .data[[address_col]] %>%
        sapply(function(x) {
          if (is.na(x)) return(NA_character_)

          # 0️⃣ Quitar todos los puntos al inicio
          x <- str_replace_all(x, "\\.", "")
          x <- str_squish(x)

          # 1️⃣ Detectar número de dirección: "al 123" o "al 900"
          patron_num <- regex("\\b(al\\s*\\d{1,5})\\b", ignore_case = TRUE)
          match_num <- str_extract(x, patron_num)

          if (!is.na(match_num)) {
            # Tomar todo hasta el número de dirección
            loc <- str_locate(x, patron_num)[,2]  # posición final del match
            x <- str_sub(x, 1, loc)
          } else {
            # Si no hay número, procesar intersecciones
            x <- x %>%
              str_replace_all(regex("\\bentre\\b", ignore_case = TRUE), "") %>%
              str_replace_all(regex("\\be/?\\b", ignore_case = TRUE), " y ") %>%
              str_squish()

            # Mantener solo las dos primeras calles
            if (str_detect(x, "\\sy\\s")) {
              parts <- unlist(strsplit(x, "\\sy\\s"))
              if (length(parts) >= 2) {
                x <- paste(parts[1], "y", parts[2])
              }
            }
          }

          # 2️⃣ Quitar todo después de coma o punto y coma
          x <- str_replace(x, "[,;].*$", "")

          # 3️⃣ Limpieza final: acentos, puntuación, espacios, minúsculas
          x <- x %>%
            str_squish() %>%
            stringi::stri_trans_general("Latin-ASCII") %>%
            str_replace_all("[^A-Za-z0-9 ]", "") %>%
            str_to_lower()

          return(x)
        }) %>%
        unname()
    )

  if (!return_streets) {
    df <- df %>% select(all_of(address_col), direccion_limpia)
  }

  return(df)
}
