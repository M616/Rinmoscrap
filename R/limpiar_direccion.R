#' Clean and standardize address data for geocoding
#'
#' Detects and standardizes addresses, handling street intersections and numeric formats.
#' Removes accents, punctuation, points, trims whitespace, converts to lowercase,
#' and removes everything after commas or semicolons.
#'
#' @param data A data frame containing address data.
#' @param address_col Name of the address column. Default is "address".
#' @param return_streets Logical. If TRUE, returns additional columns calle1 and calle2.
#' @return Original dataset plus a cleaned column direccion_limpia.
#' @import dplyr stringr stringi
#' @export
limpiar_direccion <- function(data, address_col = "address", return_streets = FALSE) {

   df <- data %>%
    mutate(
      direccion_limpia = sapply(.data[[address_col]], function(x) {
        if(is.na(x)) return(NA_character_)

        # 0️⃣ Quitar todos los puntos
        x <- str_replace_all(x, "\\.", "")
        x <- str_squish(x)

        # 1️⃣ Detectar número de dirección: "al 123" o "al 900"
        patron_num <- regex("\\b(al\\s*\\d{1,5})\\b", ignore_case = TRUE)
        match_num <- str_extract(x, patron_num)

        if(!is.na(match_num)) {
          # Tomar todo hasta el número de dirección
          loc <- str_locate(x, patron_num)[,2]
          x <- str_sub(x, 1, loc)
        } else {
          # Procesar intersecciones
          x <- x %>%
            str_replace_all(regex("\\bentre\\b", ignore_case = TRUE), "") %>%
            str_replace_all(regex("\\be/?\\b", ignore_case = TRUE), " y ") %>%
            str_squish()

          # Mantener solo las dos primeras calles
          if(!is.na(x) && str_detect(x, "\\sy\\s")) {
            parts <- unlist(strsplit(x, "\\sy\\s"))
            if(length(parts) >= 2) {
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
      }, USE.NAMES = FALSE)
    )

  # Si return_streets = FALSE, solo agrega direccion_limpia
  if(!return_streets) {
    return(df)
  }

  # Si return_streets = TRUE, separar calle1 y calle2
  df <- df %>%
    mutate(
      calle1 = sapply(direccion_limpia, function(x) {
        if(!is.na(x) && str_detect(x, "\\sy\\s")) str_split(x, "\\sy\\s")[[1]][1] else NA_character_
      }),
      calle2 = sapply(direccion_limpia, function(x) {
        if(!is.na(x) && str_detect(x, "\\sy\\s")) str_split(x, "\\sy\\s")[[1]][2] else NA_character_
      })
    )

  return(df)
}
