#' property_recode
#'
#' This function performs property type recoding for the listing announcements. It is intended to be implemented in those databases where the 'property_group' variable does not come.
#'
#' @param data A data frame or tibble.
#' @description The function takes a data frame or tibble as input and recodes the property types based on specific criteria.
#' @return A modified data frame or tibble with the property types recoded into broader categories.
#' @export
#'
#' @examples
#' # Create a data frame with property types
#' df <- data.frame(property_type = c('casa', 'departamento', 'galpon', 'oficina', 'terrenos', 'other'), stringsAsFactors = FALSE)
#' df
#'
#' # Output:
#' #   property_type
#' # 1          casa
#' # 2  departamento
#' # 3        galpon
#' # 4       oficina
#' # 5      terrenos
#' # 6         other
#'
#' # Apply the property_recode function
#' modified_df <- property_recode(df)
#' modified_df
#'
#' # Output:
#' #   property_groups
#' # 1            casa
#' # 2    departamento
#' # 3 galpon y depositos
#' # 4 comercio y oficina
#' # 5        terrenos
#' # 6  otros inmuebles
property_recode <- function(data) {
  data <- data %>% mutate(property_type_rec = tolower(property_type)) %>%
    mutate(property_groups =
             case_when(
               property_type_rec %in% c('terreno y lote', 'terreno', 'terrenos') ~ 'terrenos',
               property_type_rec %in% c('casa', 'quinta', 'casa quinta', 'quinta vacacional') ~ 'casa',
               property_type_rec == 'departamento' ~ 'departamento',
               property_type_rec == 'ph' ~ 'ph',
               property_type_rec %in% c('depósito y galpón', 'depósito', 'bodega-galpón', 'galpon') ~ 'galpon y depositos',
               property_type_rec %in% c('consultorio', 'fondo de comercio', 'fondocomercio', 'oficina comercial', 'local comercial', 'oficina', 'local') ~ 'comercio y oficina',
               is.na(property_type_rec) ~ 'sin clasificar',
               TRUE ~ 'otros inmuebles'
             )) %>%
    select(-property_type_rec)

  return(data)
}
