#' property_recode
#'
#' @param x
#' @description Property type recode for the listing anouncements
#' @return
#' @export
#'
#' @examples
property_recode <- function(x)  {
  x <- x %>% mutate(property_type_rec= tolower(property_type)) %>%
  mutate(property_groups=
           case_when(
             property_type_rec=='terreno y lote'~'terrenos',
             property_type_rec=='terreno'~'terrenos',
             property_type_rec=='terrenos'~'terrenos',

             property_type_rec=='casa'~'casa',
             property_type_rec=='quinta'~'casa',
             property_type_rec=='casa quinta'~'casa',
             property_type_rec=='quinta vacacional'~'casa',


             property_type_rec=='departamento'~'departamento',
             property_type_rec=='ph'~'ph',

             property_type_rec=='depósito y galpón'~'galpon y depositos',
             property_type_rec=='depósito'~'galpon y depositos',
             property_type_rec=='bodega-galpón'~'galpon y depositos',
             property_type_rec=='galpon'~'galpon y depositos',

             property_type_rec=='consultorio'~      'comercio y oficina',
             property_type_rec=='fondo de comercio'~'comercio y oficina',
             property_type_rec=='fondocomercio'~    'comercio y oficina',
             property_type_rec=='oficina comercial'~'comercio y oficina',
             property_type_rec=='local comercial'~  'comercio y oficina',
             property_type_rec=='oficina'~          'comercio y oficina',
             property_type_rec=='local'~            'comercio y oficina',

             is.na(property_type_rec) ~ 'sin clasificar',

             TRUE~'otros inmuebles')) %>%
    select(-property_type_rec)

  return(x)
}

