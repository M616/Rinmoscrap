#' create_sa
#'
#' @param x
#' @description Create a site abbreviation for the listing anouncements
#' @return
#' @export
#'
#' @examples
create_sa <- function(x){
    x <- x %>% mutate(
        portal=case_when(
        str_detect(url,'api.mercadolibre.com') ~'ML',
        str_detect(url,'www.zonaprop.com') ~'ZP',
        str_detect(url,'www.argenprop.com') ~'AP')
    ) %>%
      mutate(portal= as.factor(portal))
    return(x)


}

