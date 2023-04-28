#' missing_replace
#'
#' @param x
#' @description Missing replacement
#' @return an objet without '' or 'Nan'
#' @export
#'
#' @examples
missing_replace <- function(x){
  x <- x %>%  mutate(across(where(is.character),
                            ~ na_if(.,'')) )|>
    mutate(across(where(is.character),
                  ~na_if(.,'NaN')))

  return(x)
}
