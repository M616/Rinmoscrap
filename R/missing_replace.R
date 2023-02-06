#' missing_replace
#'
#' @param x
#' @description Missing replacement
#' @return
#' @export
#'
#' @examples
missing_replace <- function(x){
  x <- x %>% mutate(across(everything(), ~na_if(., ''))) %>%
    mutate(across(everything(), ~na_if(., 'NaN')))
  return(x)
}
