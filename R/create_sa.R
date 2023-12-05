#' Create site abbreviations based on URLs
#'
#' This function assigns site abbreviations to listing announcements based on the URL of the listings.
#' It determines the abbreviation for each URL using specific patterns found in the URL.
#'
#' @param data A data frame containing the listings with URLs in a column named "url".
#' @return The modified data frame with a new column "site_abbreviation" representing the site abbreviation for each listing.
#' @export
#' @import stringr
#'
#' @examples
#' # Create a data frame with listings
#' listings <- data.frame(url = c("https://api.mercadolibre.com/listing1",
#'                                "https://www.zonaprop.com/listing2",
#'                                "https://www.argenprop.com/listing3"))
#'
#' # Apply the create_sa function to add the site abbreviation
#' modified_listings <- create_sa(listings)
#' modified_listings
#'
#' # Output:
#' #                                       url site_abbreviation
#' # 1 https://api.mercadolibre.com/listing1                ML
#' # 2    https://www.zonaprop.com/listing2                ZP
#' # 3   https://www.argenprop.com/listing3                AP
create_sa <- function(data){
  data <- data  |>
    mutate(
      site_abbreviation = case_when(
        str_detect(url, 'api.mercadolibre.com') ~ 'ML',
        str_detect(url, 'www.zonaprop.com') ~ 'ZP',
        str_detect(url, 'www.argenprop.com') ~ 'AP',
        TRUE ~ NA_character_
      ),
      site_abbreviation = as.factor(site_abbreviation)
    )

  return(data)
}
