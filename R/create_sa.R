#' create_sa
#'
#' This function creates a site abbreviation for the listing announcements based on the URL of the listings.
#' It assigns a specific abbreviation to each URL based on certain patterns.
#'
#' @param data A data frame containing the listings with URLs.
#' @description The function takes a data frame as input and adds a new column called "portal" to the data frame,
#'              which represents the site abbreviation for each listing.
#'              The site abbreviation is determined based on the URL of the listing.
#'              The function uses regular expressions to match specific patterns in the URL and assign the appropriate abbreviation.
#'              The abbreviations used are 'ML' for api.mercadolibre.com, 'ZP' for www.zonaprop.com, and 'AP' for www.argenprop.com.
#'              The function converts the "portal" column to a factor for categorical representation.
#' @return The modified data frame with the new "portal" column representing the site abbreviation for each listing.
#' @export
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
#' #                                       url portal
#' # 1 https://api.mercadolibre.com/listing1     ML
#' # 2    https://www.zonaprop.com/listing2     ZP
#' # 3   https://www.argenprop.com/listing3     AP
create_sa <- function(data){
  data <- data %>%
    mutate(
      site_abbreviation = case_when(
        str_detect(url, 'api.mercadolibre.com') ~ 'ML',
        str_detect(url, 'www.zonaprop.com') ~ 'ZP',
        str_detect(url, 'www.argenprop.com') ~ 'AP'
      )
    ) %>%
    mutate(site_abbreviation = as.factor(site_abbreviation))

  return(data)
}


