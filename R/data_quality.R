#' Filter data based on property type and quality criteria
#' 
#' This function filters a dataset based on property type and specific quality criteria for different property groups, including houses, PHs, and apartments.
#' 
#' @param data The dataset to be filtered.
#' 
#' @return A filtered dataset with an additional column "data_quality" indicating the quality category of each property.
#' 
#' @details The function applies the following filtering criteria to determine the quality category of each property:
#'
#' [Documentation goes here...]
#'
#' @examples
#' data <- read.csv("property_data.csv")
#' filtered_data <- data_quality(data)
#' 
#' @export
#' 
#' @import dplyr
#' 
data_quality <- function(data) {
  data <- data |>
    mutate(data_quality =
             case_when(
               # Houses - Category A
               property_group == 'Casa' &
                 (land_surface > 0 | reconstructed_land_surface > 0 |
                    (total_surface > 0 & covered_surface > 0 |
                       reconstructed_total_surface > 0 & covered_surface > 0) &
                    (total_surface != covered_surface | reconstructed_total_surface != covered_surface)) &
                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 7) &
                 (room_amnt > 0 & room_amnt < 11) ~ 'A',
               # Houses - Category B
               property_group == 'Casa' &
                 (land_surface > 0 | reconstructed_land_surface > 0 |
                    (total_surface > 0 & covered_surface > 0 |
                       reconstructed_total_surface > 0 & covered_surface > 0) &
                    (total_surface != covered_surface | reconstructed_total_surface != covered_surface)) &
                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) ~ 'B',
               # Houses - Category C
               property_group == 'Casa' &
                 (land_surface > 0 | reconstructed_land_surface > 0 |
                    (total_surface > 0 & covered_surface > 0 |
                       reconstructed_total_surface > 0 & covered_surface > 0) &
                    (total_surface != covered_surface | reconstructed_total_surface != covered_surface)) &
                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) ~ 'C',
               # PHs - Category A
               property_group == 'Ph' &
                 (land_surface > 0 | reconstructed_land_surface > 0 |
                    (total_surface > 0 & covered_surface > 0) &
                    total_surface != covered_surface) &
                 (bed_amnt > 0 & bed_amnt < 7) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 4) &
                 (room_amnt > 0 & room_amnt < 8) ~ 'A',
               # PHs - Category B
               property_group == 'Ph' &
                 (total_surface > 0 & covered_surface > 0) &
                 total_surface != covered_surface &
                 (bed_amnt > 0 & bed_amnt < 7) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) ~ 'B',
               # PHs - Category C
               property_group == 'Ph' &
                 (total_surface > 0 & covered_surface > 0) &
                 (bed_amnt > 0 & bed_amnt < 7) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) ~ 'C',
               # Apartments - Category A
               property_group == 'Departamento' &
                 ((total_surface > 0 & covered_surface > 0) |
                    (reconstructed_total_surface > 0 & covered_surface > 0)) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 3) &
                 (room_amnt > 0 & room_amnt < 6) ~ 'A',
               # Apartments - Category B
               property_group == 'Departamento' &
                 (total_surface > 0 | covered_surface > 0) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (room_amnt > 0 & room_amnt < 6) ~ 'B',
               # Apartments - Category C
               property_group == 'Departamento' &
                 (total_surface > 0 | covered_surface > 0) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) ~ 'C',
               TRUE ~ NA_character_
             )
    )
  
  return(data)
}