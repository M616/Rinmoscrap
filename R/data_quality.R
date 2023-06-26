#' Add data quality category based on property type and quality criteria
#'
#' This function adds a data quality category to a dataset based on property type and specific quality criteria for different property groups, including houses, PHs, and apartments.
#'
#' @param data The dataset to which the data quality category will be added.
#'
#' @return A dataset with an additional column "data_quality" indicating the quality category of each property.
#'
#' @details The function applies the following criteria to determine the quality category of each property and adds a new column called "data_quality" to the dataset:
#'
#' - Category A represents properties of the highest quality.
#' - Category B represents properties of intermediate quality.
#' - Category C represents properties of relatively lower quality.
#'
#' For each property group (houses, PHs, and apartments), the function checks various attributes such as land surface, reconstructed land surface, total surface, covered surface, number of bedrooms, number of bathrooms, property age, garage amount, and room amount to determine the quality category.
#'
#' Specifically, the function assigns the following quality categories based on the filtering criteria:
#'
#' - Houses:
#'   - Category A: Houses that meet the following criteria:
#'     - Land surface or reconstructed land surface is greater than 0 and not equal to reconstructed land surface.
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 7 (inclusive).
#'     - Number of bathrooms is between 1 and 7 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'     - Number of garage spaces is between 0 and 6 (inclusive).
#'     - Number of rooms is between 1 and 10 (inclusive).
#'   - Category B: Houses that meet the following criteria:
#'     - Land surface or reconstructed land surface is greater than 0 and not equal to reconstructed land surface.
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 7 (inclusive).
#'     - Number of bathrooms is between 1 and 7 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'   - Category C: Houses that meet the following criteria:
#'     - Land surface or reconstructed land surface is greater than 0 and not equal to reconstructed land surface.
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 7 (inclusive).
#'     - Number of bathrooms is between 1 and 7 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'
#' - PHs (Propiedad Horizontal):
#'   - Category A: PHs that meet the following criteria:
#'     - Land surface or reconstructed land surface is greater than 0.
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 6 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'     - Number of garage spaces is between 0 and 3 (inclusive).
#'     - Number of rooms is between 1 and 7 (inclusive).
#'   - Category AB: PHs that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 6 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'     - Number of garage spaces is between 0 and 3 (inclusive).
#'     - Number of rooms is between 1 and 7 (inclusive).
#'   - Category B: PHs that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Total surface is not equal to covered surface.
#'     - Number of bedrooms is between 1 and 6 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'   - Category C: PHs that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Number of bedrooms is between 1 and 6 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'
#' - Apartments (Departamentos):
#'   - Category A: Apartments that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Number of bedrooms is between 1 and 4 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property is not marked as "is_finished_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'     - Number of garage spaces is between 0 and 2 (inclusive).
#'     - Number of rooms is between 1 and 5 (inclusive).
#'   - Category B: Apartments that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Number of bedrooms is between 1 and 4 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property is not marked as "is_finished_property" missing.
#'     - The property's year built is between 1900 and the current year.
#'     - Number of rooms is between 1 and 5 (inclusive).
#'   - Category C: Apartments that meet the following criteria:
#'     - Total surface and covered surface are greater than 0.
#'     - Number of bedrooms is between 1 and 4 (inclusive).
#'     - Number of bathrooms is between 1 and 4 (inclusive).
#'     - The property is not marked as "is_new_property" missing.
#'     - The property is not marked as "is_finished_property" missing.
#'
#' Note: The function assumes that the input dataset has columns named "property_group", "land_surface", "reconstructed_land_surface", "total_surface", "covered_surface", "number_bedrooms", "number_bathrooms", "is_new_property", "year_built", "garage_amount", "number_rooms", and "is_finished_property" that are used for applying the quality criteria.
#'
#'
#'
#'
#'
#'
#'
#' ##########warning!!!! ojo!!!! chequear que una vez obtenidas las categorias de esta variable
#' ver que a la hora de nalizar y operar copn datos considere las columnas correctas( complementariedad de las variables de superficie). es decir, ver que con que variables operar, por ejemplo de superficie (si es reconstructed o sin reconstructed)
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
    mutate(condition = if_else(land_surface > 0 & reconstructed_land_surface > 0 & land_surface != reconstructed_land_surface, TRUE,FALSE,missing=FALSE)) |>
    mutate(data_quality =
             case_when(
               # Houses - Category A
               property_group == 'Casa' &

                 (land_surface >0  | reconstructed_land_surface > 0) &
                 !condition &
                 (total_surface>0 & covered_surface>0 |
                    reconstructed_total_surface > 0 & covered_surface>0 ) &
                 (total_surface!=covered_surface | reconstructed_total_surface != covered_surface) &

                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 7) &
                 (room_amnt > 0 & room_amnt < 11) ~ 'A',
               # Houses - Category B
               property_group == 'Casa' &

                 (land_surface >0  | reconstructed_land_surface > 0) &
                 !condition &
                 (total_surface>0 & covered_surface>0 |
                    reconstructed_total_surface > 0 & covered_surface>0 ) &
                 (total_surface!=covered_surface | reconstructed_total_surface != covered_surface) &


                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) ~ 'B',
               # Houses - Category C
               property_group == 'Casa' &

                 (land_surface >0  | reconstructed_land_surface > 0) &
                 !condition &
                 (total_surface>0 & covered_surface>0 |
                    reconstructed_total_surface > 0 & covered_surface>0 ) &
                 (total_surface!=covered_surface | reconstructed_total_surface != covered_surface) &

                 (bed_amnt > 0 & bed_amnt < 8) &
                 (bath_amnt > 0 & bath_amnt < 8) &
                 !is.na(is_new_property) ~ 'C',

               # PHs - Category A

               property_group=='Ph' &
               (land_surface >0  | reconstructed_land_surface > 0)&
               (total_surface>0 & covered_surface>0 |
                reconstructed_total_surface > 0 & covered_surface>0 ) &
               (total_surface!=covered_surface |
                reconstructed_total_surface != covered_surface) &
                 (bed_amnt > 0 & bed_amnt < 7) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 4) &
                 (room_amnt > 0 & room_amnt < 8) ~ 'A',
               #PHs - Category AB
               property_group=='Ph' &
                 (total_surface>0 & covered_surface>0 |
                  reconstructed_total_surface > 0 & covered_surface>0 ) &
                 (total_surface!=covered_surface |
                  reconstructed_total_surface != covered_surface) &
                 (bed_amnt>0 & bed_amnt<7 )&
                 (bath_amnt>0 & bath_amnt<5 )&
                 !is.na(is_new_property)&
                 (year_built>1900 & year_built<2024) &
                 (garage_amnt>=0 & garage_amnt < 4) &
                 (room_amnt > 0 & room_amnt < 8)~'AB',
               # PHs - Category B
               property_group=='Ph' &
                 (total_surface>0 & covered_surface>0 |
                  reconstructed_total_surface > 0 & covered_surface>0 ) &
                 (total_surface!=covered_surface |
                  reconstructed_total_surface != covered_surface) &
                 (bed_amnt > 0 & bed_amnt < 7) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 (year_built > 1900 & year_built < 2024) ~ 'B',
               # PHs - Category C
               property_group=='Ph' &
                (total_surface>0 & covered_surface>0 |
                 reconstructed_total_surface > 0 & covered_surface>0 )&
                (bed_amnt > 0 & bed_amnt < 7) &
                (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) ~ 'C',
               # Apartments - Category A
               property_group == 'Departamento' &
                 ((total_surface >0 & covered_surface > 0 ) |
                  (reconstructed_total_surface > 0 & covered_surface > 0) ) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (garage_amnt >= 0 & garage_amnt < 3) &
                 (room_amnt > 0 & room_amnt < 6) ~ 'A',
               # Apartments - Category B
               property_group == 'Departamento' &
                 ((total_surface >0 & covered_surface > 0 ) |
                  (reconstructed_total_surface > 0 & covered_surface > 0) ) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) &
                 (year_built > 1900 & year_built < 2024) &
                 (room_amnt > 0 & room_amnt < 6) ~ 'B',
               # Apartments - Category C
               property_group == 'Departamento' &
                 ((total_surface >0 & covered_surface > 0 ) |
                  (reconstructed_total_surface > 0 & covered_surface > 0) ) &
                 (bed_amnt > 0 & bed_amnt < 5) &
                 (bath_amnt > 0 & bath_amnt < 5) &
                 !is.na(is_new_property) &
                 !is.na(is_finished_property) ~ 'C',
               TRUE ~ NA_character_
             )
    ) |>
    select(-condition)

  return(data)
}
