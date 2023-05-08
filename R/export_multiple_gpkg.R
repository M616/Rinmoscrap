#' export_multiple_gpkg
#'
#' @param x
#' @description Segments the inmoscrap database and generates multiple .gpkg output files according to the ARBA code obtained with the 'spatial_join_ARBA' function
#' @return
#' @export
#'
#' @examples

export_multiple_gpkg <- function(x){

  p <- st_as_sf (
    x %>%
      filter(!is.na(latitude)&!is.na(longitude) ),
    coords=c('longitude','latitude'),
    remove=FALSE,
    crs=4326)

  dir.create('out')
  p %>%
    split(. $codigo_arba) %>%
    walk2(names(.),
          ~ st_write (.x, paste0('out/','terrenos_',.y,'_2023_04', '.gpkg')))

}
