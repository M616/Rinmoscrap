#' export_gpkg
#'
#' @param x
#' @description Generates an 'inmo.gpkg' file in the 'output' directory. If the file exists it does not replace it, it must be manually deleted.
#' @return
#' @export
#'
#' @examples

export_gpkg <- function(x){

  p <- st_as_sf (
    x %>%
      filter(!is.na(latitude)&!is.na(longitude) ),
    coords=c('longitude','latitude'),
    remove=FALSE,
    crs=4326)

  dir.create('out')
st_write (p, paste0('out/','terrenos_inmo_2023_02','.gpkg'),overwrite = TRUE)

}
