#' spatial_join_arba
#'
#' @param x
#' @description Spatial union with the ARBA polygon base and recoding with official nomenclature.
#' @return
#' @export
#'
#' @examples
spatial_join_arba <- function(x)  {

  x <- st_as_sf (x %>% filter(!is.na(latitude)&!is.na(longitude) ), coords=c('longitude','latitude'),remove=FALSE, crs=4326)
  #return(x)
  #se descarga los poligonos de los partidos de arba
  temp <- tempfile()
  utils::download.file ('https://catalogo.datos.gba.gob.ar/dataset/627f65de-2510-4bf4-976b-16035828b5ae/resource/2cc73f96-98f7-42fa-a180-e56c755cf59a/download/limite_partidos.zip',temp)
  out <- utils::unzip(temp)
  partidos_pba <- st_read(out[4])

  sf_use_s2(FALSE)
  partidos_pba <- sf::st_make_valid(partidos_pba)

  #se renombran los poligonos de islas
  #partidos_pba <- partidos_pba %>% mutate(nam=case_when(nam=='Islas Baradero'~'Baradero',
  #                                              nam=='Islas Campana'~'Campana',
  #                                              nam=='Islas de San Nicolas'~'San Nicolás',
  #                                              nam=='Islas de Zárate'~'Zárate',
  #                                              nam=='Islas Ramallo'~'Ramallo',
  #                                              nam=='Islas San Fernando'~'San Fernando',
  #                                              nam=='Islas San Pedro'~'San Pedro',
  #                                              nam=='Islas Tigre'~'Tigre',
  #                                              TRUE~nam) )

  partidos_pba <- partidos_pba %>% filter(!is.na(geometry))

  x <- sf::st_join(partidos_pba[c('nam','cca')],x,join=st_intersects)

  x$geometry <- NULL
  x$nombre_arba <- x$nam
  x$nam <- NULL
  x$arba_code <- x$cca
  x$cca <- NULL

  rm(partidos_pba,out,temp)

  return(x)

  }
