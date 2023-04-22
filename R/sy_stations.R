#' sy_stations
#'
#' Stations are downloaded from meteo france website with url :
#' https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.json
#'
#' @return sf object containninge all SYNOP stations
#' @export
#'
#' @examples
#' \dontrun{
#' plot(sy_station()$geometry)
#' }
sy_stations <- function(){
   stations <- read_sf("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.json")
   return(stations)
}
