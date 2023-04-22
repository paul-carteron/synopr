#' sy_nearest_station
#'
#' @param x object of class sfg, sfc or sf
#'
#' @importFrom sf st_transform st_crs read_sf st_nearest_feature
#'
#' @return sf object
#' @export
#'
#' @examples
#' \dontrun{
#' paris <- st_sfc(st_point(c(2.335, 48.876)), crs = 4326)
#' nearest_station <- sy_nearest_station(paris)
#' }
#'
sy_nearest_station <- function(x){

   stations <- sy_stations()
   x <- st_transform(x, st_crs(stations))

   nearest_station <- stations[st_nearest_feature(x, stations),]

   return(nearest_station)
}
