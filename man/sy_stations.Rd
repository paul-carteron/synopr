% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sy_stations.R
\name{sy_stations}
\alias{sy_stations}
\title{sy_stations}
\usage{
sy_stations()
}
\value{
sf object containninge all SYNOP stations
}
\description{
Stations are downloaded from meteo france website with url :
https://donneespubliques.meteofrance.fr/donnees_libres/Txt/Synop/postesSynop.json
}
\examples{
\dontrun{
plot(sy_station()$geometry)
}
}
