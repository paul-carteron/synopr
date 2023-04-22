as_date <- function(x){

   if (!inherits(x, "character")){
      stop("x must be of class character.")
   }

   # as.Date need day so if not provided, is added
   if (all(nchar(x) == 6)){
      x <- paste0(x, "01")
   }

   date <- as.Date(x, format = "%Y%m%d")
}

kel_to_deg <- function(x){
   if (!inherits(x, "numeric")){
      stop("x should be numeric.")
   }

   deg <- x - 273.15
}
