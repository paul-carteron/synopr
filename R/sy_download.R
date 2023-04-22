#' sy_download
#'
#' @param date character; vector of dates format as "YYYYMM"
#' @param between boolean; if TRUE all month between first and last date are downloaded.
#' In this case, `date` must be set with only two date.
#' @param summarized boolean; if `TRUE` datasets already summarized per month by meteofrance are returned.
#'
#' @details
#' SYNOP every 3h data are available from 1996. When `summarized` is TRUE
#' summarized dataset from 1990 are available.
#'
#' @importFrom cli cli_progress_bar cli_progress_update
#'
#' @return data.frame
#' @export
#'
#' @examples
#' \dontrun{
#' dates <- c("202201", "202301")
#' two_month <- sy_download(dates)
#' all_year <- sy_download(dates, between = T)
#'
#' }
#'
sy_download <- function(date, between = F, summarized = F) {

   date_before_1990 <- as_date(date) < as.Date("1990-01-01")
   if (any(date_before_1990) & isTRUE(summarized)){
      stop("Wrong date(s) : ",
           paste0(date[date_before_1990], collapse = ", "),
           "\nSummarized by month data are available only from 1990.", call. = F)
   }

   date_before_1996 <- as_date(date) < as.Date("1996-01-01")
   if (any(date_before_1996) & isFALSE(summarized)){
      stop("Wrong date(s) : ",
           paste0(date[date_before_1996], collapse = ", "),
           "\nSYNOP every 3h data are available only from 1996.", call. = F)
   }

   if (between){
      more_than_two_date <- length(date) != 2
      if (more_than_two_date){
         stop("if `between` is TRUE, two dates only must be set", call. = F)
      }

      date <- seq(as_date(date[1]),
                  as_date(date[2]),
                  by = "month") |>
         format("%Y%m")
   }

   cli_progress_bar(format = paste0("{pb_spin} Downloading {i} ",
                                    "[{pb_current}/{pb_total}]   ETA:{pb_eta}"),
                    format_done = paste0(
                       "{col_green(symbol$tick)} Downloaded {pb_total} files ",
                       "in {pb_elapsed}."
                    ),
                    total = length(date),
                    clear = FALSE)
   res <- list()
   for (i in date){
      cli_progress_update()
      res_temp <- read_synop(i, summarized = summarized)
      res <- rbind(res, res_temp)
   }

   return(res)
}

#' @description download and read one synop file
#' @param date character; date format as "YYYYMM"
#' @inheritParams sy_download
#' @importFrom utils download.file read.csv2
#' @return logical
#' @noRd
#'
read_synop <- function(date, summarized){

   basename <- ifelse(summarized,
                      "Climat/climat",
                      "Synop/Archive/synop")

   url <- sprintf("https://donneespubliques.meteofrance.fr/donnees_libres/Txt/%s.%s.csv.gz",
                  basename, date)

   tmp <- tempfile()

   download.file(url,
                 destfile = tmp,
                 quiet = TRUE)

   synop <- read.csv2(gzfile(file.path(tmp)),
                      dec = ".",
                      na.strings = c("mq"))
}
