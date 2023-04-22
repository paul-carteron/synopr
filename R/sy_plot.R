#' sy_plot_temp
#'
#' @param synop data.frame; result of `sy_download()`
#' @param station numeric; id of station
#'
#' @importFrom grDevices adjustcolor
#' @importFrom graphics abline axis boxplot legend lines mtext par points
#' @importFrom stats setNames smooth.spline
#' @importFrom utils stack

#' @return plot
#' @export
#'
#' @examples
#' \dontrun{
#' # download data
#' synop <- sy_download(date = c("202201", "202304"), T)
#'
#' # find nearest station
#' paris <- st_sfc(st_point(c(2.335, 48.876)), crs = 4326)
#' nearest_station <- sy_nearest_station(paris)
#' num_station <- as.numeric(nearest_station$ID)
#'
#' # plot temperature
#' sy_plot_temp(synop, num_station)
#' }
#'
sy_plot_temp <- function(synop, station){

   t <- numer_sta <- NULL

   synop <- synop |>
      subset(numer_sta %in% station) |>
      subset(!is.na(t)) |>
      within({
         date = strptime(date, format = c("%Y%m%d%H"))
         t = kel_to_deg(t)
         ym = format(as.Date(date), "%Y-%b")
         ym = factor(ym, levels = unique(ym))
         yr = format(as.Date(date), "%Y")
      })

   synop$id <- seq_along(synop[,1])

   # temperature range
   range <- range(synop$t, na.rm = T)
   min <- range[1] - 1
   max  <- range[2] + 4

   # observation per month
   month_lines <- cumsum(lapply(split(synop, ~ym), nrow))
   year_lines <- cumsum(lapply(split(synop, ~yr), nrow))
   year_lines <- year_lines[1:(length(year_lines)-1)]

   # outliers
   outliers <- lapply(split(synop, ~ym), function(x){boxplot(x$t, plot=FALSE)$out})
   outliers <- setNames(stack(outliers)[2:1], c("ym","t"))
   outliers <- merge(outliers, synop)

   # background
   par(mar = c(3, 3, 6, 1))
   plot(NA, xlim = c(0, nrow(synop)), ylim = c(min, max), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
   abline(v = month_lines, col = "gray90", lty = 1, lwd = 0.6)
   abline(v = year_lines, col = "#95a5a6", lwd = 1.5)
   abline(h = seq(-50, 50, 5), col = "gray90", lwd = 0.9, lty = 2)

   # axis
   axis(side = 1, at = month_lines,
        labels = gsub("\\.", "", substr(x = names(month_lines), 6, 9)),
        las = 2, tick = FALSE, cex.axis = 0.85, line = -0.75, font.axis = 3, col.axis = "#95a5a6")
   axis(side = 2, at = seq(-50, 50, 5),
        labels = paste0(seq(-50, 50, 5), paste0("\u00B0")), las = 2, cex.axis = 0.85, tick = FALSE, line = -0.5, col.axis = "#95a5a6")
   axis(side = 3, at = year_lines, labels = names(year_lines), tick = FALSE, line = -0.75, col.axis = "#95a5a6")

   pnt_col= "#95a5a6"
   points(
      x = 1:nrow(synop),
      y = synop$t,
      pch = 20,
      col = adjustcolor(col = pnt_col, alpha.f = .5), cex = 0.9
   )

   points(
      x = outliers$id,
      y = outliers$t,
      pch = 20,
      col = adjustcolor(col = "purple", alpha.f = .5), cex = 1
   )

   lines(smooth.spline(x = 1:nrow(synop), y = synop$t),
         col = adjustcolor("firebrick", alpha.f = 1) , lwd = 2)

   mytitle <- "Temperature every 3 hours"
   mysubtitle <- paste(format(min(synop$date, na.rm = T), "%Y/%m/%d"),
                       "-",
                       format(max(synop$date, na.rm = T), "%Y/%m/%d"))
   mtext(side=3, line=3, at=-0.07, adj=0, cex=1.35, col = "#34495e", mytitle)
   mtext(side=3, line=2, at=-0.07, adj=0, cex=0.85, col = "#34495e", mysubtitle)

   legend(x = "topleft",
          legend = c("Hourly temperature", "Outlier by month"),
          col = c("#95a5a6", "purple"),
          pch = c(20, 20), bty = "n", cex = 0.95, text.col = "#34495e", pt.cex = 1.5, yjust = 0)

}
