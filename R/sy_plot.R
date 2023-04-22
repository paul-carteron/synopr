# test2 <- synop |>
#    subset(numer_sta == 7005) |>
#    subset(!is.na(t)) |>
#    transform(date = strptime(date, format = c("%Y%m%d%H")),
#              t = kel_to_deg(t))
#
# # temperature range
# range <- range(test2$t, na.rm = T)
# min <- range[1] - 1
# max  <- range[2] + 4
#
# # observation per month
# test2$ym <- format(as.Date(test2$date), "%Y-%b")
# test2$ym <- factor(test2$ym, levels = unique(test2$ym))
# test2$yr <- format(as.Date(test2$date), "%Y")
# test2$id <- seq_along(test2[,1])
# month_lines = cumsum(lapply(split(test2, ~ym), nrow))
# year_lines = cumsum(lapply(split(test2, ~yr), nrow))
# year_lines <- year_lines[1:(length(year_lines)-1)]
#
# outliers <- lapply(split(test2, ~ym), function(x){boxplot(x$t, plot=FALSE)$out})
# outliers <- setNames(stack(outliers)[2:1], c('ym','t'))
# outliers <- merge(outliers, test2)
#
# # background
# par(mar = c(3, 3, 6, 1))
# plot(NA, xlim = c(0, nrow(test2)), ylim = c(min, max), frame.plot = FALSE, axes = FALSE, xlab = NA, ylab = NA)
# abline(v = month_lines, col = "gray90", lty = 1, lwd = 0.6)
# abline(v = year_lines, col = "#95a5a6", lwd = 1.5)
# abline(h = seq(-50, 50, 5), col = "gray90", lwd = 0.9, lty = 2)
#
# # axis
# axis(side = 1, at = month_lines,
#      labels = gsub(".", "", substr(x = names(month_lines), 6, 9)),
#      las = 2, tick = FALSE, cex.axis = 0.85, line = -0.75, font.axis = 3, col.axis = "#95a5a6")
# axis(side = 2, at = seq(-50, 50, 5),
#      labels = paste0(seq(-50, 50, 5),"°"), las = 2, cex.axis = 0.85, tick = FALSE, line = -0.5, col.axis = "#95a5a6")
# axis(side = 3, at = year_lines, labels = names(year_lines), tick = FALSE, line = -0.75, col.axis = "#95a5a6")
#
# pnt_col= "#95a5a6"
# points(
#    x = 1:nrow(test2),
#    y = test2$t,
#    pch = 20,
#    col = adjustcolor(col = pnt_col, alpha.f = .5), cex = 0.9
# )
#
# points(
#    x = outliers$id,
#    y = outliers$t,
#    pch = 20,
#    col = adjustcolor(col = "firebrick", alpha.f = .5), cex = 1
# )
#
# lines(smooth.spline(x = 1:nrow(test2), y = test2$t),
#       col = adjustcolor("grey20", alpha.f = 1) , lwd = 2)
#
# mytitle = "Temperature every 3 hours"
# mysubtitle = paste(format(min(test2$date), "%Y/%m/%d"), "-", format(max(test2$date), "%Y/%m/%d"))
# mtext(side=3, line=3, at=-0.07, adj=0, cex=1.35, col = "#34495e", mytitle)
# mtext(side=3, line=2, at=-0.07, adj=0, cex=0.85, col = "#34495e", mysubtitle)
#
# legend(x = "topleft",
#        legend = c("Hourly temperature", "Outlier par mois"),
#        col = c("#95a5a6", "firebrick"),
#        pch = c(20, 20), bty = "n", cex = 0.95, text.col = "#34495e", pt.cex = 1.5, yjust = 0)
