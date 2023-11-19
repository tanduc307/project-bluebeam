airquality -> df

df$year <- 1973

df$date <- paste0(df$year, "-", df$Month, "-", df$Day)

df$date <- as.Date(df$date)

df$time <- row.names(df)

df$time <- as.numeric(df$time)

# options(max.print = 100000)

# windows(width = 18, height = 6) # vẽ chuẩn trên này

# save chuẩn, nếu không khớp cần chỉnh trực tiếp lại
png(width = 18,
    height = 6,
    units = "in",
    res = 300,
    filename = "do_thi_nhieu_truc_tung.png")

# oldpar <- par(no.readonly = TRUE)

par(oma = c(0, 0, 0, 0))

par(mar = c(10, 20, 4, 4))

par(xpd = TRUE)

# par(bg = "aliceblue")

plot(formula = Ozone ~ time, 
     data = na.omit(df[, c("Ozone", "time")]), # nối liền NA
     axes = FALSE, 
     ylim = c(0, max(df$Ozone, na.rm = TRUE)*3),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     lty = 2,
     lwd = 2,
     type = "l",
     col = "blue", 
     main = "",
     xlim = c(1, max(df$time))
)

# Change the plot region color
rect(par("usr")[1], par("usr")[3],
     par("usr")[2], par("usr")[4],
     col = "lightyellow") # Color

###

par(new = TRUE)

plot(formula = Ozone ~ time, 
     data = na.omit(df[, c("Ozone", "time")]), # nối liền NA
     axes = FALSE, 
     ylim = c(0, max(df$Ozone, na.rm = TRUE)*3),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     lty = 2,
     lwd = 2,
     type = "l",
     col = "blue", 
     main = "",
     xlim = c(1, max(df$time))
     )

box(which = "plot")
box(which = "figure")
box(which = "inner")
box(which = "outer")

# points(formula = Ozone ~ time, 
#        data = na.omit(df[, c("Ozone", "time")]), 
#        pch = 20, 
#        col = "black")

## trục Y1

axis(side = 2, 
     # ylim = c(0, max(df$Ozone, na.rm = TRUE)*3),
     at = c(0, 50, 100, 150, 200),
     labels = c(0, 50, 100, 150, 200),
     col = "blue",
     col.axis = "blue",
     font = 2,
     lwd = 2,
     las = 2)

mtext(side = 2,
      text = "Ozone (ppb)",
      line = 3,
      font = 2,
      col = "blue",
      adj = 0)

## trục X1

# axis(side = 1,
#      at = pretty(range(df$time), 10) # tự chia khoảng cho hợp lý
#      )

## scale X1

time_1 <- c(1, 10, 20, 30, 40, 50,
            60, 70, 80, 90, 100, 110,
            120, 130, 140, 153)

axis(side = 1,
     at = time_1,
     labels = time_1,
     font = 2,
     lwd = 2
)

mtext(side = 1,
      text = "Tính theo ngày",
      line = 3,
      font = 2)

###

par(new = TRUE)

plot(formula = Ozone ~ date, 
     data = na.omit(df[, c("Ozone", "date")]), # nối liền NA
     axes = FALSE,
     ylim = c(0, max(df$Ozone, na.rm = TRUE)),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "n",
     col = "black", 
     main = ""
)

## scale X2 (vì là datetime nên phải plot riêng cái mới,
# do R vẽ datetime theo numeric tính từ 1970-01-01)

axis(side = 1,
     at = c(seq.Date(from = df$date[1], to = df$date[153], by = "months"), df$date[153]),
     line = 5,
     labels = c(seq.Date(from = df$date[1], to = df$date[153], by = "months"), df$date[153]),
     font = 2,
     lwd = 2,
     col = "darkgreen",
     col.axis = "darkgreen",
)

mtext(side = 1,
      text = "Thời gian thực đo (YYYY-MM-DD)",
      line = 8,
      col = "darkgreen",
      font = 2)

### VẼ TRỤC Y2

par(new = TRUE)

plot(formula = Solar.R ~ time, 
     data = na.omit(df[, c("Solar.R", "time")]), # nối liền NA
     axes = FALSE, 
     ylim = c(0, max(df$Solar.R, na.rm = TRUE)*2),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "l",
     col = "red", 
     main = "",
     xlim = c(1, max(df$time))
)

## trục Y2

axis(side = 2, 
     # ylim = c(0, max(df$Solar.R, na.rm = TRUE)*2),
     at = c(0, 100, 200, 300, 400),
     labels = c(0, 100, 200, 300, 400),
     col = "red",
     col.axis = "red",
     font = 2,
     lwd = 2,
     las = 2,
     line = 5)

mtext(side = 2,
      text = "Solar radiation (lang)",
      line = 8,
      font = 2,
      col = "red",
      adj = 0)


### VẼ TRỤC Y3

par(new = TRUE)

plot(formula = Wind ~ time, 
     data = na.omit(df[, c("Wind", "time")]), # nối liền NA
     axes = FALSE, 
     ylim = c(0, max(df$Wind, na.rm = TRUE)*1.1),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "l",
     col = "black", 
     lwd = 1,
     lty = 1,
     main = "",
     xlim = c(1, max(df$time))
)

## trục Y3

axis(side = 2, 
     ylim = c(0, max(df$Wind, na.rm = TRUE)*1.1),
     col = "black",
     col.axis = "black",
     font = 2,
     lwd = 2,
     las = 2,
     line = 10)

mtext(side = 2,
      text = "Wind (mph)",
      line = 12,
      font = 2,
      adj = 0,
      col = "black")

### VẼ TRỤC Y4

par(new = TRUE)

plot(formula = Temp ~ time, 
     data = na.omit(df[, c("Temp", "time")]), # nối liền NA
     axes = FALSE, 
     ylim = c(0, max(df$Temp, na.rm = TRUE)*1.2),
     xaxs = "i",
     yaxs = "i",
     xaxt = "n",
     yaxt = "n",
     xlab = "",
     ylab = "",
     type = "l",
     lwd = 2,
     lty = 1,
     col = "cyan2", 
     main = "",
     xlim = c(1, max(df$time))
)

## trục Y4

axis(side = 2, 
     ylim = c(0, max(df$Temp, na.rm = TRUE)*1.2),
     col = "cyan2",
     col.axis = "cyan2",
     font = 2,
     lwd = 2,
     las = 2,
     line = 14)

mtext(side = 2,
      text = "Temperature (\u00B0F)",
      line = 17,
      font = 2,
      adj = 0,
      col = "cyan2")

title(main = "Diễn tiến chất lượng không khí theo thời gian | Nguồn: dataset airquality in R")

###
library(png)
library(grid)
logor <- readPNG("logor.png")
grid.raster(logor, x = 0.05, y = 0.9, width = 0.05)

###
library(unikn)
library(showtext)

par(lheight = 1.15)

###
# https://cran.r-project.org/web/packages/unikn/vignettes/text.html

mark_v1 <- function (labels, x = 0, y = 0.55, x_layout = NA, y_layout = "even", 
                     col = "black", col_bg = Seeblau, cex = 2, font = 2, new_plot = "none", ...) 
{
  if (new_plot == FALSE || tolower(new_plot) == "false" || 
      substr(tolower(new_plot), 1, 2) == "no") {
    new_plot <- "none"
  }
  unikn:::plot_text(labels = labels, x = x, y = y, x_layout = x_layout, 
                    y_layout = y_layout, col = col, col_bg = col_bg, cex = cex, 
                    font = font, new_plot = new_plot, col_bg_border = NA, 
                    pos = 4, mark = TRUE, ...)
}
###

rect(-43, -68,
     -7, -24,
     col = "gold") # Color

mark_v1(labels = c("Thực hiện: Duc Nguyen", 
                   "Website: www.tuhocr.com", 
                   "Chuyên đào tạo R căn bản", 
                   "Welcome to Tự Học R ;)"), 
     x = -40, 
     y = -30,
     family = "mono",
     y_layout = "flush",
     col_bg = "transparent",
     col = "#0000b3",
     cex = 1.2)

# par(oldpar)

dev.off()



