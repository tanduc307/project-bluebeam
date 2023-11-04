library(readxl)

doanh_thu <- readxl::read_excel("doanh_thu_ban_hang.xlsx")

doanh_thu <- as.data.frame(doanh_thu)

doanh_thu <- doanh_thu[ , c(1, 2, 5, 8, 6, 3, 4, 7, 9)]

doanh_thu 

table(doanh_thu$`Giới tính`, useNA = "no") -> gender
gender
class(gender)

library(precisePlacement)
?precisePlacement
windows(8, 8)

barplot(height = gender[1],
        las = 1,
        xlab = "Giới tính",
        ylab = "Số đếm",
        main = "Số lượng nam nữ trong công ty",
        sub = "Nguồn: Dữ liệu mô phỏng",
        width = c(0.1, 0.1),
        xlim = c(0, 0),
        space = c(-6, 9),
        col = c("coral", "aquamarine"),
        ylim = c(0, 50),
        horiz = FALSE,
        border = c("purple", "blue")
)

box()

par(pty = "s")
par(mar = c(2, 2, 2, 2))
par(oma = c(2, 2, 2, 2))


bbb <- barplot(height = gender[1],
        xaxs = "i", 
        yaxs = "i",
        xlim = c(-1.2, 1.2),
        ylim = c(0, 100),
        width = c(0.1),
        space = c(-0.5)
        )

abline(h = 50, v = 0, col = "red", lty = 2)


par("usr")

box()

showMarginLines()
showOuterMarginLines()
highlightDataRegion()
highlightDeviceRegion()
highlightFigureRegion()
highlightPlotRegion()


#########################









par(pty = "s")
par(mar = c(2, 2, 2, 2))
par(oma = c(2, 2, 2, 2))


barplot(height = gender[1],
               xaxs = "i", 
               yaxs = "i",
               xlim = c(-1.2, 1.2),
               ylim = c(0, 100),
               width = c(1.2),
               space = c(-0.5)
)

abline(h = 50, v = 0, col = "red", lty = 2)


par("usr")

box()

showMarginLines()
showOuterMarginLines()
highlightDataRegion()
highlightDeviceRegion()
highlightFigureRegion()
highlightPlotRegion()


###############################


par(pty = "s")
par(mar = c(2, 2, 2, 2))
par(oma = c(2, 2, 2, 2))
par(xpd = TRUE)


barplot(height = gender,
        xaxs = "i", 
        yaxs = "i",
        # xlim = c(-3, 3),
        ylim = c(0, 100),
        width = c(0.5, 0.5),
        space = c(-1.5, 1)
)

abline(h = 50, v = (par("usr")[1] + par("usr")[2])/2, 
       col = "red", lty = 2)
text(x = (par("usr")[1] + par("usr")[2])/2, y = 0, 
     labels = as.character((par("usr")[1] + par("usr")[2])/2), 
     pos = 1, font = 2)

abline(v = -2, col = "blue", lty = 2)
text(x = -2, y = 0, labels = "-2", pos = 1, font = 2)

abline(v = -1, col = "blue", lty = 2)
text(x = -1, y = 0, labels = "-1", pos = 1, font = 2)

abline(v = -0.75, col = "blue", lty = 2)
text(x = -0.75, y = 0, labels = "-0.75", pos = 1, font = 2)

abline(v = -0.5, col = "blue", lty = 2)
text(x = -0.5, y = 0, labels = "-0.5", pos = 1, font = 2)

abline(v = -0.25, col = "blue", lty = 2)
text(x = -0.25, y = 0, labels = "-0.25", pos = 1, font = 2)

abline(v = 2, col = "blue", lty = 2)
text(x = 2, y = 0, labels = "2", pos = 1, font = 2)

abline(v = 1.75, col = "blue", lty = 2)
text(x = 1.75, y = 0, labels = "1.75", pos = 1, font = 2)

abline(v = 1.5, col = "blue", lty = 2)
text(x = 1.5, y = 0, labels = "1.5", pos = 1, font = 2)

abline(v = 1.25, col = "blue", lty = 2)
text(x = 1.25, y = 0, labels = "1.25", pos = 1, font = 2)

abline(v = 1, col = "blue", lty = 2)
text(x = 1, y = 0, labels = "1", pos = 1, font = 2)

abline(v = 0.75, col = "blue", lty = 2)
text(x = 0.75, y = 0, labels = "0.75", pos = 1, font = 2)

abline(v = 0.5, col = "blue", lty = 2)
text(x = 0.5, y = 0, labels = "0.5", pos = 1, font = 2)

abline(v = 0.25, col = "blue", lty = 2)
text(x = 0.25, y = 0, labels = "0.25", pos = 1, font = 2)

par("usr")

box(which = "plot", col = "purple")
box(which = "outer", col = "darkgreen")
box(which = "figure", col = "cyan")




###################################

###############################


par(pty = "s")
par(mar = c(2, 2, 2, 2))
par(oma = c(2, 2, 2, 2))


barplot(height = gender,
        xaxs = "i", 
        yaxs = "i",
        xlim = c(-4, 4),
        ylim = c(0, 100),
        width = c(0.8, 0.8),
        space = c(-1.25*2, 0.5*2),
        beside = TRUE,
)

abline(h = 50, v = (par("usr")[1] + par("usr")[2])/2, 
       col = "red", lty = 2)


par("usr")

box()

showMarginLines()
showOuterMarginLines()
highlightDataRegion()
highlightDeviceRegion()
highlightFigureRegion()
highlightPlotRegion()






























