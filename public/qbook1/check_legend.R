plot(production ~ year, 
     data = coffee_vn_2010_2021,
     las = 1,
     xaxs = "i",
     yaxs = "i",
     col = "blue",
     xlim = c(2009, 2022),
     ylim = c(0, 2),
     pch = 19,
     type = "b",
     xlab = "Năm",
     ylab = "Sản lượng (triệu tấn)",
     # main = "Tình hình sản xuất cà phê ở Việt Nam và Indonesia giai đoạn 2010–2021",
     sub = "Nguồn: FAOSTAT",
     xaxt = "n" # remove thông tin trục x
     # yaxt="n"
)

## dataset coffee_indo_2010_2021
points(production ~ year, 
       data = coffee_indo_2010_2021,
       col = "red",
       pch = 17,
       type = "b"
)

# Changing x axis

xtick <- seq(from = 2009, to = 2022, by = 1) # x tick label
xtick[1] <- ""
xtick[14] <- ""

axis(side = 1, at = xtick, labels = TRUE) # vẽ trục x (side = 1)

# Thay đổi vị trí title

title(main = "Tình hình sản xuất cà phê ở Việt Nam và Indonesia giai đoạn 2010–2021",
      line = 3)

# legend

legend(x = (2022 + 2009) / 2, 
       y = 2.05,  
       legend = c("Việt Nam", "Indonesia"),
       col = c("blue", "red"),
       lty = c(1, 1), 
       cex = 1,
       pt.cex = 1, # cex của point
       pch = c(19, 17),
       lwd = 2,
       x.intersp = 2,
       # y.intersp = 1,
       xjust = 0.5,
       yjust = 0,
       box.lty = 0,
       horiz = TRUE,
       xpd = TRUE,
       adj = c(0.4, 0.5), # chỉnh text legend
       merge = TRUE # gộp pch và line
)

text(x = 2011, 
     y = 1, 
     labels = "Bước 5: Đồ thị hoàn thiện", 
     cex = 1.5,
     col = "#ff007b", 
     font = 2,
     pos = 4)

# abline(v = (2022 + 2009) / 2, col = "purple", lty = 2, xpd = TRUE)

library(png)
library(grid)
logor <- readPNG("logor.png")
par(xpd = TRUE)
grid.raster(logor, x = 0.9, y = 0.9, width = 0.1)