df_1 <- read.csv("students_summary.csv")

df_1$major <- factor(df_1$major,
                     levels = c("Biology",
                                "Economics",
                                "Environmental Sciences",
                                "Statistics",
                                "Political Science",
                                "Social Sciences"))

df_1$gender <- factor(df_1$gender,
                     levels = c("Female",
                                "Male"))

# windows(16, 8, rescale = "fixed")
png(filename = "barplot_with_table.png",
    width = 16,
    height = 8,
    units = "in",
    res = 300)

# oldpar <- par(no.readonly = TRUE)

par(mar = c(10.5, 12, 4, 4))
par(xpd = NA)
par(mgp = c(3, 1, 0))
par(font = 2)
par(font.axis = 2)
par(font.lab = 2)

p1 <- barplot(students ~ gender + major,
        data = df_1,
        beside = TRUE,
        col = c("blue", "red"),
        angle = c(45, 135),
        density = 20,
        xlab = "",
        ylab = "Sinh viên",
        yaxs = "i",
        ylim = c(0, 1000),
        xaxs = "i",
        xlim = c(0.5, 18.5),
        xaxt = "n",
        width = 1,
        las = 1)

box(which = "plot",
    bty = "l")

###
par("usr") -> usr_ok # lưu tọa độ barplot

a_1 <- dev.size(units = "px") # đơn vị px
a_2 <- par("plt") # đơn vị phần trăm
###

title(main = "Số lượng sinh viên theo học ở các chuyên ngành khác nhau")

text(x = p1, 
     y = df_1$students, 
     labels = df_1$students,
     pos = 3,
     font = 3,
     col = "red"
     )

###

# abline(h = 0, col = "black")

clip(x1 = -2,
     x2 = 18.5,
     y1 = -300,
     y2 = 0)

abline(v = 0.5, col = "black")
abline(v = 3.5, col = "black")
abline(v = 6.5, col = "black")
abline(v = 9.5, col = "black")
abline(v = 12.5, col = "black")
abline(v = 15.5, col = "black")
abline(v = 18.5, col = "black")


abline(h = -100, col = "black")
abline(h = -200, col = "black")
abline(h = -300, col = "black")
abline(h = -400, col = "black")

# points(x = 2,
#        y = -50,
#        col = "blue",
#        pch = 19)

text(x = 2,
     y = -50,
     labels = "Biology",
     col = "#ff0065",
     adj = 0.5
)

text(x = 5,
     y = -50,
     labels = "Economics",
     col = "#ff0065",
     adj = 0.5
)

text(x = 8,
     y = -50,
     labels = "Environmental Sciences",
     col = "#ff0065",
     adj = 0.5
)

text(x = 11,
     y = -50,
     labels = "Statistics",
     col = "#ff0065",
     adj = 0.5
)

text(x = 14,
     y = -50,
     labels = "Political Science",
     col = "#ff0065",
     adj = 0.5
)

text(x = 17,
     y = -50,
     labels = "Social Sciences",
     col = "#ff0065",
     adj = 0.5
)

# abline(v = 0, col = "black")
# abline(v = -1, col = "black")


text(x = -0.55,
     y = -150,
     labels = "Nữ",
     col = "black",
     adj = 0
)

text(x = -0.55,
     y = -250,
     labels = "Nam",
     col = "black",
     adj = 0
)

rect(xleft = -1.4,
     xright = -0.9,
     ybottom = -120,
     ytop = -180,
     col = "blue",
     border = "black",
     angle = 45,
     density = 20)

rect(xleft = -1.4,
     xright = -0.9,
     ybottom = -220,
     ytop = -280,
     col = "red",
     border = "black",
     angle = 135,
     density = 20)

#######################

for(i in 0:(length(df_1$students[df_1$gender == "Female"]) - 1)) {
  
  text(x = 2 + 3*i,
       y = -150,
       labels = df_1$students[df_1$gender == "Female"][i + 1],
       col = "black",
       adj = 0.5
  )
  
}

for(i in 0:(length(df_1$students[df_1$gender == "Male"]) - 1)) {
  
  text(x = 2 + 3*i,
       y = -250,
       labels = df_1$students[df_1$gender == "Male"][i + 1],
       col = "black",
       adj = 0.5
  )
  
}

###
clip(x1 = -2,
     x2 = 18.5,
     y1 = -300,
     y2 = -100)

abline(v = -2, col = "black")
###

### reset clip
clip(x1 = grconvertX(0, "ndc", "user"),
     x2 = grconvertX(1, "ndc", "user"),
     y1 = grconvertY(0, "ndc", "user"),
     y2 = grconvertY(1, "ndc", "user"))

# par(oldpar)

text(x = -2,
     y = -350,
     labels = "Nguồn: Dữ liệu mô phỏng để vẽ đồ thị cột theo nhóm có chèn bảng số liệu bên dưới.",
     xpd = TRUE,
     adj = 0,
     font = 3,
     cex = 0.9,
     col = "black",
     srt = 0,
     xpd = NA)

text(x = 18.5,
     y = -350,
     labels = "Thực hiện: Duc Nguyen | Chuyên đào tạo kỹ năng R | www.tuhocr.com",
     xpd = TRUE,
     adj = 1,
     font = 2,
     cex = 0.9,
     col = "navyblue",
     srt = 0,
     xpd = NA)

###

library(png)
library(grid)
logor <- readPNG("logor.png")

scale_logo <- 0.08

grid.raster(logor, 
            x = ((a_2[2] * a_1[1]) - (scale_logo * a_1[1])/2) / a_1[1], 
            y = ((a_2[4] * a_1[2]) - ((scale_logo * a_1[1]) * dim(logor)[1] / dim(logor)[2])/2) / a_1[2], 
            width = scale_logo)

dev.off()

shell("barplot_with_table.png")



























