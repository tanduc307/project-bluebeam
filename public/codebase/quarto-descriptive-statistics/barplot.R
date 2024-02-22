# df <- read.csv("students.csv")
# library(dplyr)
# 
# df |> dplyr::group_by(major, gender) |>
#   dplyr::summarise(so_nguoi_theo_hoc = n()) -> df_major_gender 
# 
# df_1 <- as.data.frame(df_major_gender)
# 
# windows(16, 8, rescale = "fixed")
# 
# oldpar <- par(no.readonly = TRUE)

write.csv(df_1, "students_summary.csv", row.names = FALSE)



par(mar = c(10, 12, 4, 4))
par(xpd = NA)
par(mgp = c(3, 1, 0))

barplot(so_nguoi_theo_hoc ~ gender + major,
        data = df_1,
        beside = TRUE,
        col = c("blue", "black"),
        angle = c(45, 135),
        density = 20,
        xlab = "",
        ylab = "Sinh viên",
        yaxs = "i",
        ylim = c(0, 1000),
        xaxs = "i",
        xlim = c(0.5, 18.5),
        xaxt = "n",
        # space = 0, # chỉ tính khi beside = FALSE
        width = 1,
        las = 1
        )

# axis(side = 1, at = seq(from = 0.5, to = 18.4, by = 0.5))

par("usr")
box()

# abline(h = 0, col = "black")

clip(x1 = -2, 
     x2 = 18.5, 
     y1 = -300, 
     y2 = -100)

abline(v = -2, col = "black")

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
     col = "black",
     adj = 0.5
)

text(x = 5,
     y = -50,
     labels = "Economics and Finance",
     col = "black",
     adj = 0.5
)

text(x = 8,
     y = -50,
     labels = "Environmental Sciences",
     col = "black",
     adj = 0.5
)

text(x = 11,
     y = -50,
     labels = "Mathematics and Statistics",
     col = "black",
     adj = 0.5
)

text(x = 14,
     y = -50,
     labels = "Political Science",
     col = "black",
     adj = 0.5
)

text(x = 17,
     y = -50,
     labels = "Social Sciences",
     col = "black",
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
     col = "black",
     border = "black",
     angle = 45,
     density = 20)

#######################


for(i in 0:(length(df_1$so_nguoi_theo_hoc[df_1$gender == "Female"]) - 1)) {
  
  text(x = 2 + 3*i,
       y = -150,
       labels = df_1$so_nguoi_theo_hoc[df_1$gender == "Female"][i + 1],
       col = "black",
       adj = 0.5
  )
  
}

for(i in 0:(length(df_1$so_nguoi_theo_hoc[df_1$gender == "Male"]) - 1)) {
  
  text(x = 2 + 3*i,
       y = -250,
       labels = df_1$so_nguoi_theo_hoc[df_1$gender == "Male"][i + 1],
       col = "black",
       adj = 0.5
  )
  
}
















# dev.off()



# 
# # data generation ---------------------------------------------------------
# set.seed(1)
# mat <- matrix(runif(4*7, min=0, max=10), 7, 4)
# rownames(mat) <- 1:7
# colnames(mat) <- LETTERS[1:4]
# 
# 
# # plotting settings -------------------------------------------------------
# ylim <- range(mat)*c(1,1.5)
# angle1 <- rep(c(45,45,135), length.out=7)
# angle2 <- rep(c(45,135,135), length.out=7)
# density1 <- seq(5,35,length.out=7)
# density2 <- seq(5,35,length.out=7)
# # col <- 1 # rainbow(7)
# 
# col <- rainbow(7)
# 
# # plot --------------------------------------------------------------------
# op <- par(mar=c(3,3,1,1))
# barplot(mat, beside=TRUE, ylim=ylim, col=col, angle=angle1, density=density1)
# barplot(mat, add=TRUE, beside=TRUE, ylim=ylim, col=col, angle=angle2, density=density2)
# legend("top", legend=1:7, ncol=7, fill=TRUE, col=col, angle=angle1, density=density1)
# par(bg="transparent")
# legend("top", legend=1:7, ncol=7, fill=TRUE, col=col, angle=angle2, density=density2)
# par(op)
# 
# 
# 
































