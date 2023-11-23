library(readxl)
df <- read_excel("doanh_thu_ban_hang.xlsx")
df <- as.data.frame(df)

df

# Số liệu nhân viên theo giới tính và chức vụ ở các chi nhánh
  
library(lattice)

table(df$`Giới tính`,
      df$`Chức vụ`,
      df$`Chi nhánh`) -> ok_1

plot(ok_1)

ok_2 <- as.data.frame(ok_1)

ok_2

names(ok_2) <- c("gioi_tinh", "chuc_vu", "chi_nhanh", "count")

ok_2

windows(8, 8)

lattice::barchart(count ~ gioi_tinh | chuc_vu + chi_nhanh, 
                  group = gioi_tinh,
                  data = ok_2,
                  
                  horizontal = FALSE,
                  stack = TRUE,
                  col = c("lightblue", "lightyellow"),
                  
                  ## edit xy axis
                  scales = list(axs = "i",
                                draw = TRUE,
                                x = list(relation = "free"), # relation = free | same
                                y = list(relation = "same",
                                         limits = c(0, 20))
                                ),
                  
                  ## axis label and title
                  xlab = list(label = "Đặc điểm phân loại", fontsize = 16, col = "darkgreen", font = 2),
                  ylab = list(label = "Số lượng nhân viên", fontsize = 16, col = "#FF1694", font = 2),
                  main = "Số liệu nhân viên theo giới tính và chức vụ ở các chi nhánh",
                  sub = "Nguồn: Phòng nhân sự cung cấp",
                  
                  ## value above bar
                  par.settings = list(strip.background = list(col = c("lightgreen", "yellow"))),
                  
                  panel = function(x, y, subscripts, ...){
                    
                    panel.grid(h = -1,v = 0)
                    
                    panel.barchart(x, y, subscripts = subscripts,...)
                    
                    t <- aggregate(y ~ x, data.frame(x, y), FUN = sum)
                    
                    panel.text(t$x, t$y, 
                               labels = t$y, 
                               pos = 3, 
                               col = "blue",
                               fontsize = 13,
                               font = 2
                               
                               )
                    
                  }
                  
                  )

# https://stackoverflow.com/questions/23725306/r-lattice-barchart-how-to-write-the-total-sum-on-each-bar-in-multiple-panels
# 
# p <- barchart((1:10)^2~1:10, horiz=FALSE, ylim=c(0,120),
#               panel=function(...) { 
#                 args <- list(...)
#                 panel.text(args$x, args$y, args$y, pos=3, offset=1)
#                 panel.barchart(...)
#               })
# 
# p 
# 
# 
# 
# 
# 

df$doanh_so <- df$`Doanh thu bán hàng` / 1000000

xyplot(doanh_so ~ `Tuổi` |`Chức vụ` + `Chi nhánh` , 
       data = df,
       groups = `Giới tính`,
       # layout = c(1, 2), # thay đổi vị trí các panel con
       pch = 19,
       col = c("red", "blue"),

       key = list (space = "top", column = 2,
                   text = list(c("Nam", "Nữ")),
                   points = list(pch = c(19, 19),
                                 col = c("red", "blue"))

                   ),
       
       ## axis label and title
       xlab = list(label = "Tuổi", fontsize = 16, col = "darkgreen", font = 2),
       ylab = list(label = "Doanh số (triệu VNĐ)", fontsize = 16, col = "#FF1694", font = 2),
       main = "Doanh số bán hàng theo chức vụ ở các chi nhánh",
       sub = "Nguồn: Phòng nhân sự cung cấp",
       
       scales = list(axs = "i",
                     draw = TRUE,
                     x = list(relation = "free", # relation = free | same
                              limits = c(0, 100)), 
                     y = list(relation = "same",
                              limits = c(-300, 3500)
                              )
                     ),
       
       ## value above bar
       par.settings = list(strip.background = list(col = c("lightgreen", "yellow"))),
       
       )




# https://stackoverflow.com/questions/46222308/r-package-lattice-group-by-color-doesnt-work-when-using-panel-argument



dev.off()
