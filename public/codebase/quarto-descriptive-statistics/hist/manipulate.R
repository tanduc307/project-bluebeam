# install.packages("manipulate")

library(manipulate)
# https://rpubs.com/jeremyyew/manipulate-pkg

?manipulate

windows()



par(lheight = 4)

?manipulate

manipulate(
  boxplot(Freq ~ Class, 
          data = Titanic, 
          xlab = "aaa\nbbbb",
          outline = outline),
  outline = checkbox(FALSE, "Show outliers"))
      

manipulate(
  plot(pressure, type = type_1a, col = 2, main = "aaa"),
  type_1a = picker("points" = "p", 
                   "line" = "l", 
                   "step" = "s"),
  mau_sac = slider(min = 0, max = 30))

title(main = "bbb")

manipulate(hist(x = rnorm(ok)),
           ok = slider(min = 1, max = 1000))
##############################################
windows(8, 8, rescale = "fixed")

# https://rpubs.com/xuzhihu/1093401


yy <- c(31,31,31,50,50,61,69,75,80,88,94,101,108,115,121,124,125,125,125,126,127)
name1 <- c ("15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35")

#draw bar plot
xx <- barplot(yy, ylab = "", names.arg = name1, ylim = c(0, 140),col="steelblue")
# text(xx, yy + 3, labels = as.character(yy),srt=45)
# mtext(2,text="",line=2)

# xx2 <- xx #c(15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)
# yy2 <-   c(379,474,579,725,922,1181,1473,1846,2316,2962,3688,4786,6069,7605,9504,10680,11074,11074,11074,11483,11484)

#transform data
# yy2tx <- yy2/14000 * max(pretty(yy))

#draw line data
lines(xx, yy, lwd = 2,col="red",lty=1)
points(xx, yy, pch = 18, cex = 1,col="red")
text(xx, yy, labels = as.character(yy),srt=90)

###############################################################

ok <- round(rnorm(93, mean = 50, sd = 10), digits = 0)

# ok

yes <- hist(ok, xlim = c(0, 100), probability = FALSE)

diff(yes$breaks)

points(x = c(yes$breaks[1] - unique(diff(yes$breaks))/2, 
             yes$mids, 
             yes$breaks[which.max(yes$breaks)] + unique(diff(yes$breaks))/2),
      y = c(0, yes$counts, 0),
      col = "red",
      pch = 19,
      lwd = 2,
      type = "o")

sum(ok)

sum(diff(yes$breaks) * yes$density)

sum(yes$counts/length(ok)) ### tính thủ công


### tổng diện tích dưới đường cong freq tần số là tổng số lần xuất hiện của các con số trong vector ok (ở đây ok có có 93 số thì sẽ có 93 lần xuất hiện trong phần diện tích dưới đường cong). Đơn vị là lần xuất hiện của x
sum(diff(yes$breaks) * yes$counts) / unique(diff(yes$breaks))
sum(yes$counts) ### tính thủ công
######################################
windows(9, 9, rescale = "fixed")

ok <- round(rnorm(90003, mean = 50, sd = 10), digits = 0)

yes <- hist(ok, xlim = c(min(ok) - 100, max(ok) + 100) ,
            probability = TRUE)

diff(yes$breaks)

yes
## ĐÂY KO CÒN LÀ ĐƯỜNG FREQUENCY POLYGON NỮA MÀ LÀ DENSITY POLYGON
points(x = c(yes$breaks[1] - unique(diff(yes$breaks))/2, 
             yes$mids, 
             yes$breaks[which.max(yes$breaks)] + unique(diff(yes$breaks))/2),
       y = c(0, yes$density, 0),
       col = "red",
       pch = 19,
       lwd = 2,
       type = "o")

### Tổng diện tích dưới đường cong là 1, đơn vị của xác suất (là ko có đơn vị)
sum(diff(yes$breaks) * yes$density)



################################################



hist_1 <- function(x_1, x_2, max_1, max_2, max_y, break_a) {
  
  
  number_x <- x_1 * x_2
  
  ok <- rnorm(number_x, mean = 50, sd = 10)
  
  happy <- hist(ok, plot = FALSE)
  
  yes <- hist(ok, 
              xlim = c(max(ok) - max_1, max(ok) + max_2), 
              ylim = c(0, max(happy$density) / max_y),
              probability = TRUE,
              breaks = break_a)

    ## ĐÂY KO CÒN LÀ ĐƯỜNG FREQUENCY POLYGON NỮA MÀ LÀ DENSITY POLYGON
  points(x = c(yes$breaks[1] - unique(diff(yes$breaks))/2, 
               yes$mids, 
               yes$breaks[which.max(yes$breaks)] + unique(diff(yes$breaks))/2),
         y = c(0, yes$density, 0),
         col = "red",
         pch = 19,
         lwd = 2,
         type = "o")
  
}



library(manipulate)
manipulate(hist_1(x_1, x_2, max_1, max_2, max_y, break_a),
           x_1 = slider(min = 1, max = 100),
           x_2 = slider(min = 1, max = 100),
           max_1 = slider(min = 1, max = 100),
           max_2 = slider(min = 1, max = 100),
           max_y = slider(min = 1, max = 100),
           break_a = slider(min = 1, max = 10000))

manipulate(hist(x),
           x = slider(min = 0, max = 2000),
           x_1 = slider(min = 0, max = 2000))


manipulate(
  boxplot(Freq ~ Class, 
          data = Titanic, 
          xlab = "aaa\nbbbb",
          outline = outline),
  outline = checkbox(FALSE, "Show outliers"))




















