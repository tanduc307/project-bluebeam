trees
?trees

trees -> df

trees -> df_1

names(df_1) <- c("duong_kinh", "chieu_cao", "the_tich")

df

summary(df)

fit1 <- lm(the_tich ~ duong_kinh,
           data = df)

fit1
summary(fit1)

fit2 <- lm(the_tich ~ duong_kinh + chieu_cao,
          data = df)

fit2
summary(fit2)

fit3 <- lm(the_tich ~ duong_kinh + chieu_cao + duong_kinh:chieu_cao,
           data = df)

fit3
summary(fit3)


par(mfrow = c(2, 3))

plot(fit3, which = 1)
plot(fit3, which = 2)
plot(fit3, which = 3)
plot(fit3, which = 4)
plot(fit3, which = 5)
plot(fit3, which = 6)

"diagnostic plot"

df$the_tich_dudoan <- fit$fitted.values

df$the_tich_dudoan <- round(df$the_tich_dudoan, 2)

df$residual <- df$the_tich - df$the_tich_dudoan

plot(x = df$duong_kinh,
     y = df$the_tich,
     col = "blue",
     xlab = "duongkinh",
     ylab = "thetich",
     cex = 1.4,
     xlim = c(0, 30),
     ylim = c(0, 100),
     pch = "*")

abline(fit, col = "red")

ok <- predict(object = fit1,
        newdata = df[, "duong_kinh", drop = FALSE],
        level = 0.95,
        interval = "confidence")
ok

df -> df_1

df_1$dudoan <- ok

predict(object = fit,
        newdata = data.frame(duong_kinh = 15)) -> y_a


abline(v = 15,
       col = "purple",
       lwd = 2)

abline(h = y_a,
       col = "purple",
       lwd = 2)

######################


plot(x = df$duong_kinh,
     y = df$the_tich,
     col = "blue",
     xlab = "duongkinh",
     ylab = "thetich",
     xaxs = "i",
     yaxs = "i",
     cex = 1.4,
     xlim = c(0, 30),
     ylim = c(0, 100),
     pch = "*")

abline(fit1, col = "red")

lines(x = df$duong_kinh,
      y = ok[, 2],
      col = "blue",
      lty = 2,
      lwd = 2)

lines(x = df$duong_kinh,
      y = ok[, 3],
      col = "blue",
      lty = 2,
      lwd = 2)

legend(x = "topleft",
       y = NULL,
       legend = c("khoảng tin cậy", "đường hồi quy"),
       col = c("blue", "red"),
       lty = c(2, 1),
       lwd = 2
       )

points(x = df$duong_kinh,
       y = df$the_tich_dudoan,
       col = "purple",
       type = "h",
       cex = 1,
       pch = 16)

points(x = df$duong_kinh,
       y = df$the_tich_dudoan,
       col = "purple",
       type = "p",
       cex = 1,
       pch = 16)




library(ggplot2)

ggplot(df,
       aes(x = duong_kinh, 
           y = the_tich)) +
  
  geom_point() +
  
  geom_smooth(method='loess')


loess(the_tich ~ duong_kinh,
      data = df) -> fit4
fit4


ok <- predict(object = fit4,
              newdata = df[, "duong_kinh", drop = FALSE],
              level = 0.95,
              interval = "confidence")

points(x = df$duong_kinh,
       y = ok,
       col = "purple",
       type = "o",
       cex = 1,
       pch = 16)
































