### lag

ldeaths

lag(ldeaths, k = 1)

lag(ldeaths, k = 2)

lag(ldeaths, k = 12)

lag(ldeaths, k = -1)

lag(ldeaths, k = -2)

lag(ldeaths, k = -12)

x <- 1:10
x
lag(x, k = 1)
lag(x, k = 2)
lag(x, k = 3)
lag(x, k = 10)
lag(x, k = -10)

df <- data.frame(day = 1:10,
                 sales=c(18, 10, 14, 13, 19, 24, 25, 29, 15, 18))

df

df$previous_day_salesa <- dplyr::lead(df$sales, n = 2)

df
lag(df$sales, n = 1)

?lead

###############################

?ts


ts(data = 1:36, 
   frequency = 4, 
   start = c(2024, 0))

ts(data = 1:36, 
   frequency = 4, 
   start = c(2024, 1))

ts(data = 1:36, 
   frequency = 4, 
   start = c(2024))

ts(data = 1:36, 
   frequency = 4, 
   start = c(2024),
   end = c(2027)) -> ok

ok
unclass(ok)
unlist(ok)
attributes(ok)

ts(data = 1:36, 
   frequency = 12, 
   start = c(2024, 0))

ts(data = 1:36, 
   frequency = .7, 
   start = c(2024, 0))

######################

library(fpp2)
install.packages("fpp3")

library(fpp3)


# https://people.duke.edu/~rnau/arimrule.htm

aus_production

library(dplyr)
library(tsibble)

recent_production <- aus_production |>
  filter(year(Quarter) >= 2006 & year(Quarter) <= 2007)


lubridate::year(beer$Quarter)


library(ggplot2)
recent_production |>
  feasts::gg_lag(Beer, 
                 geom = "point",
                 lag = 3) +
  labs(x = "lag(Beer, k)")


# https://phamdinhkhanh.github.io/2019/12/12/ARIMAmodel.html#2-m%C3%B4-h%C3%ACnh-arima


# https://rpubs.com/phamdinhkhanh/271055
# https://en.wikipedia.org/wiki/Lag_operator
# https://github.com/phamdinhkhanh/vnquant


devtools::install_github("phamdinhkhanh/VNDS")

# https://rpubs.com/phamdinhkhanh/392610


library(VNDS)
#2.Lấy bảng cân đối kế toán
#Cho 5 năm tài chính gần nhất kể từ 2017
VNDBalanceSheet <- tq_balancesheet(symbol = "VND",
                                    endYear = 2017,
                                    n = 5,
                                    period = "IN_YEAR")



























