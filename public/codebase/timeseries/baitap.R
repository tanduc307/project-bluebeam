library(dplyr)
library(ggplot2)
library(readxl)
GHZ <- read_excel("GHZ.xlsx")
GHZ <- as.data.frame(GHZ)
head(GHZ)

class(GHZ$DATE)

GHZ$DATE <- as.Date(GHZ$DATE,
                    format = "%d/%m/%Y")

plot(PRICE ~ DATE,
     data = GHZ,
     col = "blue",
     type = "l")

class(GHZ)

library(zoo)
GHZ_ts <- read.zoo(GHZ)

class(GHZ_ts)
zoo:::autoplot.zoo(GHZ_ts)

ts(data = 1:36, 
   frequency = 12, 
   start = c(2024, 0))

library(fpp2)

GHZ_ts[1:20]

tail(GHZ, n = 20)

check <- GHZ_ts[1:20] %>% diff(lag = 1,
                differences = 1) %>% ggtsdisplay(main = "GHZ")

x <- c(5, 4, 7, 9, 12, 32, 11)
x
diff(x)
diff(x, lag = 1, differences = 1)

diff(x, lag = 1, differences = 2)
diff(diff(x))



diff(x, lag = 2, differences = 2)

?ldeaths

stats::lag(ldeaths, k = -12)

stats::lag(ldeaths, k = 3)

GHZ_ts[1:10] -> ok

### check lại lag theo năm

stats::lag(GHZ_ts[1:10], k = 1)

stats::lag(GHZ_ts[1:10], k = 2)

stats::lag(GHZ_ts[1:10], k = 0)

stats::lag(GHZ_ts[1:10], k = -1)

df <- data.frame(year = 2010:2019,
                 sales=c(18, 10, 14, 13, 19, 24, 25, 29, 15, 18))


df

df$previous_day_sales <- dplyr::lag(df$sales, n = 2)

df$previous_day_sales <- dplyr::lead(df$sales, n = 2)

df

################

GHZ_ts %>% diff(lag = 2,
                      differences = 2) %>% 
  ggtsdisplay(main = "GHZ")


fit_GHZ <- Arima(GHZ_ts, order = c(1, 0, 0))

?Arima

summary(fit_GHZ)


dudoan <- 15 

ketqua_GHZ <- forecast(fit_GHZ, h = dudoan)

predict(fit_GHZ, n.ahead = dudoan)



forecast:::plot.forecast(ketqua_GHZ, showgap = TRUE)

source("forcast_data.R")

par(mar = c(5, 5, 5, 5))
forcast_data(ketqua_GHZ, xaxt = "n",
             showgap = FALSE,
             main = "GHZ",
             xlab = "DATE",
             ylab = "PRICE",
             las = 2,
             ylim = c(10000, 30000))

labels_ok <- seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "weeks")

axis(side = 1, 
     at = c(seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "weeks"), as.Date(all_date)[length(all_date)]),
     labels = c(format(labels_ok, format="%Y\n%m-%d"), 
                format(as.Date(all_date)[length(all_date)], format="%Y\n%m-%d")),
     # col.ticks = NA,
     col.axis = NA,
     # col = NA
     # line = 2
)

axis(side = 1, 
     at = c(seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "weeks"), as.Date(all_date)[length(all_date)]),
     labels = c(format(labels_ok, format="%Y\n%m-%d"), 
                format(as.Date(all_date)[length(all_date)], format="%Y\n%m-%d")),
     col.ticks = NA,
     col.axis = "black",
     cex.axis = 1,
     col = NA,
     line = 1)

abline(v = as.Date(all_date)[length(all_date)], col = "gray")
abline(h = 20000, col = "red", lty = 2)
abline(h = 30000, col = "red", lty = 2)











plot(fit_GHZ)

checkresiduals(fit_GHZ)



##############

GHZ_BS15 <- read_excel("GHZ_BS15.xlsx")
GHZ_BS15 <- as.data.frame(GHZ_BS15)
GHZ_BS15$DATE <- as.Date(GHZ_BS15$DATE, format = "%d/%m/%Y")

GHZ_thucte <- GHZ_BS15 |> subset(DATE <= as.Date("2024-03-15"))

GHZ_thucte <- rbind(GHZ_thucte, GHZ[1, ])

par(mar = c(5, 5, 5, 5))
par(mgp = c(4, 1, 0))

forcast_data(ketqua_GHZ, xaxt = "n", yaxt = "n",
             showgap = FALSE,
             main = "GHZ",
             # shaded = FALSE,
             # pi.col = "purple",
             shadecols = "gray80",
             xlab = "DATE",
             ylab = "PRICE",
             fcol = "blue",
             las = 2,
             xlim = c(as.numeric(as.Date("2024-02-27")), as.numeric(as.Date("2024-03-15"))),
             ylim = c(10000, 30000))


labels_ok <- seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "days")

axis(side = 1, 
     at = c(seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "days"), as.Date(all_date)[length(all_date)]),
     labels = c(format(labels_ok, format="%Y\n%m-%d"), 
                format(as.Date(all_date)[length(all_date)], format="%Y\n%m-%d")),
     # col.ticks = NA,
     col.axis = NA,
     # col = NA
     # line = 2
)

axis(side = 1, 
     at = c(seq(as.Date(all_date)[1], as.Date(all_date)[length(all_date)], by = "days"), as.Date(all_date)[length(all_date)]),
     labels = c(format(labels_ok, format="%Y\n%m-%d"), 
                format(as.Date(all_date)[length(all_date)], format="%Y\n%m-%d")),
     col.ticks = NA,
     col.axis = "black",
     cex.axis = 1,
     col = NA,
     line = 1)

abline(v = as.Date("2024-02-29"), col = "gray")
abline(v = as.Date("2024-03-15"), col = "gray")

abline(h = ketqua_GHZ$mean[length(ketqua_GHZ$mean)], 
       col = "blue", lty = 2)

abline(h = ketqua_GHZ$upper[nrow(ketqua_GHZ$upper), 2], 
       col = "blue", lty = 2)
abline(h = ketqua_GHZ$lower[nrow(ketqua_GHZ$lower), 2], 
       col = "blue", lty = 2)

axis(side = 2,
     at = round(c(10000, 12000, 14000, 16000, 18000,
                  ketqua_GHZ$lower[nrow(ketqua_GHZ$lower), 2],
                  20000, 22000, 24000, ketqua_GHZ$upper[nrow(ketqua_GHZ$upper), 2],
                  28000, 30000), digits = 0),
     las = 2)

points(GHZ_thucte$PRICE ~ GHZ_thucte$DATE,
       type = "o",
       col = "red",
       pch = 19)

legend(x = "top",
       y = NULL,
       legend = c("GHZ actual", "GHZ predict"),
       col = c("red", "blue"),
       lwd = 2,
       horiz = TRUE,
       bty = "n")


##########################

unclass(ketqua_GHZ$mean) -> dudoan_GHZ_check
attributes(dudoan_GHZ_check)$tsp[1:2] -> date_GHZ_check

date_begin <- as.POSIXct.numeric(date_GHZ_check[1] * 24 * 3600)
date_end <- as.POSIXct.numeric(date_GHZ_check[2] * 24 * 3600)
date_begin <- as.Date(date_begin)
date_end <- as.Date(date_end)

date_GHZ_check_ok <- seq(from = date_begin, to = date_end, by = "days")

df_dudoan_GHZ <- data.frame(DATE = date_GHZ_check_ok, PRICE = dudoan_GHZ_check)

GHZ_compare <- merge(x = df_dudoan_GHZ,
                     y = GHZ_thucte,
                     by = "DATE",
                     all = TRUE)

names(GHZ_compare)[2:3] <- c("PRICE_dudoan", "PRICE_thucte")

GHZ_compare

GHZ_compare_clean <- na.omit(GHZ_compare)

GHZ_compare_clean

shapiro.test(GHZ_compare_clean$PRICE_thucte)
shapiro.test(GHZ_compare_clean$PRICE_dudoan)
var.test(GHZ_compare_clean$PRICE_thucte, GHZ_compare_clean$PRICE_dudoan)

t.test(x = GHZ_compare_clean$PRICE_thucte,
       y = GHZ_compare_clean$PRICE_dudoan,
       paired = TRUE,
       var.equal = FALSE,
       conf.level = 0.95,
       alternative = "two.sided"
)


d <- abs(GHZ_compare_clean$PRICE_thucte - GHZ_compare_clean$PRICE_dudoan)

t.test(d, mu = 0,
       conf.level = 0.95,
       alternative = "two.sided"
)

#################

library(quantmod)

# AAPL (Apple Corporation)
quantmod::getSymbols("AAPL",
           src = 'yahoo',
           from = as.Date('2021-01-01'))

# saveRDS(AAPL, "AAPL.rds")
AAPL <- readRDS("AAPL.rds")


quantmod::chartSeries(AAPL, 
                      theme = 'white',
                      type = c("auto", "matchsticks"), 
                      subset = '2021-01::', # define a time range subset
                      show.grid = TRUE,
                      major.ticks='auto', 
                      minor.ticks=TRUE,
                      multi.col = FALSE,
                      TA=c(addMACD(),
                           addVo(),
                           addSMA(n=200,col = 'blue'), # add moving average of ‘n’ periods
                           addSMA(n=50,col = 'red'),
                           addSMA(n=22,col = 'green'),
                           addROC(n=200,col = 'blue'), # Add Rate Of Change indicator to chart
                           addROC(n=50,col = 'red'),
                           addROC(n=22,col = 'green')) # a vector of technical indicators and params, or character strings
)
































