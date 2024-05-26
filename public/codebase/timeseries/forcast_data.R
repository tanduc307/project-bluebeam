forcast_data <- function (x, include, PI = TRUE, showgap = TRUE, shaded = TRUE, 
                        shadebars = (length(x$mean) < 5), shadecols = NULL, col = 1, 
                        fcol = 4, pi.col = 1, pi.lty = 2, ylim = NULL, main = NULL, 
                        xlab = "", ylab = "", type = "l", flty = 1, flwd = 2, ...) 
{
  if (is.element("x", names(x))) {
    xx <- x$x
   }
  else {
    xx <- NULL
  }
  if (is.null(x$lower) || is.null(x$upper) || is.null(x$level)) {
    PI <- FALSE
  }
  else if (!is.finite(max(x$upper))) {
    PI <- FALSE
  }
  if (!shaded) {
    shadebars <- FALSE
  }
  if (is.null(main)) {
    main <- paste("Forecasts from ", x$method, sep = "")
  }
  if (PI) {
    x$upper <- as.matrix(x$upper)
    x$lower <- as.matrix(x$lower)
  }
  if (is.element("lm", class(x$model)) && !is.element("ts", 
                                                      class(x$mean))) {
    plotlmforecast(x, PI = PI, shaded = shaded, shadecols = shadecols, 
                   col = col, fcol = fcol, pi.col = pi.col, pi.lty = pi.lty, 
                   ylim = ylim, main = main, xlab = xlab, ylab = ylab, 
                   ...)
    if (PI) {
      return(invisible(list(mean = x$mean, lower = as.matrix(x$lower), 
                            upper = as.matrix(x$upper))))
    }
    else {
      return(invisible(list(mean = x$mean)))
    }
  }
  n <- length(xx)
  if (n == 0) {
    include <- 0
  }
  else if (missing(include)) {
    include <- length(xx)
  }
  if (n > 0) {
    if (sum(is.na(xx)) == length(xx)) {
      n <- 0
    }
  }
  if (n > 0) {
    xx <- as.ts(xx)
    freq <- frequency(xx)
    strt <- start(xx)
    nx <- max(which(!is.na(xx)))
    xxx <- xx[1:nx]
    include <- min(include, nx)
    if (!showgap) {
      lastObs <- x$x[length(x$x)]
      lastTime <- time(x$x)[length(x$x)]
      x$mean <- ts(c(lastObs, x$mean), start = lastTime, 
                   frequency = freq)
      x$upper <- ts(rbind(lastObs, x$upper), start = lastTime, 
                    frequency = freq)
      x$lower <- ts(rbind(lastObs, x$lower), start = lastTime, 
                    frequency = freq)
    }
  }
  else {
    freq <- frequency(x$mean)
    strt <- start(x$mean)
    nx <- include <- 1
    xx <- xxx <- ts(NA, frequency = freq, end = tsp(x$mean)[1] - 
                      1/freq)
    if (!showgap) {
      warning("Removing the gap requires historical data, provide this via model$x. Defaulting showgap to TRUE.")
    }
  }
  pred.mean <- x$mean
  if (is.null(ylim)) {
    ylim <- range(c(xx[(n - include + 1):n], pred.mean), 
                  na.rm = TRUE)
    if (PI) {
      ylim <- range(ylim, x$lower, x$upper, na.rm = TRUE)
    }
  }
  npred <- length(pred.mean)
  tsx <- is.ts(pred.mean)
  if (!tsx) {
    pred.mean <- ts(pred.mean, start = nx + 1, frequency = 1)
    type <- "p"
  }
  plot(ts(c(xxx[(nx - include + 1):nx], rep(NA, npred)), end = tsp(xx)[2] + 
            (nx - n)/freq + npred/freq, frequency = freq), xlab = xlab, 
       ylim = ylim, ylab = ylab, main = main, col = col, type = type, 
       ...)
  
  take_x <<- ts(c(xxx[(nx - include + 1):nx], rep(NA, npred)), end = tsp(xx)[2] + 
                 (nx - n)/freq + npred/freq, frequency = freq)
  
  unclass(take_x) -> num_x
  attributes(num_x)$tsp[1:2] -> date_ok
  date_begin <- as.POSIXct.numeric(date_ok[1] * 24 * 3600)
  date_end <- as.POSIXct.numeric(date_ok[2] * 24 * 3600)
  date_begin <- as.Date(date_begin)
  date_end <- as.Date(date_end)
  
  all_date <<- seq(from = date_begin, to = date_end, by = "days")
  
  dulieu_ts <- data.frame(DATE = all_date, PRICE = num_x)
  
  dulieu_ts <- dulieu_ts[1: (dim(dulieu_ts)[1] - dudoan), ]
  
  points(na.omit(dulieu_ts), col = "black", type = "l")
  
  if (PI) {
    if (is.ts(x$upper)) {
      xxx <- time(x$upper)
    }
    else {
      xxx <- tsp(pred.mean)[1] - 1/freq + (1:npred)/freq
    }
    idx <- rev(order(x$level))
    nint <- length(x$level)
    if (is.null(shadecols)) {
      if (min(x$level) < 50) {
        shadecols <- rev(colorspace::sequential_hcl(100)[x$level])
      }
      else {
        shadecols <- rev(colorspace::sequential_hcl(52)[x$level - 
                                                          49])
      }
    }
    if (length(shadecols) == 1) {
      if (shadecols == "oldstyle") {
        shadecols <- heat.colors(nint + 2)[switch(1 + 
                                                    (nint > 1), 2, nint:1) + 1]
      }
    }
    for (i in 1:nint) {
      if (shadebars) {
        for (j in 1:npred) {
          polygon(xxx[j] + c(-0.5, 0.5, 0.5, -0.5)/freq, 
                  c(rep(x$lower[j, idx[i]], 2), rep(x$upper[j, 
                                                            idx[i]], 2)), col = shadecols[i], border = FALSE)
        }
      }
      else if (shaded) {
        polygon(c(xxx, rev(xxx)), c(x$lower[, idx[i]], 
                                    rev(x$upper[, idx[i]])), col = shadecols[i], 
                border = FALSE)
      }
      else if (npred == 1) {
        lines(c(xxx) + c(-0.5, 0.5)/freq, rep(x$lower[, 
                                                      idx[i]], 2), col = pi.col, lty = pi.lty)
        lines(c(xxx) + c(-0.5, 0.5)/freq, rep(x$upper[, 
                                                      idx[i]], 2), col = pi.col, lty = pi.lty)
      }
      else {
        lines(x$lower[, idx[i]], col = pi.col, lty = pi.lty)
        lines(x$upper[, idx[i]], col = pi.col, lty = pi.lty)
      }
    }
  }
  if (npred > 1 && !shadebars && tsx) {
    lines(pred.mean, lty = flty, lwd = flwd, col = fcol)
  }
  else {
    points(pred.mean, col = fcol, pch = 19)
  }
  if (PI) {
    invisible(list(mean = pred.mean, lower = x$lower, upper = x$upper))
  }
  else {
    invisible(list(mean = pred.mean))
  }
}