function (legend, ysize, lcex, inner, style = "colorscale", minarea = 0.05, 
          maxarea = 0.8, mincnt = 1, maxcnt, trans = NULL, inv = NULL, 
          colorcut, density = NULL, border = NULL, pen = NULL, colramp = function(n) {
            LinGray(n, beg = 90, end = 15)
          }, leg.unit = "native") 
{
  style <- match.arg(style, eval(formals(grid.hexagons)[["style"]]))
  if (style %in% c("centroids", "lattice", "colorscale")) {
    if (is.null(trans)) {
      sc <- maxcnt - mincnt
      bnds <- round(mincnt + sc * colorcut)
    }
    else {
      if (!is.function(trans) && !is.function(inv)) 
        stop("'trans' and 'inv' must both be functions if 'trans' is not NULL")
      con <- trans(mincnt)
      sc <- trans(maxcnt) - con
      bnds <- round(inv(con + sc * colorcut))
    }
  }
  if (style == "colorscale") {
    n <- length(bnds)
    spacing <- ysize/(n + 3)
    inner <- min(legend/3.5, (sqrt(3) * spacing)/2)
  }
  dx <- inner/2
  dy <- dx/sqrt(3)
  hexC <- hexcoords(dx, dy, n = 1, sep = NULL)
  switch(style, colorscale = {
    midx <- legend/3
    textx <- (2 * legend)/3
    tx <- hexC$x + midx
    pen <- colramp(n)
    for (i in seq(length = n - 1)) {
      grid.polygon(tx, hexC$y + i * spacing, default.units = leg.unit, 
                   id = NULL, id.lengths = 6, gp = gpar(fill = pen[i], 
                                                        col = border))
      grid.text(as.character(bnds[i]), textx, (i - 0.5) * 
                  spacing, default.units = leg.unit, gp = gpar(cex = lcex))
    }
    grid.text(as.character(bnds[n]), textx, (n - 0.5) * spacing, 
              default.units = leg.unit, gp = gpar(cex = lcex))
    grid.text("Counts", legend/2, (n + 1.5) * spacing, default.units = leg.unit, 
              gp = gpar(cex = 1.7 * lcex))
  }, centroids = , lattice = {
    radius <- sqrt(minarea + (maxarea - minarea) * colorcut)
    n <- length(radius)
    shift <- c(0, 2 * dy * radius)
    shift <- shift[1:n] + shift[2:(n + 1)]
    labht <- convertY(unit(get.gpar(names = "fontsize")[[1]] * 
                             lcex, "points"), "native", valueOnly = TRUE)
    shift <- pmax(labht, shift)
    six <- rep.int(6:6, n)
    xmid <- legend/3
    inc <- ysize/(n + 3)
    if (inc > max(shift)) y <- inc * 1:n else {
      y <- cumsum(shift)
      extra.slop <- (n * inc) - y[n]
      shift[-1] <- shift[-1] + extra.slop/(n - 1)
      y <- cumsum(shift)
    }
    textx <- rep.int((2 * legend)/3, n)
    if (is.null(pen)) pen <- 1
    if (is.null(border)) border <- pen
    grid.polygon(x = rep.int(hexC$x, n) * rep.int(radius, 
                                                  six) + rep.int(xmid, 6 * n), y = rep.int(hexC$y, 
                                                                                           n) * rep.int(radius, six) + rep.int(y, six), default.units = leg.unit, 
                 id = NULL, id.lengths = rep.int(6, n), gp = gpar(fill = pen, 
                                                                  col = border))
    grid.text(as.character(bnds), textx, y, default.units = leg.unit, 
              gp = gpar(cex = lcex))
    grid.text("Counts", legend/2, (n + 2) * inc, default.units = leg.unit, 
              gp = gpar(cex = 1.7 * lcex))
  }, nested.lattice = , nested.centroids = {
    numb <- cut(floor(legend/inner), breaks = c(-1, 0, 2, 
                                                4))
    if (is.na(numb)) numb <- 4
    switch(numb, {
      warning("not enough space for legend")
      return()
    }, size <- 5, size <- c(1, 5, 9), size <- c(1, 3, 5, 
                                                7, 9))
    xmax <- length(size)
    radius <- sqrt(minarea + (maxarea - minarea) * (size - 
                                                      1)/9)
    txt <- as.character(size)
    lab <- c("Ones", "Tens", "Hundreds", "Thousands", "10 Thousands", 
             "100 Thousands", "Millions", "10 Millions", "100 Millions", 
             "Billions")
    power <- floor(log10(maxcnt)) + 1
    yinc <- 16 * dy
    if (ysize/power < yinc) {
      warning("Not enough height for legend")
      return()
    }
    xmid <- legend/10
    x <- inner * (1:xmax - (1 + xmax)/2) + xmid
    n <- length(x)
    tx <- rep.int(hexC$x, n)
    ty <- rep.int(hexC$y, n)
    six <- rep.int(6:6, n)
    y <- rep.int(3 * dy - yinc, xmax)
    if (is.null(pen)) {
      pen <- 1:power + 1
      pen <- cbind(pen, pen + 10)
    }
    if (is.null(border)) border <- FALSE
    for (i in 1:power) {
      y <- y + yinc
      hexpolygon(x, y, hexC, col = pen[i, 1], border = border)
      grid.polygon(x = tx * rep.int(radius, six) + rep.int(x, 
                                                           six), y = ty * rep.int(radius, six) + rep.int(y, 
                                                                                                         six), default.units = leg.unit, id = NULL, id.lengths = rep(6, 
                                                                                                                                                                     n), gp = gpar(fill = pen[i, 2], col = border))
      grid.text(txt, x, y - 4.5 * dy, default.units = leg.unit, 
                gp = gpar(cex = lcex))
      grid.text(lab[i], xmid, y[1] + 4.5 * dy, default.units = leg.unit, 
                gp = gpar(cex = 1.7 * lcex))
    }
  })
}