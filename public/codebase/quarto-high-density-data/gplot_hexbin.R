grid_hexlegend <- function (legend, title_legend, ysize, lcex, inner, style = "colorscale", minarea = 0.05, 
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
    grid.text(title_legend, legend/2, (n + 1.5) * spacing, default.units = leg.unit, 
              gp = gpar(cex = 1 * lcex, font = 2, col = "red"))
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
    grid.text(title_legend, legend/2, (n + 2) * inc, default.units = leg.unit, 
              gp = gpar(cex = 1 * lcex, font = 2, col = "red"))
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

###


gplot_hexbin <- function (x, style = "colorscale", 
                          title_legend, legend = 1.2, lcex = 1, minarea = 0.04, 
          maxarea = 0.8, mincnt = 1, maxcnt = max(x@count), trans = NULL, 
          inv = NULL, colorcut = seq(0, 1, length = min(17, maxcnt)), 
          border = NULL, density = NULL, pen = NULL, colramp = function(n) LinGray(n, 
                                                                                   beg = 90, end = 15), xlab = NULL, ylab = NULL, main = "", 
          newpage = TRUE, type = c("p", "l", "n"), xaxt = c("s", "n"), 
          yaxt = c("s", "n"), clip = "on", verbose = getOption("verbose")) 
{
  if (!is(x, "hexbin")) 
    stop("first argument must be a hexbin object")
  if (minarea < 0) 
    stop("Minimum area must be non-negative")
  if (maxarea > 1) 
    warning("Maximum area should be <= 1 this leads to overlapping hexagons")
  if (minarea > maxarea) 
    stop("Minarea must be <= maxarea")
  if (length(colorcut) > 1) {
    if (colorcut[1] != 0) 
      stop("Colorcut lower boundary must be 0")
    if (colorcut[length(colorcut)] != 1) 
      stop("Colorcut upper boundary must be 1")
  }
  else {
    colorcut <- if (colorcut > 1) 
      seq(0, 1, length = min(c(17, colorcut, maxcnt)))
    else 1
  }
  if (is.logical(legend)) {
    if (legend) 
      stop("Give the legend width")
    else legend <- 0
  }
  else stopifnot(is.numeric(legend) && length(legend) == 1)
  type <- match.arg(type)
  xaxt <- match.arg(xaxt)
  yaxt <- match.arg(yaxt)
  if (newpage) 
    grid::grid.newpage()
  hv.ob <- hexViewport(x, xbnds = x@xbnds, ybnds = x@ybnds, 
                       offset = unit(legend, "inches"))
  pushViewport(hv.ob@hexVp.off)
  grid::grid.rect()
  if (xaxt != "n") 
    grid.xaxis()
  if (yaxt != "n") 
    grid.yaxis()
  if (is.null(xlab)) 
    xlab <- x@xlab
  if (is.null(ylab)) 
    ylab <- x@ylab
  if (nchar(xlab) > 0) 
    grid.text(xlab, y = unit(-3, "lines"), gp = gpar(fontsize = 16))
  if (nchar(ylab) > 0) 
    grid.text(ylab, x = unit(-3, "lines"), gp = gpar(fontsize = 16), 
              rot = 90)
  if (nchar(main) > 0) 
    grid.text(main, y = unit(1, "npc") + unit(1.5, "lines"), 
              gp = gpar(fontsize = 18))
  if (type != "n") {
    if (clip == "on") {
      upViewport()
      pushViewport(hv.ob@hexVp.on)
    }
    grid.hexagons(x, style = style, minarea = minarea, maxarea = maxarea, 
                  mincnt = mincnt, maxcnt = maxcnt, check.erosion = FALSE, 
                  trans = trans, colorcut = colorcut, density = density, 
                  border = border, pen = pen, colramp = colramp, verbose = verbose)
  }
  upViewport()
  if (legend > 0) {
    if (!is.null(trans) && is.null(inv)) 
      stop("Must supply the inverse transformation")
    if (verbose) 
      cat("plot.hexbin( legend > 0):  ... hex.legend()\n")
    inner <- hexbin:::getPlt(hv.ob, ret.unit = "inches", numeric = TRUE)[1]/x@xbins
    ysize <- hexbin:::getPlt(hv.ob, ret.unit = "inches", numeric = TRUE)[2]
    legVp <- viewport(x = unit(1, "npc") - convertX(unit(legend, 
                                                         "inches"), "npc"), y = hv.ob@mar[1], height = unit(1, 
                                                                                                            "npc") - (hv.ob@mar[1] + hv.ob@mar[3]), width = convertUnit(unit(legend, 
                                                                                                                                                                             "inches"), "npc"), default.units = "native", just = c("left", 
                                                                                                                                                                                                                                   "bottom"), xscale = c(0, legend), yscale = c(0, ysize))
    if (type != "n") {
      pushViewport(legVp)
      grid_hexlegend(legend, title_legend, ysize = ysize, lcex = lcex, 
                     inner = inner, style = style, minarea = minarea, 
                     maxarea = maxarea, mincnt = mincnt, maxcnt = maxcnt, 
                     trans = trans, inv = inv, colorcut = colorcut, 
                     density = density, border = border, pen = pen, 
                     colramp = colramp)
      upViewport()
    }
  }
  invisible(list(plot.vp = hv.ob, legend.vp = if (legend) legVp))
}

###

