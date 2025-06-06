function (x, style = "colorscale", legend = 1.2, lcex = 1, minarea = 0.04, 
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
    grid.newpage()
  hv.ob <- hexViewport(x, xbnds = x@xbnds, ybnds = x@ybnds, 
                       offset = unit(legend, "inches"))
  pushViewport(hv.ob@hexVp.off)
  grid.rect()
  if (xaxt != "n") 
    grid.xaxis()
  if (yaxt != "n") 
    grid.yaxis()
  if (is.null(xlab)) 
    xlab <- x@xlab
  if (is.null(ylab)) 
    ylab <- x@ylab
  if (nchar(xlab) > 0) 
    grid.text(xlab, y = unit(-2, "lines"), gp = gpar(fontsize = 16))
  if (nchar(ylab) > 0) 
    grid.text(ylab, x = unit(-2, "lines"), gp = gpar(fontsize = 16), 
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
    inner <- getPlt(hv.ob, ret.unit = "inches", numeric = TRUE)[1]/x@xbins
    ysize <- getPlt(hv.ob, ret.unit = "inches", numeric = TRUE)[2]
    legVp <- viewport(x = unit(1, "npc") - convertX(unit(legend, 
                                                         "inches"), "npc"), y = hv.ob@mar[1], height = unit(1, 
                                                                                                            "npc") - (hv.ob@mar[1] + hv.ob@mar[3]), width = convertUnit(unit(legend, 
                                                                                                                                                                             "inches"), "npc"), default.units = "native", just = c("left", 
                                                                                                                                                                                                                                   "bottom"), xscale = c(0, legend), yscale = c(0, ysize))
    if (type != "n") {
      pushViewport(legVp)
      grid.hexlegend(legend, ysize = ysize, lcex = lcex, 
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











