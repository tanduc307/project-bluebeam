bins_data <- hexbin::hexbin(x = c(Galton$parent),
                            y = c(Galton$child),
                            xbnds = c(62, 75),
                            ybnds = c(60, 75),
                            xbins = 20,
                            shape = 1)

gplot_hexbin(x = bins_data,
             title_legend = "ok")
