library(hexbin)
library(RColorBrewer)
library(HistData)
data(Galton)
# source("hack.R")

## chuyển dataset về dạng bins

bins_data <- hexbin::hexbin(x = c(Galton$parent),
                            y = c(Galton$child),
                            xbnds = c(0, 100),
                            ybnds = c(0, 100))

# ok <- hex_bin(x = bins_data,
#         clip = "off",
#         xaxt = "s",
#         yaxt = "s",
#         colramp = colorRampPalette(terrain.colors(n = 30)))

plot(bins_data, border = 4)

hexbin:::gplot.hexbin(bins_data, border = 4)


smbin <- hexbin::smooth.hexbin(bins_data)
plot(smbin, border = 4,
     colramp = colorRampPalette(hcl.colors(n = 30)))
