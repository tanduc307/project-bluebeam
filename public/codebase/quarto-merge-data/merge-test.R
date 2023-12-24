library(readxl)
df_1 <- read_excel("df_1.xlsx")
df_2 <- read_excel("df_2.xlsx")
df_3 <- read_excel("df_3.xlsx")

df_1$country_year <- paste0(df_1$country, "-", df_1$year)

identical(dim(df_1)[1], length(unique(df_1$country_year)))

df_2$country_year <- paste0(df_2$country, "-", df_2$year)

identical(dim(df_2)[1], length(unique(df_2$country_year)))

merge(x = df_1,
      y = df_2,
      by = "country_year",
      all = TRUE) -> df_1_2

merge(x = df_1,
      y = df_2,
      by = "country_year",
      all.x = TRUE)

merge(x = df_1,
      y = df_2,
      by = "country_year",
      all.y = TRUE)

df_3$country_year <- paste0(df_3$country, "-", df_3$year)

identical(dim(df_3)[1], length(unique(df_3$country_year)))

merge(x = df_1_2,
      y = df_3,
      by = "country_year",
      all = TRUE) -> df_1_2_3


df_1_2_3_clean <- df_1_2_3[ , c("country_year", "var_1", "var_2", "var_3", "var_4", "var_5")]


strsplit(df_1_2_3_clean$country_year, split = "-") -> ok

do.call(rbind, ok) -> ok_1

ok_1

df_1_2_3_clean$country <- ok_1[, 1]

df_1_2_3_clean$year <- ok_1[, 2]

df_1_2_3_final <- df_1_2_3_clean[ , c("country_year", "country", "year", "var_1", "var_2", "var_3", "var_4", "var_5")]







library(eulerr)
fit <- euler(c(A = 200, 
               B = 150, 
               C = 60, 
               "A&B" = 30,
               "A&C" = 30,
               "B&C" = 30,
               "A&B&C" = 30))

eulerr:::plot.euler(fit)


library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit)

library(grid)

library(gridExtra)
grid.pattern(pattern = 1, granularity = unit(2.5, "mm")) ## (Default is 5 mm)



grid.ls() 

grid.edit(gPath = "fills.grob.1", gp = list(fill = "green", angle = 45, density = 30),
          redraw = TRUE,
          global = TRUE)

yes <- grid.get("diagram.grob.1")

yes$children$fills.grob.1$gp$fill <- "red"

grid::gTree(yes)

# angle=c(45, 0, 135, 90), density=seq(10,30,15)

vdist <- 5 * sqrt(2)

nLines <- 8L    ## can be any integer
panelHeight <- list(x = nLines*vdist, units = "mm", data = NULL)

## Plot it
print(examplePlot, panel.height=panelHeight)

## Calculate vertical distance (in mm) between 45 degree diagonal lines
## spaced 5mm apart (the default distance for grid.pattern).
vdist <- 5 * sqrt(2)

nLines <- 8L    ## can be any integer
panelHeight <- list(x = nLines*vdist, units = "mm", data = NULL)

## Plot it
print(examplePlot, panel.height=panelHeight)






install.packages("gridpattern")
library(gridpattern)
library(gridExtra)
grid.pattern(pattern = 1, granularity = unit(2.5, "mm")) ## (Default is 5 mm)


grid.newpage()
grid.pattern(x=seq(1/6, 5/6, length=6), width=unit(1/8,"npc"),
             height=unit(0.5,"npc"),
             motif.width=unit(10, "mm"),  pattern=c(1:6),
             orientation=45, motif.alpha=0.5,
             motif.cex=c(1, 0.5), motif.col=1:2, motif.fill=NA,
             gp=gpar(fill="blue", lwd=2, alpha=0.5),  clip=T)



library(gridExtra)
grid.pattern(pattern = 1)

?grid.pattern

?mean

grid.ls(viewports = TRUE)

library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit)

grid.pattern(
  pattern = "stripe",
  # x = c(0, 0, 1, 1),
  # y = c(1, 0, 0, 1),
  # id = 1L,
  # ...,
  # legend = FALSE,
  # prefix = "pattern_",
  # default.units = "npc",
  # name = NULL,
  # gp = gpar(),
  draw = TRUE,
  vp = "euler.vp"
)

yes <- grid.get("GRID.pattern.54")
str(yes)

library(ggplot2)
library(ggpattern)

df1 <- data.frame(
  x    = rep(1:6, 9),
  y    = rep(1:9, each=6),
  name = gridpattern::names_magick,
  stringsAsFactors = FALSE
)

ggplot(df1) + 
  geom_tile_pattern(
    aes(x, y, pattern_type = I("vertical")),
    pattern       = 'magick',
    pattern_scale = 1.5,
    pattern_fill  = 'white', 
    width         = 0.9, 
    height        = 0.9
  ) + 
  geom_label(aes(x+0.4, y+0.4, label = name), hjust = 1, vjust = 1) + 
  theme_void() + 
  labs(
    title = "All the possible magick pattern names"
  ) +
  coord_fixed(1)

library(patternplot)
library(png)
library(ggplot2)
data <- read.csv(system.file("extdata", "vegetables.csv", package="patternplot"))
#Example 1
pattern.type<-c('hdashes', 'vdashes', 'bricks')
pie1<-patternpie(group=data$group,pct=data$pct,label=data$label, 
                 label.size=4, label.color='black',label.distance=1.3,pattern.type=pattern.type,
                 pattern.line.size=c(10, 10, 2), frame.color='black',frame.size=1.5, pixel=12, density=c(8, 8, 10))
pie1

?patternpie

#Example 2
pattern.color<-c('red3','green3', 'white' )
background.color<-c('dodgerblue', 'lightpink', 'orange')
pie2<-patternpie(group=data$group,pct=data$pct,label=data$label, label.distance=1.3, pattern.type=pattern.type,
                 pattern.color=pattern.color,background.color=background.color, 
                 pattern.line.size=c(10, 10, 2), frame.color='grey40',frame.size=1.5, pixel=12, density=c(8, 8, 10))
pie2<-pie2+ggtitle('(B) Colors with Patterns')

library(gridExtra)
grid.arrange(pie1,pie2,  nrow = 1)



windows(8, 8)

library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit, n = 10L)

grid.lines(x = unit(c(0, 1), "npc"),
           y = unit(c(0, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)


grid.lines(x = unit(c(0, 0.8), "npc"),
           y = unit(c(0.2, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0, 0.6), "npc"),
           y = unit(c(0.4, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0, 0.4), "npc"),
           y = unit(c(0.6, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)

###

grid.lines(y = unit(c(0, 0.8), "npc"),
           x = unit(c(0.2, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)

grid.lines(y = unit(c(0, 0.6), "npc"),
           x = unit(c(0.4, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)

grid.lines(y = unit(c(0, 0.4), "npc"),
           x = unit(c(0.6, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(), draw = TRUE, vp = NULL)



grid.newpage(FALSE)

grid.ls()

line_1 <- grid.get("GRID.lines.1433")

dput(line_1)

grob_ok$id.lengths

grob_ok$gp

grob_ok$x

grob_ok$y


grid.add("grob_ok")


venn_1 <- grid.get("euler.diagram")

?grid.add









line_pattern_1 <- structure(list(x = structure(c(0, 1), unit = 0L, class = c("simpleUnit", 
"unit", "unit_v2")), y = structure(c(0, 1), unit = 0L, class = c("simpleUnit", 
"unit", "unit_v2")), arrow = NULL, name = "GRID.lines.1433", 
gp = structure(list(), class = "gpar"), vp = NULL), class = c("lines", 
"grob", "gDesc"))





grid.add(gPath = "euler.diagram", 
         child = line_pattern_1, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)


##################################


library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit, n = 200L)


grid.ls()
check_1 <- grid.get("edges.grob")


grid.remove("edges.grob")
check_1$x
check_1$y

line_pattern_1 <- structure(list(x = structure(c(-0.8, 0.3), unit = 0L, class = c("simpleUnit", 
"unit", "unit_v2")), y = structure(c(-2.9, 0.5), unit = 0L, class = c("simpleUnit", 
"unit", "unit_v2")), arrow = NULL, name = "GRID.lines.1433", 
gp = structure(list(), class = "gpar"), vp = NULL), class = c("lines", 
"grob", "gDesc"))

grid.add(gPath = "euler.diagram", 
         child = line_pattern_1, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)












grid.add(gPath = "euler.diagram", 
         child = venn_1, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)


dput(check_1$x)











###################
windows(8, 8, rescale = "fixed")
library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit, n = 200L)

grid.ls()

venn_ok <- grid.get("euler.diagram")

venn_ok$vp$gp$fill <- "yellow"


venn_ok$children$canvas.grob$gp$fill <- "green"

par(pty = "s")
par(xpd = TRUE)
par(oma = c(2, 2, 2, 2))
par(mar = c(2, 2, 2, 2))
par(xaxs = "i")
par(yaxs = "i")
par(bg = "green")
par("pin") # in
par("din") # in
par("usr")
par("plt") # percent
par("bty" = "n") # box
dev.size(units = "in")

# lấy tọa độ user trên nền base R
plot(1:10, 
     type = "n",
     xaxt = "n",
     yaxt = "n")

par(new = TRUE)

grid.rect(gp=gpar(fill="red", alpha=0.1))



grid.add(gPath = "euler.diagram", 
         child = venn_ok, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)


venn_ok$children$canvas.grob$children$diagram.grob.1$children$fills.grob.1$gp$fill




























par(pty = "s")
par(xpd = TRUE)
par(oma = c(2, 2, 2, 2))
par(mar = c(2, 2, 2, 2))
par(xaxs = "i")
par(yaxs = "i")
par("pin") # in
par("din") # in
par("usr")
par("plt") # percent
par("bty" = "n") # box
dev.size(units = "in")

# lấy tọa độ user trên nền base R
plot(1:10, 
     type = "n",
     xaxt = "n",
     yaxt = "n")

library(eulerr)
library(grid)

fit <- euler(c(A = 200, 
               B = 150, 
               C = 60, 
               "A&B" = 30,
               "A&C" = 30,
               "B&C" = 30,
               "A&B&C" = 30))

# thực vẽ trên nền grid
eulerr:::plot.euler(fit, 
                    fills = c("red", "blue", "yellow"), 
                    labels = c("A", "B", "C"),
                    legend = list(labels = c("A", "B", "C")),
                    # main = "Những mảnh ghép cần thiết",
                    quantities = TRUE, 
                    alpha = 0.5)

title(main = "Thay đổi rất nhẹ data\nđể đảm bảo\nkhông có gì sai", adj = 0, col.main = "darkblue")

title(main = "Bước 2:\nRáp code\nđúng theo hướng dẫn\ndefault value", adj = 1, col.main = "red")

box(which = "plot", lty = 2, col = "gray")
box(which = "figure", lty = 2, col = "gray")
box(which = "outer", lty = 1, col = "black")


abline(h = (par("usr")[1] + par("usr")[2])/2, 
       v = (par("usr")[3] + par("usr")[4])/2, 
       lty = 2, 
       col = "blue")

# legend(x = "top",
#        y = NULL,
#        legend = c("A", "B", "C"),
#        fill =  c("red", "blue", "yellow"))

mtext(text = "Đồ thị được plot overlay giữa base và grid graphics\nnhằm sử dụng được cùng lúc các nhóm function của hai hệ thống", side = 1, font = 3, col = "darkblue")







#####################

windows(8, 8, rescale = "fixed")
library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit, n = 200L)

grid.rect(gp=gpar(fill="red", alpha=0.9))

grid.ls()

venn_ok <- grid.get("euler.diagram")

venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.1$gp$fill <- "black"

grid.add(gPath = "euler.diagram", 
         child = venn_ok, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)




################################

library(eulerr)
fit <- euler(c(df_1 = 20, 
               df_2 = 20, 
               "df_1&df_2" = 10))
eulerr:::plot.euler(fit, n = 200L)

grid.lines(x = unit(c(0, 1), "npc"),
           y = unit(c(0, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)


grid.lines(x = unit(c(0, 0.8), "npc"),
           y = unit(c(0.2, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0, 0.6), "npc"),
           y = unit(c(0.4, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

grid.lines(x = unit(c(0, 0.4), "npc"),
           y = unit(c(0.6, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

###

grid.lines(y = unit(c(0, 0.8), "npc"),
           x = unit(c(0.2, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

grid.lines(y = unit(c(0, 0.6), "npc"),
           x = unit(c(0.4, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

grid.lines(y = unit(c(0, 0.4), "npc"),
           x = unit(c(0.6, 1), "npc"),
           default.units = "npc",
           arrow = NULL, name = NULL,
           gp=gpar(col = "white", alpha = 0.8, lwd = 2), draw = TRUE, vp = NULL)

######

venn_ok <- grid.get("euler.diagram")

venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.1$gp$fill <- "gray"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.1$gp$col <- "black"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.1$gp$alpha <- 0.8

venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.2$gp$fill <- "gray"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.2$gp$col <- "black"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.2$gp$alpha <- 0.8

venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.3$gp$fill <- "gray"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.3$gp$col <- "black"
venn_ok $children$canvas.grob$children$diagram.grob.1$children$fills.grob.3$gp$alpha <- 0.8

grid.add(gPath = "euler.diagram", 
         child = venn_ok, 
         strict = FALSE, grep = FALSE,
         global = FALSE, allDevices = FALSE, redraw = TRUE)


# grid.remove("fills.grob.2")


?eulerr:::plot.euler


















