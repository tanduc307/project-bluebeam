library(pls)
library(kableExtra)

t(gasoline[, -c(1, 2)]) -> matrix_gasoline

row.names(matrix_gasoline)

strsplit(row.names(matrix_gasoline), split = "NIR")

gsub("\\.nm", "", row.names(matrix_gasoline)) -> ok_1

gsub("NIR.", "", ok_1) -> ok_2

as.numeric(ok_2)

#############

t(gasoline[, -c(1, 2)]) -> matrix_gasoline

gsub("\\.nm", "", row.names(matrix_gasoline)) -> ok_1

gsub("NIR.", "", ok_1) -> ok_2

as.numeric(ok_2) -> row.names(matrix_gasoline)

paste0("octane.", gasoline$octane) -> colnames(matrix_gasoline)



matrix_gasoline %>%
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", 
                                      "hover", 
                                      "condensed", 
                                      "bordered", 
                                      "responsive")) %>%
  kable_classic(full_width = FALSE, html_font = "arial") -> matrix_gasoline_output

save_kable(matrix_gasoline_output, file = "matrix_gasoline_output.html")

########

asd = data.frame(a = format(c(4324322,45454354),big.mark=",",scientific=FALSE))
class(asd$a)


asd <- format(data.frame(a = c(4324322,45454354)),big.mark=",",scientific=FALSE)
class(asd$a)



gasoline

gasoline$NIR

class(gasoline$NIR)
unclass(gasoline$NIR)


# https://stackoverflow.com/questions/72870381/create-a-new-data-frame-with-asis-elements


# That "AsIs" means we have to use I() to protect data frame columns. Such protection is a must, if we want to have a matrix or a list, rather than a vector, in a data frame column.

x <- 1:4

Y <- matrix(5:12, nrow = 4, dimnames = list(LETTERS[1:4], letters[1:2]))

Z <- matrix(13:20, nrow = 4)


wrong <- data.frame(x = x, Y = Y, Z = Z)
View(wrong)
correct <- data.frame(x = x, Y = I(Y), Z = I(Z))

View(correct)
class(correct$Y)
names(correct)
class(correct)

gasoline
View(gasoline)

#####################

# df_ok <- read.csv("D:/learnr/multiregression/df_ok.csv")

A_REP2 <- read.csv("D:/learnr/multiregression/A_REP2.csv")

A_REP2 |> dplyr::arrange(WL.nm.) -> A_REP2

# View(A_REP2)

# A_REP2$WL.nm.

t(A_REP2[, -1]) -> A_REP2_ok

# dim(A_REP2)
# 
# dim(A_REP2_ok)
# 
# View(A_REP2_ok)
# 
# length(A_REP2$WL.nm.)

# paste0("WL.nm.", A_REP2$WL.nm.)

# colnames(A_REP2_ok) <- paste0("WL.nm.", A_REP2$WL.nm.)

colnames(A_REP2_ok) <- A_REP2$WL.nm.

rownames(A_REP2_ok) -> x

# A_REP2_ok
# 
# class(A_REP2_ok)
# 
# mode(A_REP2_ok)

A_REP2_working <- data.frame(ky_hieu_mau = x, ket_qua = I(A_REP2_ok))

# View(A_REP2_working)
# 
# class(A_REP2_working)

######
C_REP2 <- read.csv("D:/learnr/multiregression/C_REP2.csv")

# View(C_REP2)

t(C_REP2) -> C_REP2_ok

C_REP2_ok[-1, ] -> C_REP2_ok_1

colnames(C_REP2_ok_1) <- C_REP2[, 1] 

C_REP2_ok_1 <- as.data.frame(C_REP2_ok_1)

C_REP2_ok_1$code_thi_nghiem <- rownames(C_REP2_ok_1)

rownames(C_REP2_ok_1) <- NULL

C_REP2_ok_1$CAF <- as.numeric(C_REP2_ok_1$CAF)

C_REP2_ok_1$PHE <- as.numeric(C_REP2_ok_1$PHE)


A_REP2_working$CAF <- C_REP2_ok_1$CAF

A_REP2_working$PHE <- C_REP2_ok_1$PHE

A_REP2_working$mean_CAF_PHE <- (A_REP2_working$CAF + A_REP2_working$PHE) / 2


View(A_REP2_working)

A_REP2_final <- A_REP2_working[, -c(1, 3, 4)]

dim(A_REP2_working)

View(A_REP2_final)

rownames(A_REP2_final) <- NULL

A_REP2_final[, c(2, 1)] -> A_REP2_final

A_REP2_working$ky_hieu_mau

gasTrain <- A_REP2_final[1:20, ]
gasTest <- A_REP2_final[21:25, ]

View(gasTrain)



#####################
gas1 <- plsr(mean_CAF_PHE ~ ket_qua, ncomp = 10, data = A_REP2_final, 
             validation = "LOO")

summary(gas1)


###########################

# data(gasoline) ## vì các function bên dưới dùng cột NIR ở dạng AsIs nên ta gọi lại dataset này.
# 
# gasTrain <- gasoline[1:50, ]
# 
# gasTest <- gasoline[51:60, ]
# 
# View(gasTrain)
# 
# View(A_REP2_working)


plot(RMSEP(gas1), legendpos = "topright")

plot(gas1, ncomp = 2, asp = 1, line = TRUE)

plot(gas1, plottype = "scores", comps = 1:3)

explvar(gas1)

plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")

predict(gas1, ncomp = 2, newdata = gasTest)


RMSEP(gas1, newdata = gasTest)




















##########

## real

View(gasTrain)
data(gasoline) ## vì các function bên dưới dùng cột NIR ở dạng AsIs nên ta gọi lại dataset này.

gasTrain <- gasoline[1:50, ]

gasTest <- gasoline[51:60, ]

# A typical way of fitting a PLSR model is

gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")

summary(gas1)

plot(RMSEP(gas1), legendpos = "topright")
plot(gas1, ncomp = 2, asp = 1, line = TRUE)

plot(gas1, plottype = "scores", comps = 1:3)
explvar(gas1)


plot(gas1, "loadings", comps = 1:2, legendpos = "topleft",
     labels = "numbers", xlab = "nm")


View(gas1)

unclass(gas1)
