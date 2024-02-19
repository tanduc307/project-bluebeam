df <- read.csv("students.csv")
df
head(df)

boxplot(x = df$age, 
        # data = df,
        col = "yellow",
        outline = FALSE)

boxplot(formula = age ~ gender, 
        data = df,
        col = "yellow",
        outline = FALSE)

boxplot(formula = age ~ gender, 
        data = df,
        col = "yellow",
        outline = FALSE)

par(mar = c(4, 8, 4, 4))
boxplot(formula = age ~ gender + religion, 
        data = df,
        col = c("yellow", "green"),
        horizontal = TRUE,
        outline = TRUE,
        las = 1,
        ann = FALSE)

par(mar = c(4, 8, 4, 4))
boxplot(formula = age ~ gender : religion, 
        data = df,
        col = c("yellow", "green"),
        border = "blue",
        pch = 1,
        horizontal = FALSE,
        outline = TRUE,
        las = 1,
        ann = FALSE)


par(mar = c(4, 16, 4, 4))
boxplot(formula = age ~ gender : religion : major, 
        data = df,
        col = c("yellow", "green", "pink"),
        border = "blue",
        pch = 1,
        horizontal = TRUE,
        outline = TRUE,
        las = 1,
        ann = FALSE)

# mat <- cbind(Uni05 = (1:100)/21, Norm = rnorm(100),
#              `5T` = rt(100, df = 5), Gam2 = rgamma(100, shape = 2))
# boxplot(mat) # directly, calling boxplot.matrix()

df. <- as.data.frame(mat)
boxplot(df.)

boxplot(len ~ dose, data = ToothGrowth,
        boxwex = 0.25, at = 1:3 - 0.2,
        subset = supp == "VC", col = "yellow",
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg",
        ylab = "tooth length",
        xlim = c(0.5, 3.5), ylim = c(0, 35), yaxs = "i")

boxplot(len ~ dose, data = ToothGrowth, add = TRUE,
        boxwex = 0.25, at = 1:3 + 0.2,
        subset = supp == "OJ", col = "orange")

legend(2, 9, c("Ascorbic acid", "Orange juice"),
       fill = c("yellow", "orange"))



boxplot(len ~ dose:supp, data = ToothGrowth,
        boxwex = 0.5, col = c("orange", "yellow"),
        main = "Guinea Pigs' Tooth Growth",
        xlab = "Vitamin C dose mg", ylab = "tooth length",
        sep = "--",
        lex.order = TRUE, 
        ylim = c(0, 35),
        yaxs = "i")

















