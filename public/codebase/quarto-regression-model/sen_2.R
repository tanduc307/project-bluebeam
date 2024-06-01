data(trees)
trees -> df
df
names(df) <- c("duong_kinh", "chieu_cao", "the_tich")

fit <- lm(the_tich ~ duong_kinh + chieu_cao,
          data = df)

summary(fit)


library(sensitivity)

fit_tree <- function (input_x) {
  
  y <- coef(fit)[1] + 
    coef(fit)[2]*input_x[, 1] +
    coef(fit)[3]*input_x[, 2] 
  
  return(y)
    
}

n <- 10000

###
input_duong_kinh <- rnorm(1*n, 
                          mean = mean(df$duong_kinh, na.rm = TRUE), 
                          sd = sd(df$duong_kinh, na.rm = TRUE))

input_chieu_cao <- rnorm(1*n, 
                          mean = mean(df$chieu_cao, na.rm = TRUE), 
                          sd = sd(df$chieu_cao, na.rm = TRUE))

X1_cbin_pf <- data.frame(duong_kinh = input_duong_kinh,
                         chieu_cao = input_chieu_cao)
###
input_duong_kinh <- rnorm(1*n, 
                          mean = mean(df$duong_kinh, na.rm = TRUE), 
                          sd = sd(df$duong_kinh, na.rm = TRUE))

input_chieu_cao <- rnorm(1*n, 
                         mean = mean(df$chieu_cao, na.rm = TRUE), 
                         sd = sd(df$chieu_cao, na.rm = TRUE))

X2_cbin_pf <- data.frame(duong_kinh = input_duong_kinh,
                         chieu_cao = input_chieu_cao)
###

sens_1 <- sobol2007(model = fit_tree, 
                    X1 = X1_cbin_pf, 
                    X2 = X2_cbin_pf, 
                    nboot = 10000)



sens_1


plot(sens_1)


# I.M. Sobol, S. Tarantola, D. Gatelli, S.S. Kucherenko and W. Mauntz, 2007, Estimating the approximation errors when fixing unessential factors in global sensitivity analysis, Reliability Engineering and System Safety, 92, 957–960.
# 
# A. Saltelli, P. Annoni, I. Azzini, F. Campolongo, M. Ratto and S. Tarantola, 2010, Variance based sensitivity analysis of model output. Design and estimator for the total sensitivity index, Computer Physics Communications 181, 259–270.







