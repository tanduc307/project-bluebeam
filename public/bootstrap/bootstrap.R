# https://datasciencetut.com/how-to-perform-bootstrapping-in-r/

# Và anh nhờ Đức làm 1 boostrap cho một vài test như so sanh mean 2, 3 nhom, ...hom nao online Duc giai tjich anh ý nghia cua cac lenh va ham khi lam boostrap de anh tu lam cho nhung test khac nhu boostrap mau teuoc khi dung cac regression

# How to Perform Bootstrapping in R, Bootstrapping is a method for estimating the standard error of any statistic and generating a confidence interval for the statistic.
# 
# The basic bootstrapping procedure is as follows:
#                
# Take k repeated replacement samples from a given dataset.
# 
# Calculate the statistic of interest for each sample.
# 
# These yields k different estimates for a given statistic, which you can then use to calculate the statistic’s standard error and create a confidence interval.

# https://towardsdatascience.com/a-practical-guide-to-bootstrap-with-r-examples-bd975ec6dcea#:~:text=Bootstrap%20is%20a%20resampling%20method,check%20my%20Github%20(link).

# Bootstrap is a resampling method with replacement.

set.seed(123)
library(boot)


# boot(data, statistic, R, …)

# https://rpubs.com/evelynebrie/bootstrapping


rsq_function <- function(formula, data, indices) {
               d <- data[indices,] #allows boot to select sample
               fit <- lm(formula, data=d)
               return(summary(fit)$r.square)
}

reps <- boot(data=mtcars, 
             statistic=rsq_function, R=3000, 
             formula=mpg~disp)

reps

plot(reps)

boot.ci(reps, type="bca")

####################

can_nang <- c(60, 55, 71, 45, 89, 56, 73, 49, 69, 88, 75)

can_nang_ok <- as.data.frame(can_nang)

mean(can_nang)

tinh_trung_binh <- function(data_input) {
               
               mean(data_input) -> ok

               return(ok)
}

ket_qua_bootstrap <- boot(data = can_nang, 
                          statistic = tinh_trung_binh, 
                          R = 1000,
                          data_input = can_nang)


?boot

###################################################

# https://stackoverflow.com/questions/40025160/bootstrapping-sample-means-in-r-using-boot-package-creating-the-statistic-funct

can_nang <- c(60, 55, 71, 45, 89, 56, 73, 49, 69, 88, 75)

can_nang_df <- as.data.frame(can_nang)

library(boot)

meanfun <- function(data, i){
               d <- data[i, ]
               return(mean(d))   
}


meanfun(can_nang_df)

set.seed(123)
bo <- boot(data = can_nang_df, statistic = meanfun, R = 10)

unclass(bo)

boot.ci(bo, conf = 0.95, type = "bca")


?boot.ci

resampled.data <- boot.array(bo, 7)

?boot.array

##############

set.seed(123) # Setting the seed for replication purposes

can_nang <- c(60, 55, 71, 45, 89, 56, 73, 49, 69, 88, 75)

length(can_nang) # Sample size

n_boot <- 1000 # Number of bootstrap samples

data_boot <- data.frame() 

for (i in 1:n_boot)
{
               cach_lay_mau <- sample(1:length(can_nang), replace = TRUE)
               
               data_boot <- rbind(data_boot, can_nang[cach_lay_mau])
}


colnames(data_boot) <- paste("Giá trị đo thứ", 1:length(can_nang)) 
rownames(data_boot) <- paste("Lần lấy mẫu giả lập thứ", 1:n_boot)

apply(X = data_boot, MARGIN = 1, FUN = mean) -> ket_qua_trung_binh

length(ket_qua_trung_binh)


data_boot$ket_qua_trung_binh <- ket_qua_trung_binh


summary(data_boot$ket_qua_trung_binh)

sd(data_boot$ket_qua_trung_binh)

hist(data_boot$ket_qua_trung_binh)

write.csv(data_boot, "data_boot.csv")

write.table(x = data_boot, file = "data_boot.tsv", 
            row.names = F, sep = "\t\t\t\t", quote = FALSE)

knit::kable(data_boot)


library(kableExtra)
kable(data_boot[1:3, 1:4], "simple") -> ok


ok
class(ok)

attributes(ok)

unclass(ok) -> eee

attributes(eee) <- NULL

eee

class(unclass(ok))

write.table(ok, "ok.txt", quote = FALSE, row.names = FALSE)



# readLines("ok.txt", n=2:30) -> iii

?readLines



?kable

# https://bash-intro.rsquaredacademy.com/r-command-line.html

system("cat D:\\GITHUB\\project-bluebeam\\public\\bootstrap\\ok.txt | sed 1d > D:\\GITHUB\\project-bluebeam\\public\\bootstrap\\ppp.txt")

system("notepad ok.txt")

?system()

system("ls -F")



# cat ok.txt | sed 1d > uuu.txt

system("cat ok.txt | sed 1d")

cat("cat ok.txt | sed 1d > wert.txt")













