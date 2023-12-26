library(readxl)
df_1 <- read_excel("df_1.xlsx")
df_2 <- read_excel("df_2.xlsx")
df_3 <- read_excel("df_3.xlsx")

df_1 |> subset(country == "vn") -> df_1_vn

df_1$country_year <- paste0(df_1$country, "-", df_1$year)

identical(dim(df_1)[1], length(unique(df_1$country_year)))

df_2$country_year <- paste0(df_2$country, "-", df_2$year)

identical(dim(df_2)[1], length(unique(df_2$country_year)))

merge(x = df_1,
      y = df_2,
      by = "country_year",
      all = TRUE) -> df_1_2

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

#########################################

df_1_2_3_final 


install.packages("missMDA")

library(missMDA)
library(FactoMineR)
library(factoextra)

data(orange)
orange
class(orange)

nb <- estim_ncpPCA(orange)
res.comp <-  imputePCA(orange)

res.pca <- PCA(res.comp$completeObs)

fviz_pca_biplot(res.pca)

?estim_ncpPC



summary(res.pca)
res.pca$eig

facto_summarize(res.pca, element = "var")
var <- get_pca_var(res.pca)
var$coord
##################################










res.comp$completeObs -> df_test

res.pca <- PCA(df_test)

facto_summarize(res.pca, element = "var")
var <- get_pca_var(res.pca)
var$coord


df_test[1, 4] <- 15

df_test[2, 4] <- 20

df_test[3, 4] <- 22





############################################

# res.comp <-  imputePCA(df_us_check,
#                        ncp = 4, 
#                        scale = TRUE)
# 
# res.comp$completeObs
# 
# ?imputePCA

library(missMDA)
library(FactoMineR)
library(factoextra)
library(mice)

df_pca <- df_1_2_3_final[12:33, c(-7, -8)]



df_full_final <- data.frame()


for(i in 1:length(unique(df_pca$country))) {
  
  df_pca |> subset(country == unique(df_pca$country)[i]) -> df_1a
  
  df_1a_check <- df_1a[, c("var_1", "var_2", "var_3")]
  
  imputed_Data <- mice(df_1a_check, 
                       m=5, 
                       maxit = 50, 
                       method = 'pmm', 
                       seed = 500)
  
  complete(imputed_Data) -> df_1a_full
  
  df_1a_full$country <- df_1a$country
  df_1a_full$year <- df_1a$year
  
  df_full_final <- rbind(df_1a_full, df_full_final)
  
  
}

df_full_final_yes <- df_full_final[, c(4, 5, 1, 2, 3)]

df_pca |> dplyr::arrange(desc(country), year) -> df_pca_1


?mice





res.pca <- PCA(df_full_final_yes,
               quali.sup = c(1, 2))

res.pca$eig

factoextra::fviz_eig(res.pca, addlabels = TRUE)



fviz_pca_biplot(res.pca,
                col.ind = df_full_final_yes$country, 
                palette = "jco",
                addEllipses = TRUE, 
                label = "var",
                col.var = "black", 
                repel = TRUE,
                legend.title = "country")


"factor analysis"











  



































