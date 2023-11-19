library(tuhocr)

crop_production <- system.file("extdata",
                               "crop_production_all_data.rds",
                               package = "tuhocr")

df_1 <- readRDS(crop_production)

FAOSTAT_data_2023 <- system.file("extdata",
                                 "FAOSTAT_data_3-21-2023.csv", 
                                 package = "tuhocr")

df_2 <- read.csv(FAOSTAT_data_2023)
# 
# library(kableExtra)
# library(formattable)

rice_compare <- filter_faostat(data_rds = df_1,
                               data_region = df_2,
                               item_filter = "Rice",
                               rank_filter = 1:10,
                               year_filter = c(2000, 2020)
                               )

rice_compare
