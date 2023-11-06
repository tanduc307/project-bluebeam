id <- c("111", "111", "112", "112", "112", 
        "114", "315", "315", "315", "539")

age <- c(56, NA, 56, 21, 45, 
         67, 86, 33, NA, NA)

gender <- c("male", "female", "female", "male", "male", 
            "male", "male", "female", "female", "male")

income <- c(1000, NA, NA, NA, 540, 
            200, 440, NA, NA, 300)

year <- c(2012, 2014, 2012, 2014, 2016,
          2016, 2014, 2016, 2012, 2016)

df <- data.frame(id, age, gender, income, year)

df

df_wide_1 <- reshape(df, 
                   direction = "wide",
                   idvar = "id",                
                   timevar = "year", 
                   v.names = c("age", "gender", "income"),
                   sep= "_")

df_long_1 <- reshape(df_wide_1, 
                     idvar = "id", 
                     varying = list(2:9),
                     v.names = c("age", "gender", "income"), 
                     direction = "long")

names(df_wide_1)



reshape(df_wide_1, 
        direction = "long",
        v.names = names(df_wide_1)[-1],
        varying = list(2:9)
        # times = names(w_df)[2:3],
        # timevar = "measure",
        # idvar = "city"
        )



#######################

w_df <- data.frame(
  city = c("London", "Barcelona", "Paris"),
  rivers = c(1, 2, 1),
  letters = c(6, 9, 5),
  stringsAsFactors = F
)
w_df

reshape(w_df, 
        direction = "long",
        varying = list(2:3))


reshape(w_df, direction = "long",
        v.names = "value",
        varying = 2:3,
        times = names(w_df)[2:3])

reshape(w_df, direction = "long",
        v.names = "value",
        varying = 2:3,
        times = names(w_df)[2:3],
        timevar = "measure")

reshape(w_df, direction = "long",
        v.names = "value",
        varying = 2:3,
        times = names(w_df)[2:3],
        timevar = "measure",
        idvar = "city")





# https://rstudio-pubs-static.s3.amazonaws.com/594650_88973f23f57c4d60b346abe0bc38801b.html#reshape_function

# https://stats.oarc.ucla.edu/r/faq/how-can-i-reshape-my-data-in-r/






#######################

# https://trinkerrstuff.wordpress.com/2012/05/06/reshape-from-base-explained-part-ii/
df3 <- data.frame(id = 1:4, age = c(40,50,60,50), dose1 = c(1,2,1,2),
                  dose2 = c(2,1,2,1), dose4 = c(3,3,3,3))
df3

reshape(data = df3, direction = "long"
        , idvar = "id"
        , timevar = "dose_type"
        , times = c(1, 2, 4)
        , v.names = "dose"
        , varying = 3:5
        , new.row.names = 1:12
        , sep = "")

reshape(data = df3, direction = "long"
        , varying = 3:5
        , sep = "")


set.seed(10)
dat <- data.frame(id=paste0("ID",1:5), 
                  sex=sample(c("male", "female"), 5, replace=TRUE), 
                  matrix(rpois(30, 10), 5, 6))
colnames(dat)[-c(1:2)] <- paste0(rep(1:2, times=3), 
                                 rep(c("work", "home", "church"), 2))
dat




reshape(dat, direction="long"
        , idvar = "id"
        , timevar = "part_area"
        , times = names(dat)[3:8]
        , v.names = "# pieces"
        , varying = 3:8
        , new.row.names = 1:30)

reshape(data = dat, direction = "long",
        varying = list(c(3, 7, 5), c(6, 4, 8)),
        idvar = "id",
        v.names = c("TIME_1", "TIME_2"),
        timevar = "PLACE",
        times = c("wrk", "hom", "chr"),
        new.row.names = 1:15)

reshape(data = dat, direction = "long",
        timevar = "TIME",
        times = 1:2,
        varying = list(c(3, 6), c(7, 4), c(5,8)),
        v.names = c("WORK", "HOME", "CHURCH"),
        new.row.names = 1:10)

wide_1 <- reshape(data = dat, direction = "long",
                  timevar = "TIME",
                  times = 1:2,
                  varying = list(c(3, 6), c(7, 4), c(5,8)),
                  v.names = c("WORK", "HOME", "CHURCH"),
                  new.row.names = 1:10)

reshape(data = wide_1, direction = "long",
        timevar = "PLACE",
        times = names(wide_1)[4:6], #c("work", "home", "church")
        varying = 4:6, #c("WORK", "HOME", "CHURCH"),
        v.names = "# people",
        new.row.names = 1:30)
