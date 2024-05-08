setwd('/home/l/src/rpkg/lbml')
house_train <- read.csv("rawdata/house_train.csv", stringsAsFactors = FALSE)
house_test <- read.csv("rawdata/house_test.csv", stringsAsFactors = FALSE)


house_train <- within(house_train, {
    y <- SalePrice
    id <- Id
    Id <- NULL
    SalePrice <- NULL
})

house_test <- within(house_test, {
    y <- NA
    id <- Id
    Id <- NULL
})



save("house_train", file = "data/house_train.rda", compress = "bzip2")
save("house_test", file = "data/house_test.rda", compress = "bzip2")
