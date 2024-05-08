setwd('/home/l/src/rpkg/lbml')
titanic_train <- read.csv("rawdata/titanic_train.csv", stringsAsFactors = FALSE)
titanic_test <- read.csv("rawdata/titanic_test.csv", stringsAsFactors = FALSE)

titanic_train <- within(titanic_train, {
    y <- Survived
    id <- PassengerId
    Survived <- NULL
    PassengerId <- NULL
})

titanic_test <- within(titanic_test, {
    y <- NA
    id <- PassengerId
    PassengerId <- NULL
})

save("titanic_train", file = "data/titanic_train.rda", compress = "bzip2")
save("titanic_test", file = "data/titanic_test.rda", compress = "bzip2")
