# Get Distance Metric for given split
calculate_distance <- function(train, test){

    num_cols <- unlist(lapply(train, is.numeric))
    train <- train[, num_cols]
    test <- test[, num_cols]

    n.train <- nrow(train)
    n.test <- nrow(test)

    cov.train <- cov(train)
    cov.test <- cov(test)

    mu.train <- colMeans(train)
    mu.test <- colMeans(test)

    pooled_cov <- matrix(((n.train-1)*cov.train + (n.test-1)*cov.test)/(n.train + n.test -2),
                         ncol = ncol(train))

    d1 <- mean(mahalanobis(train, mu.test, pooled_cov))
    d2 <- mean(mahalanobis(test, mu.train, pooled_cov))

    d <- (d1 + d2)/2

    return(d)
}


# Get Model Performance
calculate_performance <- function(actual, prediction, n, k, metric.performance = "Normalized AIC"){
    performance <- 0
    # finding AIC

    act <- as.numeric(actual)
    pred <- as.numeric(prediction)

    if (metric.performance == "Normalized AIC"){
        rss <- sum((act-pred)**2)
        k <- k + 1
        performance <- log(rss/n) + 2*k/n
    }

    # finding R squared
    else if (metric.performance == "R Squared"){
        rss <- sum((act - pred)**2)
        tss <- sum((act - mean(actual))**2)
        performance <- 1 - rss/tss
    }
  
    return(performance)
}

# get_scores <- function(df.train, df.test, model.relation, metric.performance = "Normalized AIC") {
#
#     response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])
#
#     model <- nnet::multinom(model.relation, data = df.train)
#     train.data <- as.data.frame(model.matrix(model))[-c(1)]
#     test.data <- as.data.frame(model.matrix(model.relation, df.test))[-c(1)]
#
#     train.predictions <- predict(model, df.train)
#     train.actual <- df.train[[response.var]]
#
#     test.predictions <- predict(model, df.test)
#     test.actual <- df.test[[response.var]]
#
#     num.variables <- ncol(train.data) + 1
#
#     train.performance <- calculate_performance(train.actual, train.predictions, nrow(df.train),
#                                                num.variables, metric.performance)
#     test.performance <- calculate_performance(test.actual, test.predictions, nrow(df.test),
#                                               num.variables, metric.performance)
#
#     train.data[[response.var]] <- df.train[[response.var]]
#     test.data[[response.var]] <- df.test[[response.var]]
#     distance <- calculate_distance(train.data, test.data)
#
#     return(c(train.performance, test.performance, distance))
#
# }

# obtain the train AIC, test AIC, Metric distance for a given split.
get_scores <- function(df.train, df.test, model.relation, metric.performance = "Normalized AIC") {
    # getting response variable from model.relation
    response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])

    # training the model
    model <- lm(model.relation, data = df.train)
    train.data <- as.data.frame(model.matrix(model))[-c(1)]
    test.data <- as.data.frame(model.matrix(model.relation, df.test))[-c(1)]

    # getting train predictions
    train.predictions <- predict(model, df.train)
    train.actual <- df.train[[response.var]]

    # getting test predictions
    test.predictions <- predict(model, df.test)
    test.actual <- df.test[[response.var]]

    num.variables <- ncol(train.data) + 1

    # calculating train and test performance
    train.performance <- calculate_performance(train.actual, train.predictions, nrow(df.train),
                                         num.variables, metric.performance)
    test.performance <- calculate_performance(test.actual, test.predictions, nrow(df.test),
                                        num.variables, metric.performance)

    # calculating the proposed distance metric for given train and test split
    train.data[[response.var]] <- df.train[[response.var]]
    test.data[[response.var]] <- df.test[[response.var]]
    distance <- calculate_distance(train.data, test.data)

    return(c(train.performance, test.performance, distance))

}

# calculating one sided threshold for given alpha value
get_one_sided_threshold <- function(d, alpha){

    l <- length(d)
    n <- ceiling((1 - alpha)*l)
    c <- sort(d)[n]

    return(c)
}

# calculating two sided threshold for given alpha value
get_two_sided_threshold <- function(d, alpha){

    b <- length(d)
    n1 <- floor(alpha*b/2)
    n2 <- ceiling((1 - alpha/2)*b)

    if(n1 == 0){
        n1 <- 1
    }
    c1 <- sort(d)[n1]
    c2 <- sort(d)[n2]

    return(c(c1, c2))
}
