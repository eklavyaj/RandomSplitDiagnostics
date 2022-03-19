get_distance <- function(train, test){

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

    d <- d2 - d1

    return(d)
}


# get Akaike Information Criterion score for a model using RSS
get_performance <- function(actual, prediction, n, k, metric.performance = "Normalized AIC"){
    performance <- 0
    if (metric.performance == "Normalized AIC"){
        rss <- sum((actual-prediction)**2)
        k <- k + 1
        performance <- log(rss/n) + 2*k/n
    }
    return(performance)
}


# obtain the train AIC, test AIC, Metric distance for a given split.
get_scores <- function(df.train, df.test, model.relation, metric.performance = "Normalized AIC") {

    response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])

    model <- lm(model.relation, data = df.train)
    train.data <- as.data.frame(model.matrix(model))[-c(1)]
    test.data <- as.data.frame(model.matrix(model.relation, df.test))[-c(1)]

    train.predictions <- predict(model, df.train)
    train.actual <- df.train[[response.var]]

    test.predictions <- predict(model, df.test)
    test.actual <- df.test[[response.var]]

    num.variables <- ncol(train.data) + 1

    train.performance <- get_performance(train.actual, train.predictions, nrow(df.train),
                                 num.variables, metric.performance)
    test.performance <- get_performance(test.actual, test.predictions, nrow(df.test),
                                num.variables, metric.performance)

    train.data[[response.var]] <- df.train[[response.var]]
    test.data[[response.var]] <- df.test[[response.var]]
    distance <- get_distance(train.data, test.data)

    return(c(train.performance, test.performance, distance))

}

get_threshold <- function(d, alpha){

    d.abs <- abs(d)
    b <- length(d)
    n1 <- floor(alpha*b/2)
    n2 <- ceiling((1 - alpha/2)*b)

    if(n1 == 0){
        n1 <- 1
    }
    c1 <- sort(d.abs)[n1]
    c2 <- sort(d.abs)[n2]

    return(c(c1, c2))
}

