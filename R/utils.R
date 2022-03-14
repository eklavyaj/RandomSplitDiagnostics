# ------------------------------- helper functions -------------------------------

#' Obtain a Modified version of the Mahalnobis Distance between two populations
#'
#' @param train Population Data Frame from which distance is to be measured
#' @param test Population Data Frame for which distance is to be measured (variables should be same as train)
#'
#' @return A numerical value denoting the modified mahalanobis distance
#' @export
#'
#' @examples
get_mahalanobis_distance <- function(train, test){

    # mnb_dist <- sqrt(mean((mahalanobis(df2, colMeans(df1), cov(df1)))^2))
    # return(mnb_dist)

    n1 <- nrow(train)
    n2 <- nrow(test)

    cov1 <- cov(train)
    cov2 <- cov(test)

    mu1 <- colMeans(train)
    mu2 <- colMeans(test)

    pooled_cov <- matrix(((n1-1)*cov1 + (n2-1)*cov2)/(n1 + n2 -2), ncol = ncol(train))

    d1 <- mean(mahalanobis(train, mu2, pooled_cov))
    d2 <- mean(mahalanobis(test, mu1, pooled_cov))

    d <- d2 - d1

    return(d)

    # Hotelling T2
    # n1 <- nrow(train)
    # n2 <- nrow(test)
    #
    # cov1 <- cov(train)
    # cov2 <- cov(test)
    #
    # mu1 <- colMeans(train)
    # mu2 <- colMeans(test)
    #
    # pooled_cov <- matrix(((n1-1)*cov1 + (n2-1)*cov2)/(n1 + n2 -2), ncol = ncol(train))
    # diff <- matrix(mu1 - mu2)
    #
    # t2 <- (n1*n2/(n1 + n2))*(t(diff) %*% pooled_cov %*% diff)
    # return(t2)
}


#' Get the Residual Sum of Squares
#'
#' @param actual Actual values of response variable
#' @param prediction Model predictions of response variable
#'
#' @return A numeric value denoting the residual sum of squares
#' @export
#'
#' @examples
#'
get_RSS <- function(actual, prediction){

    RSS <- sum((actual-prediction)**2)
    return(RSS)
}


# get Akaike Information Criterion score for a model using RSS
get_AIC <- function(actual, prediction, n, k){

    # print("get_AIC")
    rss <- get_RSS(actual, prediction)
    k <- k + 1

    if (n > 4000) {
        aic <- n*log(rss/n) + 2*k
    } else {
        aic <- n*log(rss/n) + 2*k + (2*k*(k+1))/(n-k-1)
    }

    return(aic)
}

# obtain the train AIC, test AIC, Mahalanobis distances for a given split.
get_scores <- function(df.train, df.test, response.var, model.relation) {

    model <- lm(model.relation, data = df.train)
    train.data <- as.data.frame(model.matrix(model))[-c(1)]
    test.data <- as.data.frame(model.matrix(model.relation, df.test))[-c(1)]

    train.predictions <- predict(model, df.train)
    train.actual <- df.train[[response.var]]

    test.predictions <- predict(model, df.test)
    test.actual <- df.test[[response.var]]

    num.variables <- ncol(train.data) + 1

    train.aic <- get_AIC(train.actual, train.predictions, nrow(df.train), num.variables) / nrow(df.train)
    test.aic <- get_AIC(test.actual, test.predictions, nrow(df.test), num.variables) / nrow(df.test)

    distance.wr <- get_mahalanobis_distance(train.data, test.data)

    train.data[[response.var]] <- df.train[[response.var]]
    test.data[[response.var]] <- df.test[[response.var]]
    d <- get_mahalanobis_distance(train.data, test.data)

    return(c(train.aic, test.aic, distance.wr, d))

}


get_plot <- function(n, df.melt, title, metric.performance, metric.distance) {

    p <- ggplot2::ggplot(df.melt, ggplot2::aes(Distance, PerformanceValue, col = PerformanceType)) +
        ggplot2::geom_text(label = rep(1:n, times = 2), size = 1.5) +
        ggplot2::xlab(metric.distance) +
        ggplot2::ylab(metric.performance) +
        ggplot2::ggtitle(title) +
        ggplot2::scale_color_manual(values = c( "#08bdba", "#da1e28",  "#ffb635", "#6929c4")) +
        ggplot2::theme_bw() +
        ggplot2::theme(panel.spacing = ggplot2::unit(0.5, "in"), legend.position = "bottom",
                       axis.text = ggplot2::element_text(size = 6),
                       axis.title = ggplot2::element_text(size = 7),
                       legend.text = ggplot2::element_text(size = 6),
                       title = ggplot2::element_text(size = 7) ) +
        ggplot2::guides(colour = ggplot2::guide_legend(title = "Set", override.aes = list(size = 5, shape = 15)))

    return(p)
}


get_df_melt <- function(vec1, vec2, vec3) {

    df <- data.frame(Distance = vec1, Train = vec2, Test = vec3)
    df.melt <- reshape2::melt(df, id.vars = 'Distance',
                              variable.name = 'PerformanceType',
                              value.name = 'PerformanceValue')
    return(df.melt)
}


