# ------------------------------- helper functions -------------------------------

#' Obtain a Modified version of the Mahalnobis Distance between two populations
#'
#' @param df1 Population Data Frame from which distance is to be measured
#' @param df2 Population Data Frame for which distance is to be measured (variables should be same as df1)
#'
#' @return A numerical value denoting the modified mahalanobis distance
#' @export
#'
#' @examples
get_mahalanobis_distance <- function(df1, df2){

    # df2_mean <- colMeans(df2)
    # mnb_dist <- (mahalanobis(df2_mean, colMeans(df1), cov(df1)))^2

    mnb_dist <- sqrt(mean((mahalanobis(df2, colMeans(df1), cov(df1)))^2))
    return(mnb_dist)
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
        ggplot2::geom_text(label = rep(1:n, times = 2), size = 3) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 4),
                       legend.text = ggplot2::element_text(size = 6),
                       title = ggplot2::element_text(size = 6),
                       legend.position = "bottom") +
        ggplot2::xlab(metric.distance) +
        ggplot2::ylab(metric.performance) +
        ggplot2::ggtitle(title) +
        ggplot2::scale_color_manual(values = c( "#d00000", "#1976d2",  "#662e9b", "#ffba08")) +
        # ggplot2::scale_color_manual(values = c( "#ff6f69", "#ffcc5c", "#005b96", "#88d8b0")) +
        # ggplot2::scale_color_brewer(palette = "Dark2") +
        ggplot2::theme_bw() +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4, shape = 15)))

    return(p)
}


get_df_melt <- function(vec1, vec2, vec3) {

    df <- data.frame(Distance = vec1, TrainPerformance = vec2, TestPerformance = vec3)
    df.melt <- reshape2::melt(df, id.vars = 'Distance',
                              variable.name = 'PerformanceType',
                              value.name = 'PerformanceValue')
    return(df.melt)
}




# generate_plot_optimal <- function(dataset.name, df, model.relation, min_test_size, response.var, num.iterations){
#
#     n <- nrow(df)
#     split.percentages <- seq(0.5, 0.9, 0.05)
#     train.performance <- c()
#     test.performance <- c()
#     distance.wr <- c()
#     distance <- c()
#     splits <- c()
#
#     print("Beginning Simulations")
#     outer_i <-0
#     for (split.percentage in split.percentages){
#         outer_i <- outer_i + 1
#         for (i in 1:num.iterations){
#
#             s <- as.vector(sample(x = 1:n, size = floor(n*split.percentage), replace = F))
#
#             df.train <- df[s, ]
#             df.test <- df[-s, ]
#
#             scores <- get_scores(df.train, df.test, response.var, model.relation)
#
#             train.performance <- c(train.performance, scores[1])
#             test.performance <- c(test.performance, scores[2])
#             distance.wr <- c(distance.wr, scores[3])
#             distance <- c(distance, scores[4])
#             splits <- c(splits, s)
#
#             if(i %% 10 == 0){
#                 svMisc::progress(i*outer_i/(length(split.percentages)*num.iterations)*100, progress.bar = TRUE)
#             }
#         }
#     }
#     print("Simulations Complete")
#
#     df.scores <- data.frame(distance = distance,
#                             distance.wr = distance.wr,
#                             train.performance = train.performance,
#                             test.performance = test.performance,
#                             split.percentage = rep(split.percentages, each = num.iterations))
#
#
#
#     df.melt.wr <- get_df_melt(distance.wr, train.performance, test.performance)
#     p_wr <- get_plot(num.iterations*length(split.percentages),
#                      df.melt.wr,
#                      paste(dataset.name, '( Distance without', response.var, ')'),
#                      'Akaike Information Criterion (AIC)',
#                      'Modified Mahalanobis Distance')
#
#
#     df.melt <- get_df_melt(distance, train.performance, test.performance)
#     p <- get_plot(num.iterations*length(split.percentages),
#                   df.melt,
#                   paste(dataset.name,  '( Distance with', response.var, ')'),
#                   'Akaike Information Criterion (AIC)',
#                   'Modified Mahalanobis Distance')
#
#     arranged_plot <- ggpubr::ggarrange(p_wr, p, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
#     arranged_plot <- ggpubr::annotate_figure(arranged_plot,
#                                              top = ggpubr::text_grob(deparse(model.relation),
#                                                                      color = "red",
#                                                                      face = "bold",
#                                                                      size = 10))
#     dir.create("Output")
#     dir.create(paste0('Output', '/',dataset.name))
#     filename <- paste0('Output', '/', dataset.name, '/', deparse(model.relation), '_optimal.png')
#     ggplot2::ggsave(filename, plot = arranged_plot, device = 'png', width = 8, height = 4, dpi = 1000, bg = "white")
#     print(arranged_plot)
#
#     write.csv(df.scores,
#               paste0("Output", "/", dataset.name, "/", deparse(model.relation), "_optimal.csv"),
#               row.names = FALSE)
#
#     return(TRUE)
#
# }

