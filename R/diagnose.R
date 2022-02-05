# get mahalanobis distance of df2 from df1
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


#
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

# obtain the train AIC, test AIC, mnb distances for a given split.
get_scores <- function(df_train, df_test, response_var, model_relation) {

    model <- lm(model_relation, data = df_train)
    train_data <- as.data.frame(model.matrix(model))[-c(1)]
    test_data <- as.data.frame(model.matrix(model_relation, df_test))[-c(1)]

    train_prediction <- predict(model, df_train)
    train_actual <- df_train[[response_var]]

    test_prediction <- predict(model, df_test)
    test_actual <- df_test[[response_var]]

    num_variables <- ncol(train_data) + 1

    train_aic <- get_AIC(train_actual, train_prediction, nrow(df_train), num_variables) / nrow(df_train)
    test_aic <- get_AIC(test_actual, test_prediction, nrow(df_test), num_variables) / nrow(df_test)

    d_wr <- get_mahalanobis_distance(train_data, test_data)

    train_data[[response_var]] <- df_train[[response_var]]
    test_data[[response_var]] <- df_test[[response_var]]
    d <- get_mahalanobis_distance(train_data, test_data)

    return(c(train_aic, test_aic, d_wr, d))

}


get_plot <- function(num_iterations, df_melt, title, performance_metric, distance_metric) {

    p <- ggplot2::ggplot(df_melt, ggplot2::aes(Distance, PerformanceValue, col = PerformanceType)) +
        ggplot2::geom_text(label = rep(1:num_iterations, times = 2), size = 2) +
        ggplot2::xlab(distance_metric) +
        ggplot2::ylab(performance_metric) +
        ggplot2::ggtitle(title) +
        ggplot2::scale_color_manual(values = c("#da1e28",  "#08bdba",  "#6929c4", "#ffb635")) +
        ggplot2::theme_bw() +
        ggplot2::theme(text = ggplot2::element_text(size = 8), legend.text = ggplot2::element_text(size=8)) +
        ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4, shape = 16)))

    return(p)
}


get_df_melt <- function(vec1, vec2, vec3) {

    df_local <- data.frame(Distance = vec1, TrainPerformance = vec2, TestPerformance = vec3)
    df_melt <- reshape2::melt(df_local, id.vars = 'Distance',
                              variable.name = 'PerformanceType',
                              value.name = 'PerformanceValue')
    return(df_melt)
}


generate_plot <- function(dataset_name, num_iterations, df, split_percentage, response_var,
                          model_relation, initial_scores) {

    n <- nrow(df)

    train_performance <- c()
    test_performance <- c()
    distance_wr <- c()
    distance <- c()

    for (i in 1:num_iterations){

        s <- as.vector(sample(x = 1:n, size = floor(n*split_percentage), replace = F))

        df_train <- df[s, ]
        df_test <- df[-s, ]

        scores <- get_scores(df_train, df_test, response_var, model_relation)

        train_performance <- c(train_performance, scores[1])
        test_performance <- c(test_performance, scores[2])
        distance_wr <- c(distance_wr, scores[3])
        distance <- c(distance, scores[4])

    }

    df_melt_wr <- get_df_melt(distance_wr, train_performance, test_performance)
    p_wr <- get_plot(num_iterations,
                     df_melt_wr,
                     paste(dataset_name, '( Distance without', response_var, ')'),
                     'Akaike Information Criterion (AIC)',
                     'Modified Mahalanobis Distance') +
            ggplot2::geom_point(ggplot2::aes(x = initial_scores[3],
                                             y = initial_scores[1],
                                             colour = 'Train (Given Split)',
                                             size = 3),
                                shape = 17) +
            ggplot2::geom_point(ggplot2::aes(x = initial_scores[3],
                                             y = initial_scores[2],
                                             colour = 'Test (Given Split)',
                                             size = 3),
                                shape = 17) +
            ggplot2::guides(size = "none")

    df_melt <- get_df_melt(distance, train_performance, test_performance)
    p <- get_plot(num_iterations,
                  df_melt,
                  paste(dataset_name,  '( Distance with', response_var, ')'),
                  'Akaike Information Criterion (AIC)',
                  'Modified Mahalanobis Distance') +
            ggplot2::geom_point(ggplot2::aes(x = initial_scores[4],
                                             y = initial_scores[1],
                                             colour = 'Train (Given Split)',
                                             size = 3),
                                shape = 17) +
            ggplot2::geom_point(ggplot2::aes(x = initial_scores[4],
                                             y = initial_scores[2],
                                             colour = 'Test (Given Split)',
                                             size = 3),
                                shape = 17) +
            ggplot2::guides(size = "none")

    arranged_plot <- ggpubr::ggarrange(p_wr, p, ncol = 2, nrow = 1, common.legend = TRUE, legend = "right")
    arranged_plot <- ggpubr::annotate_figure(arranged_plot,
                                             top = ggpubr::text_grob(deparse(model_relation),
                                                                     color = "red",
                                                                     face = "bold",
                                                                     size = 10))

    dir.create(paste0('Output', '/',dataset_name))
    filename <- paste0('Output', '/', dataset_name, '/', deparse(model_relation), '.png')
    ggplot2::ggsave(filename, plot = arranged_plot, device = 'png', width = 8, height = 4, dpi = 1000, bg = "white")
    print(arranged_plot)

    return(data.frame(distance = distance,
                      distance_wr = distance_wr,
                      train_performance = train_performance,
                      test_performance = test_performance))
}


#' Diagnose the Random Split
#'
#' @param dataset_name Name of the dataset (string)
#' @param df_train Train set corresponding to the initial split performed by the user
#' @param df_test Test set corresponding to the initial split performed by the user
#' @param model_relation The Regression Model to be fitted on the training data
#'
#' @return
#' 1. Decision regarding the randomness of the split.
#' 2. Decision regarding the performance of the model.
#' 3. Supporting Plots of Mahalanobis Distance vs. Akaike Information Criterion.
#' @export
#'
#' @examples
#'
#' # ---------------- Example 1 ----------------
#'
#' # data preparation
#' dataset_name <- "Abalone"
#' data(abalone)
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.7), replace = F)
#' df_train <- abalone[s, ]
#' df_test <- abalone[-s, ]
#'
#' # defining model relation based on variables of data
#' model_relation <- WholeWeight ~ Height + LongestShell + Diameter
#'
#' # function call
#' diagnose(dataset_name, df_train, df_test, model_relation)
#'
#' # ---------------- Example 2 ----------------
#'
#' # data preparation
#' dataset_name <- "Diamonds"
#' data(diamonds)
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(diamonds), size = floor(nrow(diamonds)*0.7), replace = F)
#' df_train <- diamonds[s, ]
#' df_test <- diamonds[-s, ]
#'
#' # defining model relation based on variables of data
#' model_relation <- price ~ x:z + x + z + depth
#'
#' # function call
#' diagnose(dataset_name, df_train, df_test, model_relation)
#'
#'
diagnose <- function(dataset_name = "DATASET", df_train, df_test, model_relation) {

    # print("evaluate")

    response_var <- stringr::str_trim(strsplit(deparse(model_relation), "\\~")[[1]][1])
    # params <- strsplit(strsplit(deparse(model_relation), "\\~")[[1]][2], "\\+")[[1]]
    # independent_vars  <- unlist(lapply(params, function(x){return(stringr::str_trim(x))}))


    # df_train <- df_train[c(independent_vars, response_var)]
    # df_test <- df_test[c(independent_vars, response_var)]

    n1 <- nrow(df_train)
    n2 <- nrow(df_test)
    n <- n1 + n2
    split_percentage <- n1/n

    initial_scores <- get_scores(df_train, df_test, response_var, model_relation)

    df <- rbind(df_train, df_test)
    num_iterations <- 200
    df_scores <- generate_plot(dataset_name, num_iterations, df, split_percentage,
                               response_var, model_relation, initial_scores)
    dir.create("Output")
    dir.create(paste0("Output", "/", dataset_name))
    write.csv(df_scores,
              paste0("Output", "/", dataset_name, "/", deparse(model_relation), ".csv"),
              row.names = FALSE)
}



