#' Title
#'
#' @param dataset.name Name of the Dataset (String)
#' @param df.train Train Partition (R DataFrame)
#' @param df.test Test Partition (R DataFrame)
#' @param num.simulations Number of simulations, defaults to 200
#' @param model.relation The relation used for regression model
#' @param split.percentage Ratio of the number of train entries to total entries
#' @param initial.scores The performance metric value for the given initial train-test split
#' @param alpha The level of the test for visualize_threshold, default set to 0.05
#' @param save.plots Saves plots in output.dir when set to TRUE
#' @param output.dir The path to output directory the plots are saved to
#' @param metric.performance The performance metric, usually Normalized AIC
#'
#' @return
#'
#'
simulate <- function(dataset.name,
                     df.train,
                     df.test,
                     num.simulations,
                     model.relation,
                     split.percentage,
                     initial.scores = c(),
                     alpha,
                     save.plots = TRUE,
                     output.dir,
                     metric.performance = "Normalized AIC"){

    # concatenating both train and test data to generate more random splits
    df <- rbind(df.train, df.test)
    n <- nrow(df)

    train.performance <- c()
    test.performance <- c()
    distance <- c()

    # simulating num.simulations number of random splits
    for (i in 1:num.simulations){

        s <- sample(x = 1:n, size = floor(n*split.percentage), replace = F)

        df.train.temp <- df[s, ]
        df.test.temp <- df[-s, ]

        if(model.relation == ""){
            dist <- calculate_distance(df.train.temp, df.test.temp)
            distance <- c(distance, dist)

        }

        else{
            scores <- get_scores(df.train.temp, df.test.temp, model.relation, metric.performance)
            train.performance <- c(train.performance, scores[1])
            test.performance <- c(test.performance, scores[2])
            distance <- c(distance, scores[3])
        }
    }

    if(model.relation == ""){
        split.conclusion <- visualize_threshold(dataset.name,
                            distance,
                            df.train,
                            df.test,
                            model.relation,
                            initial.scores,
                            alpha,
                            save.plots,
                            output.dir)
    }

    else{
        df.scores <- data.frame(Distance = distance,
                                Train = train.performance,
                                Test = test.performance)

        visualize_simulation(df.scores,
                             dataset.name,
                             initial.scores,
                             model.relation,
                             save.plots,
                             output.dir,
                             metric.performance)

        visualize_clusters(df.scores,
                           dataset.name,
                           initial.scores,
                           model.relation,
                           save.plots,
                           output.dir,
                           metric.performance)

        split.conclusion <- visualize_threshold(dataset.name,
                            distance,
                            df.train,
                            df.test,
                            model.relation,
                            initial.scores,
                            alpha,
                            save.plots,
                            output.dir)

    }

    return(split.conclusion)

}
