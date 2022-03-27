simulate <- function(dataset.name,
                     df.train,
                     df.test,
                     num.simulations,
                     model.relation,
                     split.percentage,
                     initial.scores,
                     alpha,
                     save.plots = TRUE,
                     output.dir,
                     metric.performance = "Normalized AIC"){

    df <- rbind(df.train, df.test)
    n <- nrow(df)

    train.performance <- c()
    test.performance <- c()
    distance <- c()

    for (i in 1:num.simulations){

        s <- sample(x = 1:n, size = floor(n*split.percentage), replace = F)

        df.train.temp <- df[s, ]
        df.test.temp <- df[-s, ]

        scores <- get_scores(df.train.temp, df.test.temp, model.relation, metric.performance)

        train.performance <- c(train.performance, scores[1])
        test.performance <- c(test.performance, scores[2])
        distance <- c(distance, scores[3])

    }


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

    visualize_threshold(dataset.name,
                   distance,
                   df.train,
                   df.test,
                   model.relation,
                   initial.scores,
                   alpha,
                   save.plots,
                   output.dir)

}
