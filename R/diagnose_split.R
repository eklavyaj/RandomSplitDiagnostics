diagnose_split <- function(dataset.name, num.iterations, df, split.percentage, response.var, model.relation, initial.scores, dir){

    # ------------ run simulations => populate scores data frame ---------------
    n <- nrow(df)

    print("Progress: Running Simulations...")
    print('\n')
    train.performance <- c()
    test.performance <- c()
    distance.wr <- c()
    distance <- c()


    for (i in 1:num.iterations){

        s <- as.vector(sample(x = 1:n,
                              size = floor(n*split.percentage),
                              replace = F))

        df.train <- df[s, ]
        df.test <- df[-s, ]

        scores <- get_scores(df.train, df.test, response.var, model.relation)

        train.performance <- c(train.performance, scores[1])
        test.performance <- c(test.performance, scores[2])
        distance.wr <- c(distance.wr, scores[3])
        distance <- c(distance, scores[4])

        # ----------- print progress -----------
        if(i %% 10 == 0){
            svMisc::progress(i*100/num.iterations)
        }

    }

    print("Progress: Simulations Complete")
    print("Progress: Applying Clustering Techniques...")

    df.scores <- data.frame(distance = distance, distance.wr = distance.wr,
                            train.performance = train.performance,
                            test.performance = test.performance)

    diagnose_plot(df.scores, dataset.name, "distance", response.var, num.iterations, initial.scores, model.relation, dir)
    diagnose_plot(df.scores, dataset.name, "distance.wr", response.var, num.iterations, initial.scores, model.relation, dir)
}
