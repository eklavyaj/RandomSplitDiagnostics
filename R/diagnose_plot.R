diagnose <- function(dataset.name, num.iterations, df, split.percentage,
                     response.var, model.relation, initial.scores){

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

    avg.aic <- (sum(train.performance) + sum(test.performance))/(2*num.iterations)


    # ----------------------------- with response ------------------------------
    # ----------------------------- with response ------------------------------

    # melt data on performance
    df.scores.melt <- reshape2::melt(df.scores[c('distance',
                                                 'train.performance',
                                                 'test.performance')],
                                     id.vars = "distance",
                                     value.name = "performance")[c(1, 3)]

    # scale data, and store center and scale values of distance variable
    df.scores.melt.scaled <- scale(df.scores.melt)
    distance.scale <- attr(df.scores.melt.scaled,"scaled:scale")[['distance']]
    distance.center <- attr(df.scores.melt.scaled,"scaled:center")[['distance']]

    # apply clustering algorithm: DBScan with eps = 0.4 (based on experimentation)
    dbs <- fpc::dbscan(df.scores.melt.scaled, eps = 0.4, MinPts = 10)

    # add cluster id to main data frame
    df.scores.melt.new <- df.scores.melt
    df.scores.melt.new$cluster <- dbs$cluster

    # construct new data frame with cluster details
    cluster.means <- aggregate(df.scores.melt.new[1],
                               list(df.scores.melt.new$cluster),
                               mean)
    cluster.sizes <- aggregate(df.scores.melt.new[1],
                               list(df.scores.melt.new$cluster),
                               length)
    df.scores.melt.cluster <- cbind(cluster.means,
                                    cluster.sizes)[, c(1, 2, 4)]
    colnames(df.scores.melt.cluster) <- c('cluster',
                                          'cluster.mean.distance',
                                          'cluster.size')

    # remove outliers clusters
    df.scores.melt.cluster <- subset(df.scores.melt.cluster,
                                     df.scores.melt.cluster$cluster != 0)

    # get the two farthest clusters along the distance dimension
    row.max <- df.scores.melt.cluster[which.max(df.scores.melt.cluster$cluster.mean.distance), ]
    row.min <- df.scores.melt.cluster[which.min(df.scores.melt.cluster$cluster.mean.distance), ]

    # calculate the weighted average of the distance between the two farthest clusters
    x = c(row.max$cluster.mean.distance, row.min$cluster.mean.distance)
    wt = c(row.max$cluster.size, row.min$cluster.size)
    distance.threshold <- weighted.mean(x, wt)
    distance.threshold.scaled <- (distance.threshold - distance.center)/distance.scale


    # ------------------------------- PLOT GRAPH -------------------------------

    # plot clusters
    cluster.plot <- factoextra::fviz_cluster(dbs,
                                             df.scores.melt,
                                             geom = "point",
                                             ellipse.type = "convex")

    # plot threshold line along with the cluster plots
    cluster.plot <- cluster.plot +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold.scaled),
                            linetype = "dashed",
                            color = "#4d194d") +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 4),
                       legend.text = ggplot2::element_text(size = 6),
                       title = ggplot2::element_text(size = 6),
                       legend.position = "bottom") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold.scaled,
                                        y = 0.8,
                                        label = paste("\nScaled Threshold =",round(distance.threshold.scaled, 3)),
                                        size = 1),
                           colour="#4d194d",
                           angle=90,
                           fontface = "plain",
                           inherit.aes = FALSE) +
        ggplot2::xlab("Scaled Modified Mahalanobis Distance") +
        ggplot2::ylab("Scaled Normalized AIC Score") +
        ggplot2::ggtitle(paste("Cluster Plot ( Distance with", response.var, ")")) +
        ggplot2::theme_bw() +
        # ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4))) +
        ggplot2::guides(size = "none")

    df.melt <- get_df_melt(distance, train.performance, test.performance)
    simulation.plot <- get_plot(num.iterations,
                                df.melt,
                                paste('Simulation Plot ( Distance with', response.var, ')'),
                                'Normalized AIC Score',
                                'Modified Mahalanobis Distance') +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[4],
                                         y = initial.scores[1],
                                         colour = 'Train (Given Split)',
                                         size = 2), shape = 17) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[4],
                                         y = initial.scores[2],
                                         colour = 'Test (Given Split)',
                                         size = 2), shape = 17) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold),
                            linetype = "dashed",
                            color = "#4d194d") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold,
                                        y = avg.aic,
                                        label = paste("\nThreshold =", round(distance.threshold, 3)),
                                        size = 2),
                           colour="#4d194d",
                           angle=90,
                           fontface = "plain",
                           inherit.aes = FALSE) +
        ggplot2::guides(size = "none")


    #------------------------ without response ---------------------------------
    #------------------------ without response ---------------------------------

    # melt data on performance
    df.scores.melt <- reshape2::melt(df.scores[c('distance.wr',
                                                 'train.performance',
                                                 'test.performance')],
                                     id.vars = "distance.wr",
                                     value.name = "performance")[c(1, 3)]

    # scale data, and store center and scale values of distance variable
    df.scores.melt.scaled <- scale(df.scores.melt)
    distance.scale <- attr(df.scores.melt.scaled,"scaled:scale")[['distance.wr']]
    distance.center <- attr(df.scores.melt.scaled,"scaled:center")[['distance.wr']]

    # apply clustering algorithm - dbscan with eps = 0.4 (based on experimentation)
    dbs.wr <- fpc::dbscan(df.scores.melt.scaled, eps = 0.4, MinPts = 10)

    # add cluster id to main dataframe
    df.scores.melt.new <- df.scores.melt
    df.scores.melt.new$cluster <- dbs$cluster

    # construct new dataframe with cluster details
    cluster.means <- aggregate(df.scores.melt.new[1],
                               list(df.scores.melt.new$cluster),
                               mean)
    cluster.sizes <- aggregate(df.scores.melt.new[1],
                               list(df.scores.melt.new$cluster),
                               length)
    df.scores.melt.cluster <- cbind(cluster.means, cluster.sizes)[, c(1, 2, 4)]
    colnames(df.scores.melt.cluster) <- c('cluster',
                                          'cluster.mean.distance',
                                          'cluster.size')

    # remove outliers clusters
    df.scores.melt.cluster <- subset(df.scores.melt.cluster,
                                     df.scores.melt.cluster$cluster != 0)

    # get the two farthest clusters along the distance dimension
    row.max <- df.scores.melt.cluster[which.max(df.scores.melt.cluster$cluster.mean.distance), ]
    row.min <- df.scores.melt.cluster[which.min(df.scores.melt.cluster$cluster.mean.distance), ]

    # calculate the weighted average of the distance between the two farthest clusters
    x = c(row.max$cluster.mean.distance, row.min$cluster.mean.distance)
    wt = c(row.max$cluster.size, row.min$cluster.size)
    distance.threshold.wr <- weighted.mean(x, wt)
    distance.threshold.scaled.wr <- (distance.threshold.wr - distance.center)/distance.scale


    print("Progress: Clustering Complete")
    print("Progress: Plotting Graphs...")

    # ------------------------------- PLOT GRAPH -------------------------------

    # plot clusters
    cluster.plot.wr <- factoextra::fviz_cluster(dbs.wr,
                                                df.scores.melt,
                                                geom = "point",
                                                ellipse.type = "convex")

    # plot threshold line along with the cluster plots
    cluster.plot.wr <- cluster.plot.wr +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold.scaled.wr),
                            linetype = "dashed",
                            color = "#4d194d") +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 4),
                       legend.text = ggplot2::element_text(size = 6),
                       title = ggplot2::element_text(size = 6),
                       legend.position = "bottom") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold.scaled.wr,
                                        y = 0.8,
                                        label = paste("\nScaled Threshold =", round(distance.threshold.scaled.wr, 3)),
                                        size = 1),
                           colour = "#4d194d",
                           angle=90, fontface = "plain", inherit.aes = FALSE) +
        ggplot2::xlab("Scaled Modified Mahalanobis Distance") +
        ggplot2::ylab("Scaled Normalized AIC Score") +
        ggplot2::ggtitle(paste("Cluster Plot ( Distance without", response.var, ")")) +
        ggplot2::theme_bw() +
        # ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size = 4))) +
        ggplot2::guides(size = "none")


    df.melt.wr <- get_df_melt(distance.wr, train.performance, test.performance)
    simulation.plot.wr <- get_plot(num.iterations,
                                   df.melt.wr,
                                   paste('Simulation Plot ( Distance without', response.var, ')'),
                                   'Normalized AIC Score',
                                   'Modified Mahalanobis Distance') +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[1],
                                         colour = 'Train (Given Split)',
                                         size = 2), shape = 17) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[2],
                                         colour = 'Test (Given Split)',
                                         size = 2), shape = 17) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold.wr),
                            linetype = "dashed", color = "#4d194d") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold.wr,
                                        y = avg.aic,
                                        label = paste("\nThreshold =", round(distance.threshold.wr, 3)),
                                        size = 2),
                           colour="#4d194d",
                           angle=90,
                           fontface = "plain",
                           inherit.aes = FALSE) +
        ggplot2::guides(size = "none")


    # ----------------- arrange plot --------------------
    arranged_plot <- ggpubr::ggarrange(simulation.plot, simulation.plot.wr, cluster.plot, cluster.plot.wr, ncol = 2, nrow = 2)
    arranged_plot <- ggpubr::annotate_figure(arranged_plot,
                                             top = ggpubr::text_grob(paste(dataset.name, "with", deparse(model.relation)),
                                                                     color = "#4d194d",
                                                                     face = "bold",
                                                                     size = 18))

    # print(arranged_plot)
    print("Progress: Diagnosis Complete")
    filename <- paste0('Output', '/', dataset.name, '/', deparse(model.relation), '_diagnose.png')
    ggplot2::ggsave(filename, plot = arranged_plot, device = 'png', width = 14, height = 11, dpi = 1000, bg = "white")


}
