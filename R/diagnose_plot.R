diagnose_plot <- function(df.scores, dataset.name, id.variable, response.var, num.iterations, initial.scores, model.relation, dir){

    print("Progress: Applying Clustering Techniques...")

    avg.aic <- (sum(df.scores$train.performance) + sum(df.scores$test.performance))/(2*num.iterations)

    # melt data on performance
    df.scores.melt <- reshape2::melt(df.scores[c(id.variable,
                                                 'train.performance',
                                                 'test.performance')],
                                     id.vars = id.variable,
                                     value.name = "performance")[c(1, 3)]

    # scale data, and store center and scale values of distance variable
    df.scores.melt.scaled <- scale(df.scores.melt)
    distance.scale <- attr(df.scores.melt.scaled,"scaled:scale")[[id.variable]]
    distance.center <- attr(df.scores.melt.scaled,"scaled:center")[[id.variable]]

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

    print("Progress: Clustering Complete")
    print("Progress: Plotting Graphs...")

    # ------------------------------- PLOT GRAPH -------------------------------

    df.melt <- get_df_melt(df.scores[[id.variable]], df.scores$train.performance, df.scores$test.performance)
    simulation.plot <- get_plot(num.iterations,
                                df.melt,
                                paste('Simulation Plot'),
                                'Normalized AIC Score',
                                'Modified Mahalanobis Distance') +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[4],
                                         y = initial.scores[1],
                                         colour = 'Train (Given Split)',
                                         size = 1),
                            shape = 17) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[4],
                                         y = initial.scores[2],
                                         colour = 'Test (Given Split)',
                                         size = 1),
                            shape = 17) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold),
                            linetype = "dashed",
                            color = "#d9534f") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold,
                                        y = avg.aic,
                                        label = paste("\nThreshold =", round(distance.threshold, 3))),
                           size = 3, colour = "#d9534f", angle = 90, fontface = "bold") +
        ggplot2::guides(size = "none")


    # plot clusters
    cluster.plot <- factoextra::fviz_cluster(dbs,
                                             df.scores.melt,
                                             geom = "point",
                                             ellipse.type = "convex",
                                             pointsize = 1 ,
                                             palette = "Set2")

    # plot threshold line along with the cluster plots
    cluster.plot <- cluster.plot +
        ggplot2::geom_vline(ggplot2::aes(xintercept = distance.threshold.scaled),
                            linetype = "dashed",
                            color = "#d9534f") +
        ggplot2::geom_text(ggplot2::aes(x = distance.threshold.scaled, y = 0.8,
                                        label = paste("\nScaled Threshold =", round(distance.threshold.scaled, 3))),
                           size = 3, colour = "#d9534f", angle = 90, fontface = "bold") +
        ggplot2::xlab("Scaled Modified Mahalanobis Distance") +
        ggplot2::ylab("Scaled Normalized AIC Score") +
        ggplot2::ggtitle(paste("Cluster Plot")) +
        ggplot2::theme_bw() +
        ggplot2::guides(size = "none") +
        ggplot2::theme(panel.spacing = ggplot2::unit(1.5, "in"), legend.position = "bottom",
                       axis.text = ggplot2::element_text(size = 6),
                       axis.title = ggplot2::element_text(size = 7),
                       legend.text = ggplot2::element_text(size = 6),
                       title = ggplot2::element_text(size = 7) ) +
        ggplot2::guides(colour = ggplot2::guide_legend( override.aes = list(size = 3)))





    title <- paste(dataset.name, "with", deparse(model.relation), "(Distance includes Response Variable)")
    filename.suffix <- ""
    if (id.variable == 'distance.wr'){
        title <- paste(dataset.name, "with", deparse(model.relation), "(Distance excludes Response Variable)")
        filename.suffix <- "_wr"
    }



    arranged_plot <- ggpubr::ggarrange(simulation.plot, cluster.plot, ncol = 2, nrow = 1)
    arranged_plot <- ggpubr::annotate_figure(arranged_plot,
                                             top = ggpubr::text_grob(title,
                                                                     color = "red",
                                                                     face = "bold",
                                                                     size = 9))


    filename <- paste0(dir, '/', deparse(model.relation), '_diagnose', filename.suffix, '.png')
    ggplot2::ggsave(filename, plot = arranged_plot, device = 'png', width = 9, height = 4, dpi = 200, bg = "white")

}
