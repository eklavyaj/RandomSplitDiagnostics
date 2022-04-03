#' Title
#'
#' @param df.scores Dataframe consisting of metric performance scores for different train-test splits
#' @param dataset.name Name of the Dataset (String)
#' @param initial.scores The performance metric value for the given initial train-test split
#' @param model.relation The relation used for regression model
#' @param save.plots Saves plots in output.dir when set to TRUE
#' @param output.dir The path to output directory the plots are saved to
#' @param metric.performance The performance metric, usually Normalized AIC
#'
#' @return
#'
#'
visualize_clusters <- function(df.scores,
                               dataset.name,
                               initial.scores,
                               model.relation,
                               save.plots = TRUE,
                               output.dir,
                               metric.performance = "Normalized AIC"){

    # melt data on performance
    df.scores.melt <- reshape2::melt(df.scores,
                                     id.vars = "Distance",
                                     value.name = "Performance")[c(1, 3)]

    # scale data, and store center and scale values of distance variable
    df.scores.melt.scaled <- scale(df.scores.melt)

    # apply clustering algorithm: DBScan with eps = 0.4 (based on experimentation)
    dbs <- fpc::dbscan(df.scores.melt.scaled, eps = 0.4, MinPts = 10)

    # add cluster id to main data frame
    df.scores.melt.new <- df.scores.melt
    df.scores.melt.new$cluster <- dbs$cluster

    df.scores.melt.new = df.scores.melt.new[df.scores.melt.new$cluster != 0, ]
    # plot clusters
    cluster.plot <- ggplot2::ggplot(df.scores.melt.new, ggplot2::aes(Distance, Performance,
                                                                     color = as.factor(cluster))) +
        ggplot2::geom_point()  +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[1],
                                         color = 'Train (Initial Split)'),
                                         size = 5, shape = 18) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[2],
                                         color = 'Test (Initial Split)'),
                                         size = 5, shape = 18) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = latex2exp::TeX("Distance $(\\Lambda)$"),
                      y = metric.performance,
                      title = "Cluster Plot",
                      subtitle = paste(dataset.name, "with", deparse(model.relation))) +
        ggplot2::guides(size = "none",
                        shape = "none",
                        colour = ggplot2::guide_legend(title = "Cluster")) +
        ggplot2::theme(text = ggplot2::element_text(size = 8),
                       plot.subtitle = ggplot2::element_text(size = 10),
                       title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(size = 10),
                       axis.text = ggplot2::element_text(size = 7),
                       legend.title = ggplot2::element_text(face = "bold"))


    print(cluster.plot)

    if (save.plots){
        filename <- paste0(output.dir, "/", dataset.name, "_cluster_plot.pdf")
        ggplot2::ggsave(filename, plot = cluster.plot, bg = "white", width = 7.08, height = 3.94)
        print(paste("Cluster Plot saved @", filename))
    }

}
