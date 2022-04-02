visualize_simulation <- function(df.scores,
                                 dataset.name,
                                 initial.scores,
                                 model.relation,
                                 save.plots = TRUE,
                                 output.dir,
                                 metric.performance = "Normalized AIC"){


    num.simulations <- nrow(df.scores)
    df.melt <- reshape2::melt(df.scores, id.vars = 'Distance',
                              variable.name = 'PerformanceType',
                              value.name = 'PerformanceValue')


    simulation.plot <- ggplot2::ggplot(df.melt, ggplot2::aes(Distance, PerformanceValue,
                                                             col = PerformanceType)) +
        ggplot2::geom_text(label = rep(1:num.simulations, times = 2), size = 2) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[1],
                                         colour = 'Train (Initial Split)'),
                                         size = 5, shape = 18) +
        ggplot2::geom_point(ggplot2::aes(x = initial.scores[3],
                                         y = initial.scores[2],
                                         colour = 'Test (Initial Split)'),
                                         size = 5, shape = 18) +
        ggplot2::labs(x = latex2exp::TeX("Distance $(\\Lambda)$"),
                      y = metric.performance,
                      title = "Simulation Plot",
                      subtitle = paste(dataset.name, "with", deparse(model.relation))) +
        ggplot2::scale_color_manual(values = c( "#08bdba", "#da1e28",  "#ffb635", "#6929c4")) +
        ggplot2::theme_bw() +
        ggplot2::guides(size = "none",
                        colour = ggplot2::guide_legend(title = "Set",
                                                       override.aes = list(size = 3, shape = 16))) +
        ggplot2::theme(text = ggplot2::element_text(size = 8),
                       plot.subtitle = ggplot2::element_text(size = 10),
                       title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(size = 10),
                       axis.text = ggplot2::element_text(size = 7),
                       legend.title = ggplot2::element_text(face = "bold"))

    print(simulation.plot)

    if (save.plots){
        filename <- paste0(output.dir, "/", dataset.name, "_simulation_plot.pdf")
        ggplot2::ggsave(filename, plot = simulation.plot, bg = "white", width = 7.08, height = 3.94)
        print(paste("Simulation Plot saved @", filename))
    }

}
