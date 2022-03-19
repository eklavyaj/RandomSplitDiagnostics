find_threshold <- function(dataset.name,
                           df.train,
                           df.test,
                           num.bootstrap = 1000,
                           alpha = 0.05,
                           save.plots = TRUE,
                           output.dir = "Output"){

    initial.distance <- get_distance(df.train, df.test)

    n.train <- nrow(df.train)
    n.test <- nrow(df.test)

    d.vec <- rep(0, num.bootstrap)

    for (i in 1:num.bootstrap){

        train.idx = sample.int(n.train, size = n.train, replace = TRUE)
        df.train.resample <- df.train[train.idx, ]

        test.idx = sample.int(n.test, size = n.test, replace = TRUE)
        df.test.resample <- df.test[test.idx, ]

        d <- get_distance(df.train.resample, df.test.resample)
        d.vec[i] = d
    }

    thresholds <- get_threshold(d.vec, alpha)

    c1 <- min(thresholds)
    c2 <- max(thresholds)


    if (initial.distance >= c1 & initial.distance <= c2){
        subtitle <- paste("Null Hypothesis with alpha =", alpha, "accepted since |c1| < |d| < |c2|")
    } else{
        if (initial.distance <= c1){
            subtitle <- paste("Null Hypothesis with alpha =", alpha, "rejected since |d| <= |c1|")
        } else{
            subtitle <- paste("Null Hypothesis with alpha =", alpha, "rejected since |d| >= |c2|")
        }
    }

    df.distance <- data.frame(distance = d.vec)

    density.plot <- ggplot2::ggplot(df.distance, ggplot2::aes(x = distance)) +
        ggplot2::geom_density()

    df.to_fill <- data.frame(x = ggplot2::ggplot_build(density.plot)$data[[1]]$x,
                             y = ggplot2::ggplot_build(density.plot)$data[[1]]$y)

    threshold.plot <- density.plot +
        ggplot2::geom_area(data = df.to_fill[df.to_fill$x <= c1, ],
                           ggplot2::aes(x = x, y = y),
                           fill = "#D16103", alpha = 0.3) +
        ggplot2::geom_area(data = df.to_fill[df.to_fill$x >= c2, ],
                           ggplot2::aes(x = x, y = y),
                           fill = "#D16103", alpha = 0.3 ) +
        ggplot2::geom_area(data = df.to_fill[df.to_fill$x <= c2 & df.to_fill$x >= c1, ],
                           ggplot2::aes(x = x, y = y),
                           fill = "#4E84C4", alpha = 0.3) +
        ggplot2::scale_fill_manual(name = "Regions",
                                    values = c("Acceptance" = "#CDE2FA",
                                               "Rejection" = "#FFB16F")) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = thresholds[1], colour = "Threshold 1 (c1)"),
                            linetype="dashed", size = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = thresholds[2], colour = "Threshold 2 (c2)"),
                            linetype="dashed", size = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = initial.distance, colour = "Distance (d)"),
                            linetype="dashed", size = 1) +
        ggplot2::scale_color_manual(name = "Distances",
                                    values = c("Threshold 1 (c1)" = "#006CFF",
                                               "Threshold 2 (c2)" = "#FF0000",
                                               "Distance (d)" = "#03B621")) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = "Modified Mahalanobis Distance", y = "Density",
                      title = "Parameter Evaluation Plot",
                      subtitle = subtitle,
                      caption = paste("Distance Values :","c1 =", round(c1, 2),
                                      ", d =", round(initial.distance, 2),
                                      ", c2 =", round(c2, 2))) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(color = "red"))

    threshold.plot

    if (save.plots){
        filename <- paste0(output.dir, "/", dataset.name, "_threshold_plot.pdf")
        ggplot2::ggsave(filename, plot = threshold.plot, bg = "white")
        print(paste("Threshold Plot saved @", filename))
    }

}
