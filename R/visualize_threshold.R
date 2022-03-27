visualize_threshold <- function(dataset.name,
                                d.vec,
                                df.train,
                                df.test,
                                model.relation = "",
                                initial.scores,
                                alpha = 0.05,
                                save.plots = TRUE,
                                output.dir = "Output"){

    initial.distance <- initial.scores[3]

    c <- get_one_sided_threshold(d.vec, alpha)

    if (initial.distance >= c){
        subtitle <- paste("Null Hypothesis Rejected")
    } else{
        subtitle <- paste("Null Hypothesis Accepted")
    }

    df.distance <- data.frame(distance = d.vec)

    density.plot <- ggplot2::ggplot(df.distance, ggplot2::aes(x = distance)) +
        ggplot2::geom_density()

    df.to_fill <- data.frame(x = ggplot2::ggplot_build(density.plot)$data[[1]]$x,
                             y = ggplot2::ggplot_build(density.plot)$data[[1]]$y)


    str1 <- paste("Threshold = ", round(c, 2))
    str2 <- paste("Initial Distance = ", round(initial.distance, 2))
    caption <- paste(str1, ",",str2)
    threshold.plot <- density.plot +
        ggplot2::geom_area(data = df.to_fill[df.to_fill$x <= c, ],
                           ggplot2::aes(x = x, y = y),
                           fill = "#4E84C4", alpha = 0.3) +
        ggplot2::geom_area(data = df.to_fill[df.to_fill$x >= c, ],
                           ggplot2::aes(x = x, y = y),
                           fill = "#D16103", alpha = 0.3) +
        ggplot2::scale_fill_manual(name = "Regions",
                                   values = c("Acceptance" = "#CDE2FA",
                                              "Rejection" = "#FFB16F")) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = c, colour = "Rejection Threshold"),
                            linetype="dashed", size = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = initial.distance, colour = "Initial Distance"),
                            linetype="dashed", size = 1) +
        ggplot2::scale_color_manual(name = "Distances",
                                    values = c("Rejection Threshold" = "#FF0000",
                                               "Initial Distance" = "#03B621")) +
        ggplot2::theme_bw() +
        ggplot2::labs(x = latex2exp::TeX("Distance $(\\Lambda)$"),
                      y = "Density",
                      title = "Parameter Evaluation Plot" ,
                      subtitle = paste(subtitle, ':', caption)) +
        ggplot2::theme(plot.subtitle = ggplot2::element_text(color = "red", size = 9),
                       title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(size = 10),
                       axis.text = ggplot2::element_text(size = 7),
                       legend.title = ggplot2::element_text(face = "bold"))


    print(threshold.plot)


    if (save.plots){
        filename <- paste0(output.dir, "/", dataset.name, "_threshold_plot.pdf")
        ggplot2::ggsave(filename, plot = threshold.plot, bg = "white")
        print(paste("Threshold Plot saved @", filename))
    }

}
