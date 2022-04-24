#' Title
#'
#' @param dataset.name Name of the Dataset (String)
#' @param d.vec Vector containing the mahalanobis distance based metric across different train-test splits
#' @param df.train Train Partition (R DataFrame)
#' @param df.test Test Partition (R DataFrame)
#' @param model.relation The relation used for regression model
#' @param initial.scores The performance metric value for the given initial train-test split
#' @param alpha The level of the test for visualize_threshold, default set to 0.05
#' @param save.plots Saves plots in output.dir when set to TRUE
#' @param output.dir The path to output directory the plots are saved to
#'
#' @return
#'
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
    p.val <- get_pval(d.vec, initial.distance)

    if (initial.distance > c){
        subtitle <- paste("Rejected")
    } else {
        subtitle <- paste("Accepted")
    }

    df.distance <- data.frame(distance = d.vec)

    density.plot <- ggplot2::ggplot(df.distance, ggplot2::aes(x = distance)) +
        ggplot2::geom_density()

    df.to_fill <- data.frame(x = ggplot2::ggplot_build(density.plot)$data[[1]]$x,
                             y = ggplot2::ggplot_build(density.plot)$data[[1]]$y)


    str1 <- paste0("p-value = ", round(p.val, 4))
    str2 <- paste0("Threshold = ", round(c, 2))
    str3 <- paste0("Test Statistic = ", round(initial.distance, 2))
    caption <- paste0(str1, ", ", str2, ", ", str3)
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
        ggplot2::geom_vline(ggplot2::aes(xintercept = c, colour = "Threshold"),
                            linetype="dashed", size = 1) +
        ggplot2::geom_vline(ggplot2::aes(xintercept = initial.distance, colour = "Test Statistic"),
                            linetype="dashed", size = 1) +
        ggplot2::scale_color_manual(name = "Distances",
                                    values = c("Threshold" = "#FF0000",
                                               "Test Statistic" = "#03B621")) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = latex2exp::TeX("Distance $(\\Lambda)$"),
                      y = "Density",
                      title = "Hypothesis Testing Plot" ,
                      subtitle = paste(subtitle, ':', caption)) +
        ggplot2::theme(text = ggplot2::element_text(size = 8),
                       plot.subtitle = ggplot2::element_text(color = "red", size = 10),
                       title = ggplot2::element_text(size = 12),
                       legend.text = ggplot2::element_text(size = 10),
                       axis.text = ggplot2::element_text(size = 9),
                       legend.title = ggplot2::element_text(face = "bold"),
                       legend.box = "vertical",
                       axis.line = ggplot2::element_line(size = 0.5))

    print(threshold.plot)
    return(threshold.plot)
}
