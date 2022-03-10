# library(ggplot2)
# library(reshape2)
# library(ggpubr)
#
# get_mnb_dist <- function(train, test){
#
#     n1 <- nrow(train)
#     n2 <- nrow(test)
#
#     cov1 <- cov(train)
#     cov2 <- cov(test)
#
#     mu1 <- colMeans(train)
#     mu2 <- colMeans(test)
#
#     pooled_cov <- matrix(((n1-1)*cov1 + (n2-1)*cov2)/(n1 + n2 -2), ncol = ncol(train))
#
#     d1 <- mean(mahalanobis(train, mu2, pooled_cov))
#     d2 <- mean(mahalanobis(test, mu1, pooled_cov))
#
#     d <- d1 - d2
#
#     return(c(d1, d2, d))
# }
#
# get_hotelling_t2 <- function(train, test){
#
#     n1 <- nrow(train)
#     n2 <- nrow(test)
#
#     cov1 <- cov(train)
#     cov2 <- cov(test)
#
#     mu1 <- colMeans(train)
#     mu2 <- colMeans(test)
#
#     pooled_cov <- matrix(((n1-1)*cov1 + (n2-1)*cov2)/(n1 + n2 -2), ncol = ncol(train))
#     diff <- matrix(mu1 - mu2)
#
#     t2 <- (n1*n2/(n1 + n2))*(t(diff) %*% pooled_cov %*% diff)
#     return(t2)
# }
#
#
# n_sims <- 20
#
# for (k in 1:n_sims){
#
#     n_iters <- 1000
#
#     d1.vec <- c()
#     d2.vec <- c()
#     d.vec <- c()
#
#     vals <- c()
#
#     data(diamonds)
#     s <- sample(x = 1:nrow(diamonds), size = floor(nrow(diamonds)*0.8), replace = F)
#
#     train <- diamonds[s, c(7, 8, 9, 10)]
#     train.n <- nrow(train)
#     test <- diamonds[-s, c(7, 8, 9, 10)]
#     test.n <- nrow(test)
#
#     # data(abalone)
#     # s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.8), replace = F)
#     #
#     # train <- abalone[s, c(2, 3, 4, 5)]
#     # train.n <- nrow(train)
#     # test <- abalone[-s, c(2, 3, 4, 5)]
#     # test.n <- nrow(test)
#
#     for (i in 1:n_iters){
#
#         train.idx = sample.int(train.n, size = train.n, replace = TRUE)
#         x = train[train.idx, ]
#         test.idx = sample.int(test.n, size = test.n, replace = TRUE)
#         y = train[test.idx, ]
#
#
#         # val <- get_hotelling_t2(train, test)
#         vals <- get_mnb_dist(x, y)
#
#         # vals <- c(vals, val)
#         d1.vec <- c(d1.vec, vals[1])
#         d2.vec <- c(d2.vec, vals[2])
#         d.vec <- c(d.vec, vals[3])
#     }
#
#     # df <- data.frame(t2 = vals)
#     df <- data.frame(d1 = d1.vec, d2 = d2.vec, d = d.vec)
#
#     # plot(density(t2))
#
#     # p1 <- ggplot(df, aes(x = t2)) + geom_density() + theme_bw() +
#     #     ggtitle("Hotelling T^2 Distribution")
#     #
#     # ggsave(filename = "Density Plots/t2.png", plot = p1)
#
#     p1 <- ggplot(df, aes(x = d1)) + geom_density() + theme_bw() +
#         ggtitle("Distance of Train observations from Test Mean (d1^2)")
#
#     p2 <- ggplot(df, aes(x = d2)) + geom_density() + theme_bw() +
#         ggtitle("Distance of Test observations from Train Mean (d2^2)")
#
#     p3 <- ggplot(df, aes(x = d)) + geom_density() + theme_bw() +
#         ggtitle("d1^2 - d2^2")
#
#     p4.1 <- ggplot(melt(df), aes(x = value, fill = variable)) + theme_bw() +
#         geom_density(alpha = 0.5, trim = TRUE) +
#         ggtitle(" ", subtitle = "Density computed on range of the group (Trimmed)") +
#         scale_fill_discrete(name = "Distance", labels = c("d1^2", "d2^2", "d^2"))
#
#     p4.2 <- ggplot(melt(df), aes(x = value, fill = variable)) + theme_bw() +
#         geom_density(alpha = 0.5) +
#         ggtitle("Comparing densities of the three distances", subtitle = "Density computed on full range of data") +
#         scale_fill_discrete(name = "Distance", labels = c("d1^2", "d2^2", "d^2"))
#
#     # ap <- ggarrange(p4.1, p4.2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
#     # ap <- ggarrange(p4.2, ncol = 1, nrow = 1, common.legend = TRUE, legend = "bottom")
#     # ap <- annotate_figure(ap, top = ggpubr::text_grob("Comparing densities of the three distances",
#     #                                               color = "red",
#     #                                               face = "bold",
#     #                                               size = 12))
#     # print(ap)
#
#     if (file.exists("Density Plots/") == FALSE) {
#         dir.create("Density Plots")
#     }
#
#     if (!file.exists("Density Plots/Simulation_0")){
#         dir.create("Density Plots/Simulation_0")
#         n <- 0
#     } else {
#         dir_list <- list.dirs("Density Plots/",full.names = FALSE, recursive = FALSE)
#         n <- max(as.numeric(gsub(".*?([0-9]+)", "\\1", dir_list))) + 1
#         dir.create(paste0("Density Plots/Simulation_", n))
#     }
#
#     ggsave(filename = paste0("Density Plots/Simulation_", n, "/d1_density.png"), plot = p1, width = 7.17)
#     ggsave(filename = paste0("Density Plots/Simulation_", n, "/d2_density.png"), plot = p2, width = 7.17)
#     ggsave(filename = paste0("Density Plots/Simulation_", n, "/d_density.png"), plot = p3, width = 7.17)
#     ggsave(filename = paste0("Density Plots/Simulation_", n, "/all_density.png"),
#            plot = p4.2, bg = "white", width = 7.17)
#
# }
#
#
# # model.relation <- WholeWeight ~ Height + LongestShell + Diameter
# # diagnose(dataset.name, df.train, df.test, model.relation)
