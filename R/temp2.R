# library(ggplot2)
# dir.create("Rsq Plots")
#
# get_rsq <- function(preds, actual){
#     rss <- sum((preds - actual) ^ 2)  ## residual sum of squares
#     tss <- sum((actual - mean(actual)) ^ 2)  ## total sum of squares
#     rsq_train <- 1 - rss/tss
# }
#
#
# data(abalone)
#
# # ------------------------------------- normal ---------------------------------
#
# set.seed(69)
# s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.7), replace = F)
# df.train <- abalone[s, ]
# df.test <- abalone[-s, ]
#
# model.relation <- WholeWeight ~ Height
# model <- lm(model.relation, data = df.train)
#
# preds.train <- predict(model, df.train)
# actual.train <- df.train$WholeWeight
#
# preds.test <- predict(model, df.test)
# actual.test <- df.test$WholeWeight
#
#
# rsq.train <- get_rsq(preds.train, actual.train)
# rsq.test <- get_rsq(preds.test, actual.test)
#
# df.train$preds <- preds.train
# df.test$preds <- preds.test
#
# p1 <- ggplot(df.train, aes(Height, WholeWeight)) +
#     geom_point(color = "#4E84C4")+
#     geom_line(data = df.train, aes(x = Height, y = preds), color = "#D16103") +
#     theme_bw() +
#     theme(legend.position = "none", axis.title = element_text(), plot.subtitle = element_text(color = "red"), axis.text = element_text(size = 13)) +
#     labs(subtitle = paste("R-squared =", round(rsq.train, 3)),
#          title = "Train Set - I",
#          x = "Height",
#          y = "Whole Weight",
#          caption = "Source: Abalone") + xlab("Height")
#
#
# p2 <- ggplot(df.test, aes(Height, WholeWeight)) +
#     geom_point(color = "#4E84C4")+
#     geom_line(data = df.test, aes(x = Height, y = preds), color = "#D16103") +
#     theme_bw() +
#     theme(legend.position = "none", axis.title = element_text(), plot.subtitle = element_text(color = "red"), axis.text = element_text(size = 13)) +
#     labs(subtitle = paste("R-squared =", round(rsq.test, 3)),
#          title = "Test Set - I",
#          x = "Height",
#          y = "Whole Weight",
#          caption = "Source: Abalone")
#
# print(p1)
# print(p2)
#
# ggsave("Rsq Plots/train_normal.eps", plot = p1)
# ggsave("Rsq Plots/test_normal.eps", plot = p2)
#
# # ----------------------------------- rsq drop --------------------------------------
#
# set.seed(5)
# s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.7), replace = F)
# df.train <- abalone[s, ]
# df.test <- abalone[-s, ]
#
#
# model.relation <- WholeWeight ~ Height
# model <- lm(model.relation, data = df.train)
#
# preds.train <- predict(model, df.train)
# actual.train <- df.train$WholeWeight
#
# preds.test <- predict(model, df.test)
# actual.test <- df.test$WholeWeight
#
#
# rsq.train <- get_rsq(preds.train, actual.train)
# rsq.test <- get_rsq(preds.test, actual.test)
#
# df.train$preds <- preds.train
# df.test$preds <- preds.test
#
# p1 <- ggplot(df.train, aes(Height, WholeWeight)) +
#     geom_point(color = "#4d7cb7")+
#     geom_line(data = df.train, aes(x = Height, y = preds), color = "#FFA500") +
#     theme_bw() +
#     theme(legend.position = "none", axis.title = element_text(), plot.subtitle = element_text(color = "red"), axis.text = element_text(size = 13)) +
#     labs(subtitle = paste("R-squared =", round(rsq.train, 3)),
#          title = "Train Set - II",
#          x = "Height",
#          y = "Whole Weight",
#          caption = "Source: Abalone")
#
#
# p2 <- ggplot(df.test, aes(Height, WholeWeight)) +
#     geom_point(color = "#4d7cb7")+
#     geom_line(data = df.test, aes(x = Height, y = preds), color = "#FFA500") +
#     theme_bw() +
#     theme(legend.position = "none", axis.title = element_text(), plot.subtitle = element_text(color = "red"), axis.text = element_text(size = 13)) +
#     labs(subtitle = paste("R-squared =", round(rsq.test, 3)),
#          title = "Test Set - II",
#          x = "Height",
#          y = "Whole Weight",
#          caption = "Source: Abalone")
#
# print(p1)
# print(p2)
#
# ggsave("Rsq Plots/train_drop.eps", plot = p1)
# ggsave("Rsq Plots/test_drop.eps", plot = p2)
#
#
