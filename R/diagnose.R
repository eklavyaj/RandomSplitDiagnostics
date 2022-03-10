#' Diagnose the Random Split
#'
#' @param dataset.name Name of the dataset (string)
#' @param df.train Train set corresponding to the initial split performed by the user
#' @param df.test Test set corresponding to the initial split performed by the user
#' @param model.relation The Regression Model to be fitted on the training data
#'
#' @return
#' 1. Decision regarding the randomness of the split.
#' 2. Decision regarding the performance of the model.
#' 3. Supporting Plots of Mahalanobis Distance vs. Akaike Information Criterion.
#' @export
#'
#' @examples
#'
#' # ---------------- Example 1 ----------------
#'
#' # data preparation
#' dataset.name <- "Abalone"
#' data(abalone)
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.7), replace = F)
#' df.train <- abalone[s, ]
#' df.test <- abalone[-s, ]
#'
#' # defining model relation based on variables of data
#' model.relation <- WholeWeight ~ Height + LongestShell + Diameter
#'
#' # function call
#' diagnose(dataset.name, df.train, df.test, model.relation)
#'
#' # ---------------- Example 2 ----------------
#'
#' # data preparation
#' dataset.name <- "Diamonds"
#' data(diamonds)
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(diamonds), size = floor(nrow(diamonds)*0.7), replace = F)
#' df.train <- diamonds[s, ]
#' df.test <- diamonds[-s, ]
#'
#' # defining model relation based on variables of data
#' model.relation <- price ~ x:y:z + depth
#'
#' # function call
#' diagnose(dataset.name, df.train, df.test, model.relation)
#'
#'
diagnose <- function(dataset.name, df.train, df.test, model.relation) {

    dir.create("Output")
    dir.create(paste0("Output", "/", dataset.name))
    if (!file.exists(paste0("Output", "/", dataset.name, "/Simulation_0"))){
        dir.create(paste0("Output", "/", dataset.name, "/Simulation_0"))
        n <- 0
    } else {
        dir_list <- list.dirs(paste0("Output", "/", dataset.name), full.names = FALSE, recursive = FALSE)
        n <- max(as.numeric(gsub(".*?([0-9]+)", "\\1", dir_list))) + 1
        dir.create(paste0("Output", "/", dataset.name, "/Simulation_", n))
    }

    dir <- paste0("Output", "/", dataset.name, "/Simulation_", n)
    response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])

    n1 <- nrow(df.train)
    n2 <- nrow(df.test)
    n <- n1 + n2
    split.percentage <- n1/n

    initial.scores <- get_scores(df.train, df.test, response.var, model.relation)

    df <- rbind(df.train, df.test)
    num.iterations <- 200

    diagnose_split(dataset.name, num.iterations, df, split.percentage, response.var, model.relation, initial.scores, dir)

}
