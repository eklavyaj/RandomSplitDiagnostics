#' Diagnose the random split
#'
#' @param dataset.name Name of the Dataset (String)
#' @param df.train Train Partition (R DataFrame)
#' @param df.test Test Partition (R DataFrame)
#' @param flag.simulate
#' @param model.relation
#' @param metric.performance
#' @param num.simulations
#' @param flag.find_threshold
#' @param num.bootstrap
#' @param alpha
#' @param save.plots
#' @param output.dir
#'
#' @return
#' @export
#'
#' @examples
#'
#' # ------------------------- Example 1 ------------------------------
#'
#' # data preparation
#' dataset.name <- "Abalone"
#' data(abalone)
#' split.percentage <- 0.8
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*split.percentage), replace = F)
#' df.train <- abalone[s, ]
#' df.test <- abalone[-s, ]
#'
#' # defining model relation based on variables of data
#' model.relation <- WholeWeight ~ Height + LongestShell + Diameter
#'
#' # function call
#' diagnose(dataset.name, df.train, df.test, flag.simulate = TRUE,
#'  model.relation = model.relation, metric.performance = "Normalized AIC",
#'   num.simulations = 200, flag.find_threshold = TRUE, num.bootstrap = 1000,
#'    alpha = 0.05, save.plots = TRUE, output.dir = "Output")
#'
#' # ------------------------- Example 2 ------------------------------
#'
#' # data preparation
#' dataset.name <- "Diamonds"
#' data(diamonds)
#' split.percentage <- 0.8
#'
#' # intial random split of data
#' s <- sample(x = 1:nrow(diamonds), size = floor(nrow(diamonds)*split.percentage), replace = F)
#' df.train <- diamonds[s, ]
#' df.test <- diamonds[-s, ]
#'
#' # defining model relation based on variables of data
#' model.relation <- price ~ x:y:z + depth
#'
#' # function call
#' diagnose(dataset.name, df.train, df.test, flag.simulate = TRUE,
#'  model.relation = model.relation, metric.performance = "Normalized AIC",
#'   num.simulations = 200, flag.find_threshold = TRUE, num.bootstrap = 1000,
#'    alpha = 0.05, save.plots = TRUE, output.dir = "Output")
#'
diagnose <- function(dataset.name,
                     df.train,
                     df.test,
                     flag.simulate = TRUE,
                     model.relation = "",
                     metric.performance = "Normalized AIC",
                     num.simulations = 200,
                     flag.find_threshold = TRUE,
                     num.bootstrap = 1000,
                     alpha = 0.05,
                     save.plots = TRUE,
                     output.dir = "Output") {

    if (save.plots == TRUE){

        if (!file.exists(output.dir)){
            dir.create(output.dir)
        }

        if(!file.exists(file.path(output.dir, dataset.name))){
            dir.create(file.path(output.dir, dataset.name))
        }

        n.simulation <- length(list.files(file.path(output.dir, dataset.name))) + 1
        dir.create(file.path(output.dir, dataset.name, n.simulation))


        output.dir <- file.path(output.dir, dataset.name, n.simulation)
    }

    print(output.dir)

    if (flag.simulate == TRUE){

        n.train <- nrow(df.train)
        n.test <- nrow(df.test)
        n.total <- n.train + n.test
        split.percentage <- n.train/n.total

        initial.scores <- get_scores(df.train, df.test, model.relation, metric.performance)

        simulate(dataset.name,
                 df.train,
                 df.test,
                 num.simulations,
                 model.relation,
                 split.percentage,
                 initial.scores,
                 save.plots,
                 output.dir,
                 metric.performance)

    }

    if (flag.find_threshold == TRUE){
        find_threshold(dataset.name,
                       df.train,
                       df.test,
                       num.bootstrap,
                       alpha,
                       save.plots,
                       output.dir)
    }

}
