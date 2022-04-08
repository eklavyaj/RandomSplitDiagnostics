#' Diagnose the random split
#'
#' @param dataset.name Name of the Dataset (String)
#' @param df.train Train Partition (R DataFrame)
#' @param df.test Test Partition (R DataFrame)
#' @param model.relation The relation used for regression model
#' @param metric.performance The performance metric, usually Normalized AIC
#' @param num.simulations Number of simulations, defaults to 200
#' @param alpha The level of the test for visualize_threshold, default set to 0.05
#' @param save.plots Saves plots in output.dir when set to TRUE
#' @param output.dir The path to output directory the plots are saved to
#'
#' @return The following three plots are plotted:
#'
#' @export
#'
#' @examples
#'
#' # ------------------------- Example 1 ------------------------------
#' # set.seed(19) # accepted
#' # set.seed(20) # rejected
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
#' model.relation <- Rings ~ LongestShell + Diameter + Height
#'
#' # function call
#' diagnose(dataset.name, df.train, df.test, model.relation = model.relation,
#'  metric.performance = "Normalized AIC", num.simulations = 200,
#'   alpha = 0.05, save.plots = TRUE, output.dir = "Output")
#'
#' #function call with metric.performance as R Squared, alpha set to 0.06, 400 simulations, unsaved plots and model.relation for regression as WholeWeight ~ LongestShell + Diameter
#' diagnose(dataset.name, df.train, df.test, model.relation = WholeWeight ~ LongestShell + Diameter,
#'  metric.performance = "R Squared", num.simulations = 400,
#'   alpha = 0.06, save.plots = FALSE, output.dir = "Output")
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
#' diagnose(dataset.name, df.train, df.test, model.relation = model.relation,
#'  metric.performance = "Normalized AIC", num.simulations = 200,
#'   alpha = 0.05, save.plots = TRUE, output.dir = "Output")
#'
diagnose <- function(dataset.name,
                     df.train,
                     df.test,
                     model.relation = "",
                     metric.performance = "Normalized AIC",
                     num.simulations = 200,
                     alpha = 0.05,
                     save.plots = TRUE,
                     output.dir = "Output") {

    # validating metric.performance input values
    if (model.relation != "" && metric.performance != "Normalized AIC" && metric.performance != "R Squared" ){
        stop("Error in metric.performance needs to be set to Normalized AIC or R Squared")
    }

    # validating alpha input values
    if (alpha > 1 || alpha < 0){
        stop("Error in value of alpha needs to be in the range 0 <= alpha <=  1")
    }

    if(model.relation != ""){
        # validating model.relation response variable input values
        response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])
        if(is.null(df.test[[response.var]]) || is.null(df.train[[response.var]])){
            stop("Error in provided response variable in model.relation doesn't exist in test/train dataframe")
        }

        # validating model.relation variable input values
        tryCatch(
            expr = {
                model.matrix(model.relation, df.test)
            },
            error = function(e){
                stop(e)
            }
        )

        tryCatch(
            expr = {
                model.matrix(model.relation, df.train)
            },
            error = function(e){
                stop(e)
            }
        )
    }

    # creating directories if required
    if (save.plots){

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

    # calculating split.percentage for given input split
    n.train <- nrow(df.train)
    n.test <- nrow(df.test)
    n.total <- n.train + n.test
    split.percentage <- n.train/n.total

    ncol.train <- ncol(df.train)
    ncol.test <- ncol(df.test)

    if(ncol.train != ncol.test){
        stop("Error in columns of train and test are unequal")
    }

    # calculating table data
    table.data <- c(n.total, ncol.train)

    # calculating initial scores to plot on graphs
    if(model.relation != ""){
        initial.scores <- get_scores(df.train, df.test, model.relation, metric.performance)
    }
    else{
        initial.dist <- calculate_distance(df.train, df.test)
        initial.scores <- c(0,0,initial.dist)
    }


    split.conclusion <- simulate(dataset.name,
             df.train,
             df.test,
             num.simulations,
             model.relation,
             split.percentage,
             initial.scores,
             alpha,
             save.plots,
             output.dir,
             metric.performance)


    if(model.relation != ""){
        table.data <- c(table.data, round(initial.scores[3], 3), deparse(model.relation), metric.performance, round(initial.scores[1], 3), round(initial.scores[2], 3), split.conclusion)
        table.rows <- c('No. of rows in dataset','No. of columns in dataset','Mahalanobis distance based metric for given split','Model Relation','Model Performance metric','Model Performance for given train split','Model performance for given test split','Split Conclusion')
        table.data <- data.frame(
            Attribute = table.rows,
            Value = table.data
        )
    }
    else{
        table.data <- c(table.data, round(initial.scores[3], 3), split.conclusion)
        table.rows <- c('No. of rows in dataset','No. of columns in dataset','Mahalanobis distance based metric for given split','Split Conclusion')
        table.data <- data.frame(
            Attribute = table.rows,
            Value = table.data
        )
    }

    if (save.plots){
        filename <- paste0(output.dir, "/", dataset.name, "_stats.csv")
        write.csv(table.data, file = filename, row.names = FALSE)
    }

    print(table.data)
}

