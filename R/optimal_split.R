#' A function to find the optimal split ratio along with the split mask for a given dataset and model
#'
#' @param dataset.name Name of the Dataset
#' @param dataset Dataset as a data.frame
#' @param model.relation Regression model in R format
#' @param min_test_size a numeric value in [0.0, 1.0] denoting the minimum percentage of entries in test set, by default 0.1
#'
#' @return
#' 1. Split Ratio/Percentage.
#' 2. Optimal Dataset split with optimal split ratio/percentage.
#' 3. Supporting Plots of Mahalanobis Distance vs. Akaike Information Criterion.
#' @export
#'
#' @examples
#'
#' # ------------ Example 1 ------------
#' # data preparation
#' dataset.name <- "Abalone"
#' data(abalone)
#'
#' # defining model relation based on variables of data
#' model.relation <- WholeWeight ~ Height + LongestShell + Diameter
#'
#' # function call
#' find_optimal_split(dataset.name, abalone, model.relation, min_test_size)
#'
find_optimal_split <- function(dataset.name, dataset, model.relation, min_test_size){

    response.var <- stringr::str_trim(strsplit(deparse(model.relation), "\\~")[[1]][1])

    num.iterations <- 200
    generate_plot_optimal(dataset.name, dataset, model.relation, min_test_size, response.var, num.iterations)

}
