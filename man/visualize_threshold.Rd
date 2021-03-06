% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize_threshold.R
\name{visualize_threshold}
\alias{visualize_threshold}
\title{Title}
\usage{
visualize_threshold(
  dataset.name,
  d.vec,
  df.train,
  df.test,
  model.relation = "",
  initial.scores,
  alpha = 0.05,
  save.plots = TRUE,
  output.dir = "Output"
)
}
\arguments{
\item{dataset.name}{Name of the Dataset (String)}

\item{d.vec}{Vector containing the mahalanobis distance based metric across different train-test splits}

\item{df.train}{Train Partition (R DataFrame)}

\item{df.test}{Test Partition (R DataFrame)}

\item{model.relation}{The relation used for regression model}

\item{initial.scores}{The performance metric value for the given initial train-test split}

\item{alpha}{The level of the test for visualize_threshold, default set to 0.05}

\item{save.plots}{Saves plots in output.dir when set to TRUE}

\item{output.dir}{The path to output directory the plots are saved to}
}
\value{

}
\description{
Title
}
