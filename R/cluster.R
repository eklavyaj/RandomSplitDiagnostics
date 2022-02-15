cluster <- function(df.scores, id.variable = "distance"){

    # melt data on performance
    # df.scores <- read.csv(filename)
    df.scores <- reshape2::melt(df.scores[c(id.variable, 'train.performance', 'test.performance')], id.vars = id.variable, value.name = "performance")[c(1, 3)]

    # print(df.scores)
    # scale data, and store center and scale values of distance variable
    df.scores.scaled <- scale(df.scores)
    distance.scale <- attr(df.scores.scaled,"scaled:scale")[[id.variable]]
    distance.center <- attr(df.scores.scaled,"scaled:center")[[id.variable]]

    # apply clustering algorithm - dbscan with eps = 0.4 (based on experimentation)
    dbs <- fpc::dbscan(df.scores.scaled, eps = 0.4, MinPts = 10)

    # add cluster id to main dataframe
    df.scores.new <- df.scores
    df.scores.new$cluster <- dbs$cluster

    # construct new dataframe with cluster details
    cluster.means <- aggregate(df.scores.new[1], list(df.scores.new$cluster), mean)
    cluster.sizes <- aggregate(df.scores.new[1], list(df.scores.new$cluster), length)
    df.scores.cluster <- cbind(cluster.means, cluster.sizes)[, c(1, 2, 4)]
    colnames(df.scores.cluster) <- c('cluster', 'cluster.mean.distance', 'cluster.size')


    # remove outliers clusters
    df.scores.cluster <- subset(df.scores.cluster, df.scores.cluster$cluster != 0)

    # get the two farthest clusters along the distance dimension
    row.max <- df.scores.cluster[which.max(df.scores.cluster$cluster.mean.distance), ]
    row.min <- df.scores.cluster[which.min(df.scores.cluster$cluster.mean.distance), ]

    # calculate the weighted average of the distance between the two farthest clusters
    x = c(row.max$cluster.mean.distance, row.min$cluster.mean.distance)
    wt = c(row.max$cluster.size, row.min$cluster.size)
    distance.threshold <- weighted.mean(x, wt)

    # scaled threshold
    distance.threshold.scaled <- (distance.threshold - distance.center)/distance.scale

    return(list(dbs, df.scores, distance.threshold.scaled))

}



