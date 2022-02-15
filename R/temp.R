# df <- read.csv(file = "Output/Abalone/WholeWeight ~ Height + LongestShell + Diameter_optimal.csv")

# filter out all the entries with greater than average train performance
# df1 <- subset(df, train.performance >= mean(train.performance))

# sort according to test performance
# df2 <- df1[order(df1$distance, -df1$test.performance),]
# df2[1,]


# sort according to least mahalanobis distance
# df3 <- df2[order(df2$distance),]
# df3[1,]
