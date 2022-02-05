df = read.csv("Output/Abalone/WholeWeight ~ Height + LongestShell + Diameter.csv")
df_melt = reshape2::melt(df[c(1, 3, 4)], id.vars = "distance", value.name = "performance")[c(1, 3)]
df_scaled = scale(df_melt)
km <- kmeans(df_scaled, 3)
factoextra::fviz_cluster(km , df_scaled)


df = read.csv("Output/Diamonds/price ~ x + y + z + depth.csv")
df_melt = reshape2::melt(df[c(1, 3, 4)], id.vars = "distance", value.name = "performance")[c(1, 3)]
df_scaled = scale(df_melt)
km <- kmeans(df_scaled, 6)
km$centers
factoextra::fviz_cluster(km , df_scaled)
