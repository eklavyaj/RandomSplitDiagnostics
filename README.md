# Random Split Diagnostics

This package assesses the quality of a random split of a dataset. 

## About

The analysis is based on a modified version of the Mahalanobis distance, a multidimensional distance measuring technique. 

After the user inputs an initial split along with the model relation for regression (in R format), the `diagnose()` function will return our conclusion in addition to a plot displaying the foundation of our conclusions.

## Example
```
library(RandomSplitDiagnostics)

# data preparation
dataset_name <- "Abalone"
data(abalone)

# intial random split of data
s <- sample(x = 1:nrow(abalone), size = floor(nrow(abalone)*0.7), replace = F)
df_train <- abalone[s, ]
df_test <- abalone[-s, ]

# defining model relation based on variables of data
model.relation <- Rings ~ LongestShell + Diameter + Height

# function call
diagnose(dataset.name, df.train, df.test, model.relation = model.relation,
 metric.performance = "Normalized AIC", num.simulations = 200,
  alpha = 0.05, save.plots = TRUE, output.dir = "Output")
  
```

## Output Plot

[Abalone_1.pdf](https://github.com/eklavyaj/RandomSplitDiagnostics/files/8589188/Abalone_1.pdf)
![sample_table](https://user-images.githubusercontent.com/50804314/165907822-dfb0c6e9-d8d6-4f9a-a0f7-a13aace1d4ac.png)

