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
model_relation <- WholeWeight ~ Height + LongestShell + Diameter

# function call
diagnose(dataset_name, df_train, df_test, model_relation)
  
```

## Output Plot

![WholeWeight ~ Height + LongestShell + Diameter_diagnose](https://user-images.githubusercontent.com/50804314/157613690-e3baef1d-73d2-4963-a1e3-09f42290821b.png)
