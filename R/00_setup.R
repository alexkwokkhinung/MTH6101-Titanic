library(pROC)  
library(ggplot2)

set.seed(6101)

titanic_raw <- read.csv("data/train.csv", stringsAsFactors = FALSE,
                        na.strings = c("", "NA"))

dim(titanic_raw)
