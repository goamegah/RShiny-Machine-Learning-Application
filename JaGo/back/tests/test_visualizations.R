source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/exploration/vizualizations.R")
library(ggplot2)

dataframe1=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/data/datasets/click_rates.csv")

#diag_barres_qualitative(dataframe["Headline"])


#dataframe1=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/data/datasets/loan_data.csv")

as.data.frame(table(dataframe1["Headline"]))[,1]

as.data.frame(summary(dataframe1))

cbind(c(12,5),c(1))