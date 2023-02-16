source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/exploration/vizualizations.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/dataset/globals_dataset.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/dataset/processing.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/exploration/globals_exploration.R")
library(ggplot2)

#dataframe1=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/data/datasets/click_rates.csv")

#diag_barres_qualitative(dataframe["Headline"])


dataframe1=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/data/datasets/loan_data.csv")
col_categories=get_categories(dataframe1)
dataframe1=process(dataframe1,col_categories = col_categories)
df=data.frame(x=c(TRUE,FALSE),y=c("A","B"))
prop.table(table(df[["x"]],df[["y"]]))
#a=cbind(table(df[["x"]],df[["y"]]),)
colnames(two_var_contingency_table(dataframe1[c("status","loan_amnt")],type_cols = c("quantitative ","quantitative continue")))
#x1=c(5.42,4,3.0)
#sum(x1-floor(x1)==0)/3
#as.integer("5.99")
#factor(c(TRUE,FALSE),ordered = TRUE)