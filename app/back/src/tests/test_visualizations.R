source("./app/back/src/exploration/vizualizations.R")
source("./app/back/src/dataset/globals_dataset.R")
source("./app/back/src/dataset/processing.R")
source("./app/back/src/exploration/globals_exploration.R")
source("./app/back/src/machine_learning/globals_machine_learning.R")
source("./app/back/src/machine_learning/models.R")


library(ggplot2)
library(dplyr)
library(treemapify)

#dataframe1=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/app/data/datasets/click_rates.csv")

#diag_barres_qualitative(dataframe["Headline"])


dataframe1=read.csv("./app/data/datasets/loan_data.csv")
col_categories=get_categories(dataframe1)
dataframe1=process(dataframe1,col_categories = col_categories)
type_col1=col_categories[match("borrower_score", names(col_categories))]
type_col2=col_categories[match("loan_amnt", names(col_categories))]
type_cols=c(type_col1,type_col2)


test_boxplot=function(df_cols,type_cols){
  two_var_boxplot_qual_var_cond(df_cols,type_cols)
}

test_barplot=function(df_cols,type_cols){
  two_var_barplot_cond(df_cols,type_cols)
}

test_tree=function(df_cols,type_cols){
  two_var_treemap_2_var(df_cols,type_cols)
}


test_scatter_plot=function(df_cols,type_cols){
  two_var_scatterplot(df_cols,type_cols)
}

test_statistics=function(df_cols,type_cols){
  two_var_statistics(df_cols,type_cols)
}



two_var_statistics(dataframe1[,c("borrower_score","loan_amnt")],type_cols)




#df=data.frame(x=c(TRUE,FALSE),y=c("A","B"))
#prop.table(table(df[["x"]],df[["y"]]))
#a=c(table(df[["x"]],df[["y"]]))
#print(a)
#colnames(two_var_contingency_table(dataframe1[c("status","loan_amnt")],type_cols = c("quantitative ","quantitative continue")))
#x1=c(5.42,4,3.0)
#sum(x1-floor(x1)==0)/3
#as.integer("5.99")
#factor(c(TRUE,FALSE),ordered = TRUE)