source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/exploration/vizualizations.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/dataset/globals_dataset.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/dataset/processing.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/exploration/globals_exploration.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/machine_learning/globals_machine_learning.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/machine_learning/models.R")

library(ggplot2)
library(dplyr)
library(treemapify)
library(CHAID)
library(ISLR)
library(rpart)
library(rpart.plot)
library(fastDummies)


outcome="status"
mod_positive="Charged Off"
df=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/data/datasets/loan_data.csv")


outcome_model=df[,outcome]==mod_positive

df[outcome]=outcome_model
method="CART"
prop=.7
prune=FALSE
model_results=NULL
model=NULL
true_labels_test=NULL
pred_prob_test=NULL
threshold=.5
model_results=decision_tree_model(df[,c(outcome,"X")],outcome,c("qualititative nominale","quantitative discrète"),method=method,prop=prop,prune=prune)
#model_results=logistic_regression(df[,c(outcome,"payment_inc_ratio")],outcome,c("quantitative","quantitative"),prop=prop)


model=model_results[["model"]]
true_labels_test=as.logical(unlist(model_results[2]))
pred_prob_test=unlist(model_results[3])
metrics_confusion=calculate_metrics(true_labels_test,pred_prob_test>threshold)
predicted=pred_prob_test>threshold
actual=true_labels_test
tp = sum(predicted == TRUE & actual == TRUE)
fp = sum(predicted == TRUE & actual == FALSE)
fn = sum(predicted == FALSE & actual == TRUE)
tn = sum(predicted == FALSE & actual == FALSE)
aa=display_informations(model,"Arbre de décision CART")
print("jd")
