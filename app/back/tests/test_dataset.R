source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/exploration/vizualizations.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/dataset/globals_dataset.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/dataset/processing.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/exploration/globals_exploration.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/machine_learning/globals_machine_learning.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/machine_learning/models.R")
source("/home/khaldi/Documents/EDA_ML_RShiny/app/back/src/machine_learning/processing_machine_learning.R")

library(ROSE)

outcome="term"
mod_positive="Default"
df=read.csv("/home/khaldi/Documents/EDA_ML_RShiny/app/data/datasets/loan_data.csv")
f=paste(outcome,".",sep="~")
counts=table(df[[outcome]])
minority_values=counts[which.min(counts)]

t=do.call(ovun.sample, list(as.formula(paste(outcome,"~.")), data=df,method="under",seed=1,N=minority_values/0.05))$data
a=c("sk","ab")

length(unique(df[[outcome]]))
