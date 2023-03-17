source("./app/back/src/exploration/vizualizations.R")
source("./app/back/src/dataset/globals_dataset.R")
source("./app/back/src/dataset/processing.R")
source("./app/back/src/exploration/globals_exploration.R")
source("./app/back/src/machine_learning/globals_machine_learning.R")
source("./app/back/src/machine_learning/models.R")

library(ROSE)

outcome="term"
mod_positive="Default"
df=read.csv("./app/data/datasets/loan_data.csv")
f=paste(outcome, "", sep="~")
col_categories=get_categories(df)
counts=table(df[[outcome]])
minority_values=counts[which.min(counts)]
df_cols=df[c("X","loan_amnt")]
multi_var_correlations(df,col_categories)
two_var_correlations(df_cols,c("quantitative continue","quantitative continue"))
t=do.call(ovun.sample, list(as.formula(paste(outcome,"~.")), data=df,method="under",seed=1,N=minority_values/0.05))$data
a=c("sk","ab")

length(unique(df[[outcome]]))
