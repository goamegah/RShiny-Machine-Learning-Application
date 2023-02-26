source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/exploration/vizualizations.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/dataset/globals_dataset.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/dataset/processing.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/exploration/globals_exploration.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/machine_learning/globals_machine_learning.R")
source("/home/khaldi/Documents/repo_r_shiny/JaGo/back/src/machine_learning/models.R")

dataframe1=read.csv("/home/khaldi/Downloads/archive/weatherAUS.csv")
get_categories(dataframe1)