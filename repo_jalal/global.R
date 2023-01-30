if(!require("shinyjs")) install.packages("shinyjs")
if(!require("shiny")) install.packages("shiny")
if(!require("shinydashboard")) install.packages("shinydashboard")
if(!require("DT")) install.packages("DT")

library(shiny)
library(shinydashboard)
library(shinyjs)
library(DT)

data(mtcars)
useShinyjs()