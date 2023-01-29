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

ui = dashboardPage(
  dashboardHeader(
    title = "Shiny App"
  ),
  dashboardSidebar(
    selectInput("input_typedata_choice",
                label = "Choix du type de données",
                choices = c("Fichier personnel","Fichier intégré")
    ),
    conditionalPanel(condition = "input.input_typedata_choice == 'Fichier personnel'",
      fileInput("input_file", label = "Fichier à importer", placeholder = "Choississez votre fichier"),
      selectInput("input_format",
                  label="Sélectionner le format de fichier",
                  choices=c("csv","json","Autre")
      ),
      checkboxInput("input_header_file",label = "Noms de variables présents", value = FALSE),
      sliderInput("input_fileSkip",label="Ignorer les premières lignes?",min=0,max=101,value=0,step=1),
      conditionalPanel(condition = "input.input_format == 'csv'",
        radioButtons("input_fileSep",
                     label = "Séparateur",
                     c("point-virgule" = ";", "virgule" = ",", "espace" = " ", "tabulation" = "\t", "autre" = "autre")
        ),
        conditionalPanel(condition = "input.input_fileSep == 'autre'",
          textInput("input_fileSepOther", label = "Séparateur")
        ),
        radioButtons("input_fileDec",
                     label = "Séparateur de décimales",
                     c("point" = ".", "virgule" = ",")
        )


      )
    ),
    conditionalPanel(condition="input.input_typedata_choice == 'Fichier intégré'",
                     selectInput("input_intdataset",
                                 label="Sélectionner le dataset intégré",
                                 choices=NULL
                     ),
                     checkboxInput("input_header_file",label = "Noms de variables présents", value = FALSE),
                     sliderInput("input_fileSkip",label="Ignorer les premières lignes?",min=0,max=101,value=0,step=1)
    ),

    actionButton("input_valid_button",label="Valider")




  ),
  dashboardBody(
    includeScript("/home/khaldi/Documents/shiny_project/www/script.js"),
    tabBox(
      title="Tableau de Bord",
      id="tabox_exp",
      width=12,
      tabPanel("Dataset",dataTableOutput("output_table"),
               fluidRow(
                 column(12,
                        uiOutput("input_varschoice")
                 )
               ),
               fluidRow(
                 column(3,
                        uiOutput("input_varchoice")
                 ),
                 column(3,
                        uiOutput("input_type_varchoice")
                 ),
                 column(3,
                        uiOutput("input_valid_type")
                 )

               )




      ),





      tabPanel("Exploration",
               fluidRow(
                 column(3,
                        uiOutput("input_exp_type")
                 )
               ),

               conditionalPanel(condition="input.input_exp_type == 'Univarié'",
                                fluidRow(
                                  column(3,uiOutput("input_uni_select_var")),
                                  column(3,uiOutput("input_uni_select_viz"))
                                ),
                                actionButton("input_valid_exp",label="Valider les options")

               ),
               conditionalPanel(condition="input.input_exp_type == 'Bivarié'",
                                fluidRow(
                                  column(3,uiOutput("input_bi_select_var")),
                                  column(3,uiOutput("input_bi_select_viz"))
                                ),
                                actionButton("input_valid_exp",label="Valider les options")

               ),
      ),




















      tabPanel("Apprentissage de Modèles",
               "Tab Apprentissage de modèles"
      )
    ),
  ),
  title = "Title",

)

server = function (input, output, session) {
  options(shiny.maxRequestSize=30*1024^2) #maximum size for uploading
  list_reavalues=reactiveValues(
    table=NULL, #initialization
    table_all=NULL,
    col_names=NULL #columns name

  )
  observeEvent(input$input_valid_button,{
    if(input$input_typedata_choice=="Fichier personnel"){
      if(input$input_format=="csv"){
        if (is.null(input$input_file)) personnalFile=NULL
        if (input$input_fileSep == 'autre') sep = input$input_fileSepOther
        else sep = input$input_fileSep
        try({
          personnalFile=read.table(
            input$input_file$datapath,
            header = input$input_header_file,
            sep = sep,
            dec = input$input_fileDec,
            skip = input$input_fileSkip,
            stringsAsFactors = FALSE)
          list_reavalues$col_names=names(personnalFile)
          list_reavalues$table_all=personnalFile
          list_reavalues$table=data.frame(list_reavalues$table_all)
          output$input_varschoice=renderUI({
            selectInput("input_varschoice",label="Choisissez les variables",choices=cbind("#Toutes",list_reavalues$col_names),selected = "#Toutes",multiple = TRUE)
          })
          output$input_varchoice=renderUI({
            selectInput("input_varchoice",label="Choisissez une variable",choices=list_reavalues$col_names,selected = list_reavalues$col_names[1])
          })
          output$input_type_varchoice=renderUI({
            selectInput("input_type_varchoice",label="Choisissez le type de variable",choices=c("quantitative discrète","quantitative continue","qualitative"))
          })
          output$input_valid_type=renderUI({
            actionButton("input_valid_type",label="Valider le type de la variable")
          })
          output$input_exp_type=renderUI({
            selectInput("input_exp_type",
                        label="Choisissez le type d'exploration",
                        choices=c("Univarié","Bivarié"),
                        selected = "Univarié"
            )
          })
          output$input_uni_select_viz=renderUI({
            selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=NULL)
          })
          output$input_bi_select_viz=renderUI({
            selectInput("input_bi_select_viz",label="Selectionner votre visualisation",choices=NULL)
          })
          output$input_uni_select_var=renderUI({
            selectInput("input_uni_select_var",label="Selectionner votre variable",choices=list_reavalues$col_names,selected =list_reavalues$col_names[1] )
          })
          output$input_bi_select_var=renderUI({
            selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=list_reavalues$col_names,multiple=TRUE,options = list(maxItems = 2),selected =list_reavalues$col_names[1])
          })













        }, silent = TRUE)
      }
    }




  })

  observeEvent(input$input_varschoice, {
      if(!"#Toutes" %in% input$input_varschoice){
        list_reavalues$table=as.data.frame(list_reavalues$table_all[input$input_varschoice])
        output$input_varchoice=renderUI({
          selectInput("input_varchoice",label="Choisissez une variable",choices=input$input_varschoice,selected = input$input_varschoice[1])
        })
        output$input_uni_select_var=renderUI({
          selectInput("input_uni_select_var",label="Selectionner votre variable",choices=input$input_varschoice)
        })
        output$input_bi_select_var=renderUI({
          selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=input$input_varschoice,multiple=TRUE,options = list(maxItems = 2),selected =list_reavalues$col_names[1])
        })
      }
      else{
        list_reavalues$table=data.frame(list_reavalues$table_all)
      }
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  observeEvent(input$input_valid_type, {
    col_select=input$input_varchoice
    col_type=input$input_type_varchoice
    list_reavalues$table=as.data.frame(list_reavalues$table)
    if(col_type=="quantitative discrète"){
      list_reavalues$table[,col_select]=as.integer(list_reavalues$table[,col_select])
      mean_integer=as.integer(mean(list_reavalues$table[,col_select],na.rm=TRUE))
      list_reavalues$table[is.na(list_reavalues$table[,col_select]),col_select]=mean_integer
    }
    if(col_type=="quantitative continue"){
      list_reavalues$table[,col_select]=as.numeric(list_reavalues$table[,col_select])
      mean_numeric=as.numeric(mean(list_reavalues$table[,col_select],na.rm=TRUE))
      list_reavalues$table[is.na(list_reavalues$table[,col_select]),col_select]=mean_numeric
    }
    if(col_type=="qualitative"){
      list_reavalues$table[col_select]=sapply(list_reavalues$table[col_select],as.factor)
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)
  observeEvent(input$input_valid_exp,{
    print("Hello")
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  output$output_table = renderDataTable({
    if(!is.null(list_reavalues$table)) DT::datatable(list_reavalues$table,options = list(scrollX=TRUE,scrollY = "500px"))
  })











}

shinyApp(ui, server,options=c(port=5000))


