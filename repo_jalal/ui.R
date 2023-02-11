ui = dashboardPage(
  dashboardHeader(
    title = "JaGo"
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
    includeCSS("./www/style.css"),
    includeScript("./www/script.js"),
    tabBox(
      title="Tableau de Bord",
      id="tabox_exp",
      width=12,
      tabPanel("Dataset",
               fluidRow(column(12,div(class="output_tables",dataTableOutput("output_table")))),
               fluidRow(column(12,div(class="output_tables",dataTableOutput("output_type_table")))),
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


