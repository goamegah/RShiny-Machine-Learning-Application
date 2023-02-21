##### Interface Utilisateur - UI =================================

ui = dashboardPage(
  
  ##### Header  =================================
  dashboardHeader(
    title = "JaGo"
  ),
  
  ##### SideBar  =================================
  dashboardSidebar(
    selectInput(
      "input_typedata_choice",
      label = "Choix du type de données",
      choices = c("Fichier personnel","Fichier intégré")
    ),
    
    # Condition:
    conditionalPanel(
      condition = "input.input_typedata_choice == 'Fichier personnel'",
      fileInput(
        "input_file", 
        label = "Fichier à importer", 
        placeholder = "Choississez votre fichier"
        ),
        selectInput(
          "input_format",
          label = "Sélectionner le format de fichier",
          choices = c("csv","json","Autre")
          ),
      checkboxInput(
        "input_header_file",
        label = "Noms de variables présents",
        value = FALSE
        ),
      #
      sliderInput(
        "input_fileSkip",
        label = "Ignorer les premières lignes?",
        min=0, max=101, value=0, step=1
        ),
      
      # Condition:
      conditionalPanel(
        condition = "input.input_format == 'csv'",
        radioButtons(
          "input_fileSep",
          label = "Séparateur",
          choices = 
            c("point-virgule" = ";",
              "virgule" = ",",
              "espace" = " ", 
              "tabulation" = "\t",
              "autre" = "autre"),
          selected = ","
          ),
        
        # Condition:  
        conditionalPanel(
          condition = "input.input_fileSep == 'autre'",
          textInput(
            "input_fileSepOther", 
            label = "Séparateur"
            )
          ),
        
        radioButtons(
          "input_fileDec",
          label = "Séparateur de décimales",
          c("point" = ".", "virgule" = ",")
          )
        )
    ),
    
    # Condition:
    conditionalPanel(
      condition ="input.input_typedata_choice == 'Fichier intégré'",
      selectInput(
        "input_intdataset",
        label = "Sélectionner le dataset intégré",
        choices = NULL
        ),
      checkboxInput(
        "input_header_file",
        label = "Noms de variables présents", 
        value = FALSE
        ),
      sliderInput(
        "input_fileSkip",
        label = "Ignorer les premières lignes?",
        min = 0, max = 101, value = 0, step = 1
        )
      ),
    
    actionButton("input_valid_button",label="Valider")
  ),
  
  ##### Le centre la page  =================================
  dashboardBody(
    includeCSS("./www/style.css"),
    includeScript("./www/script.js"),
    tabBox(
      title = "Tableau de Bord",
      id = "tabox_exp",
      width=12,
      
      # ---------------------- Elements de la partie Dataset
      tabPanel(
        "Dataset",
        
        fluidRow(
          column(
            12, 
            div(
              class = "output_tables",
              dataTableOutput("output_table"))
            )
          ),
        
        fluidRow(
          column(
            12,
            div(
              class = "output_tables",
              dataTableOutput("output_type_table")
              )
            )
          ),
          
        fluidRow(
          column(
            12,
            uiOutput("input_varschoice")
            )
          ),
        
        fluidRow(
          column(
            3,
            uiOutput("input_varchoice")
            ),
          column(
            3,
            uiOutput("input_type_varchoice")
            ),
          column(
            3,
            uiOutput("input_valid_type")
            )
          ),
        
        fluidRow(
          column(
            12,
            uiOutput("input_process")
            )
          )
        ),
      
      # ---------------------- Elements de la partie Exploration
      tabPanel(
        "Exploration",
        fluidRow(
          column(
            3,
            uiOutput("input_exp_type")
            )
          ),
        
        # Condition:
        conditionalPanel(
          condition = "input.input_exp_type == 'Univariée'",
          fluidRow(
            column(
              3,
              uiOutput("input_uni_select_var")
              ),
            column(
              3,
              uiOutput("input_uni_select_viz")
              )
            ),
          ),
        
        # Condition:
        conditionalPanel(
          condition = "input.input_exp_type == 'Bivariée'",
          fluidRow(
            column(
              3,
              uiOutput("input_bi_select_var")
              ),
            column(
              3,
              uiOutput("input_bi_select_viz")
              ),
            column(
              1,
              uiOutput("bins_input")
              )
            ),
          ),
        
        fluidRow(
          column(
            12,
            uiOutput("input_valid_exp")
            )
          ),
        
        fluidRow(
          column(
            12,
            uiOutput("viz_input")
            )
          )
        ),
      
      # ---------------------- Elements de la partie Machine Learning
      tabPanel(
        "Apprentissage de Modèles",
        uiOutput("input_model_type"),
        conditionalPanel(
          condition ="input.input_model_type == 'Classification Binaire'",
          fluidRow(
            column(
              3,
              uiOutput("input_model_type_bin")
              ),
            column(
              3,
              uiOutput("input_proportion_bin")
              ),
            column(
              3,
              uiOutput("input_threshold_bin")
              )
            ),
          
          fluidRow(
            column(
              2,
              uiOutput("input_model_outcome_bin")
              ),
            column(
              2,
              uiOutput("input_model_poschoice_bin")
              ),
            column(
              2,
              uiOutput("input_model_features_bin")
              ),
            
            # Condition:
            conditionalPanel(
              condition ="input.input_model_type_bin == 'Arbre de décision CART'",
              column(
                2,
                checkboxInput(
                  "input_model_pruned_bin", "Arbre élagué ?", 
                  value = FALSE)
                )
              ),
            
            # Condition
            conditionalPanel(
              condition = "input.input_model_type_bin == 'Arbre de décision CHAID'",
              column(
                2,
                selectInput(
                  "input_model_bins_bin",
                  label = "Nombre de bacs",
                  choices = 1:15,
                  selected = 7
                  )
                )
              )
            ),
          
          fluidRow(
            column(
              12,
              uiOutput("input_valid_model_bin")
              )
            ),
          
          fluidRow(
            column(
              5,
              uiOutput("input_confusion_model_bin")
              ),
            column(
              4,
              uiOutput("input_metrics_model_bin")
              )
            ),
          
          # Condition:
          conditionalPanel(
            condition = 
              "input.input_model_type_bin == 'Arbre de décision CART' ||
            input.input_model_type_bin == 'Arbre de décision CHAID'",
            fluidRow(
              column(
                12,
                uiOutput("input_viz_tree_bin")
                )
              )
            )
          )
        )
      ),
    ),
  
  # Nom de l'application - JaGo
  title = "JaGo",
)


