##### Interface Utilisateur - UI =================================
# 
# Dans cette partie nous définissons les visuels (boutons, champs, widgets) 
# ainsi la logique d'apparition de ces derniers en fonction du choix de l'
# utilisateur
#
# Notes:  Dans l'optique d'améliorer la lisibilité du code, nous avions
#         introduit quelques notation de commentaire.
# e.g:
# - Les suggestion de code seront reconnu grâce au symbole (%%%)
# - Les commentaire relative au section de code grâce au symbole (===) 
# - Les commentaire des sous fonctionnalités via le symbole (***)
#
##### ========================= *** =================================

ui = dashboardPage(
  
  # === Header  =================================
  dashboardHeader(
    title = "JaGo"
  ),
  
  # === SideBar  =================================
  dashboardSidebar(
    
    # *** Choix du type de données
    selectInput(
      "input_typedata_choice",
      label = "Choix du type de données",
      choices = c("Fichier personnel","Fichier intégré")
    ),
    
    # Condition:
    # *** Cas des fichiers personnels
    # *******************************
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
        choices = c("csv", "json", "Autre")
        ),
      checkboxInput(
        "input_header_file",
        label = "Noms de variables présents",
        value = FALSE
        ),
      
      sliderInput(
        "input_fileSkip",
        label = "Ignorer les premières lignes?",
        min = 0, 
        max = 101, # %% max peut être dynamique (dépendant du nombre de données)
        value = 0,
        step = 1 
        ),
      
      # Sous Condition:
      # *** Cas des fichiers CSV
      # ************************
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
        
        # Sous sous Condition:  
        # *** Cas des autres formats de fichiers 
        # **************************************
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
    # *** Cas des fichiers intégrés
    # *****************************
    conditionalPanel(
      condition ="input.input_typedata_choice == 'Fichier intégré'",
      selectInput(
        "input_intdataset",
        label = "Sélectionner le dataset intégré",
        choices = NULL # %% On listera les data sets plûtard. %%
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
    
    actionButton("input_valid_button", label = "Valider")
    
    # %% Le meilleur reste à venir ;) . %%
  ),
  
  ##### Le centre la page  =================================
  dashboardBody(
    includeCSS("./www/style.css"),
    includeScript("./www/script.js"),
    tabBox(
      title = "Tableau de Bord",
      id = "tabox_exp",
      width=12,
      
      # ---------------------- Eléments de la partie Data set
      tabPanel(
        "Dataset",
        
        # Visuel:
        # *** Espace réservé au tableau pour l'aperçue du data set
        # ********************************************************
        fluidRow(
          column(
            12, 
            div(
              class = "output_tables",
              dataTableOutput("output_table"))
            )
          ),
        
        # Visuel:
        # *** Espace réservé au tableau pour l'aperçue du type des variables
        # ******************************************************************
        fluidRow(
          column(
            12,
            div(
              class = "output_tables",
              dataTableOutput("output_type_table")
              )
            )
          ),
        
        # Visuel:
        # *** Espace réservé au choix des var. et de la def. de leur type
        # ***************************************************************
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
      
      # ---------------------- Eléments de la partie Exploration
      tabPanel(
        "Exploration",
        
        # Visuel:
        # *** Espace réservé au choix du type var., la.es var. et les vis.
        # ***************************************************************
        fluidRow(
          column(
            3,
            uiOutput("input_exp_type")
            )
          ),
        
        # Condition:
        # *** Cas d'une exploration Uni variée
        # ************************************
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
        # *** Cas d'une exploration Bivariée
        # **********************************
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
        
        # Visuel:
        # *** Zone réservée aux visualisations
        # ************************************
        fluidRow(
          column(
            12,
            uiOutput("viz_input")
            )
          )
        ),
      
      # ---------------------- Eléments de la partie Machine E-learning
      tabPanel(
        "Apprentissage de Modèles",
        uiOutput("input_model_type"),
        
        # Condition:
        # *** Cas d'une Classification Binaire
        # ************************************
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
            
            # Sous Condition:
            # *** Cas d'une Classification Binaire - CART (arbre à 2 branches)
            # ****************************************************************
            conditionalPanel(
              condition =
                "input.input_model_type_bin == 'Arbre de décision CART'",
              column(
                2,
                checkboxInput(
                  "input_model_pruned_bin", "Arbre élagué ?", 
                  value = FALSE)
                )
              ),
            
            # Sous Condition:
            # *** Cas d'une Classification Binaire - CHAID (arbre multibranches)
            # ******************************************************************
            conditionalPanel(
              condition = 
                "input.input_model_type_bin == 'Arbre de décision CHAID'",
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
          
          # Zone pour la afficher quelques info (table de conf., tab Métriques)
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
          
          # Sous Condition:
          # *** Zone pour affichage de l'arbre de décision 
          # **********************************************
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


