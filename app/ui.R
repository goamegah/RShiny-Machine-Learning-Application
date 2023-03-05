##### Interface Utilisateur - UI =================================
#
# Dans cette partie nous définissons les visuels (boutons, champs, widgets)
# ainsi la logique d'apparition de ces derniers en fonction du choix de l'
# utilisateur
#
# Notes:  Dans l'optique d'améliorer la lisibilité du code, nous avions
#         introduit quelques notations de commentaire.
# e.g:
# - Les suggestions de code seront reconnu grâce au symbole (%%%)
# - Les commentaires relative au section de code grâce au symbole (===)
# - Les commentaires des sous fonctionnalités via le symbole (***)
#
##### ========================= *** =================================




ui = dashboardPage(
  dashboardHeader(
    title = ""
  ),
  dashboardSidebar(
    # *** Choix du type de données
    selectInput("input_typedata_choice",
                label = "Choix du type de données",
                choices = c("Fichier personnel","Fichier intégré")
    ),
    # Condition:
    # *** Cas des fichiers personnels
    # *******************************
    conditionalPanel(condition = "input.input_typedata_choice == 'Fichier personnel'",
                     fileInput("input_file", label = "Fichier à importer", placeholder = "Choississez votre fichier"),
                     selectInput("input_format",
                                 label="Sélectionner le format de fichier",
                                 choices=c("csv")
                     ),

                     # Sous Condition:
                     # *** Cas des fichiers CSV
                     # ************************
                     conditionalPanel(condition = "input.input_format == 'csv'",
                                      checkboxInput("input_header_file",label = "Noms de variables présents", value = FALSE),
                                      sliderInput("input_fileSkip_csv",label="Ignorer les premières lignes?",min=0,max=101,value=0,step=1),
                                      radioButtons("input_fileSep",
                                                   label = "Séparateur",
                                                   choices=c("point-virgule" = ";", "virgule" = ",", "espace" = " ", "tabulation" = "\t", "autre" = "autre"),
                                                   selected = ","
                                      ),
                                      # Sous sous Condition:
                                      # *** Cas des autres formats de fichiers
                                      # **************************************
                                      conditionalPanel(condition = "input.input_fileSep == 'autre'",
                                                       textInput("input_fileSepOther", label = "Séparateur")
                                      ),
                                      radioButtons("input_fileDec",
                                                   label = "Séparateur de décimales",
                                                   c("point" = ".", "virgule" = ",")
                                      )
                                      
                                      
                     )
    ),
    # Condition:
    # *** Cas des fichiers intégrés
    # *****************************
    conditionalPanel(condition="input.input_typedata_choice == 'Fichier intégré'",
                     selectInput("input_intdataset",
                                 label="Sélectionner le dataset intégré",
                                 choices=c("click_rates.csv","four_sessions.csv","loan_data.csv","web_page_data.csv")
                     ),
                     sliderInput("input_fileSkip_integrate",label="Ignorer les premières lignes?",min=0,max=101,value=0,step=1)
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
               # Visuel:
               # *** Espace réservé au tableau pour le dataset
               # ********************************************************
               fluidRow(column(12,div(class="output_tables",dataTableOutput("output_table")))),
               fluidRow(column(12,div(class="output_tables",dataTableOutput("output_type_table")))),
               # Visuel:
               # *** Espace réservé au choix des var.
               # ***************************************************************
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
                        uiOutput("input_type_process")
                 ),
                 # Visuel:
                 # *** Espace réservé pour la déclaration de leur type/ outliers/normalisation/ dummification
                 # *******************************************************************************************
                 conditionalPanel(condition="input.input_type_process == 'Type de variable'",
                                  column(2,
                                         uiOutput("input_type_varchoice")
                                  ),
                                  column(2,
                                         uiOutput("input_valid_type")
                                  )
                 ),
                 conditionalPanel(condition="input.input_type_process == 'Outliers'",
                                  column(1,
                                         uiOutput("input_var_outliers")
                                  ),
                                  column(1,
                                         uiOutput("input_valid_outliers")
                                  )
                 ),
                 conditionalPanel(condition="input.input_type_process == 'Normalisation'",
                                  column(1,
                                         uiOutput("input_var_normalize")
                                  ),
                                  column(1,
                                         uiOutput("input_valid_normalize")
                                  )
                 ),
                 conditionalPanel(condition="input.input_type_process == 'Dummification'",
                                  column(1,
                                         uiOutput("input_var_dummy")
                                  ),
                                  column(1,
                                         uiOutput("input_valid_dummy")
                                  )
                 )
               ),
               fluidRow(
                 column(12,
                        uiOutput("input_process")
                 )
               )
               
      ),
      tabPanel("Exploration",
               # Visuel:
               # *** Espace réservé au choix du type de var., la/les var. et les vis.
               # ***************************************************************

               fluidRow(
                 column(3,
                        uiOutput("input_exp_type")
                 )
               ),

               # Condition:
               # *** Cas d'une exploration Univariée
               # ************************************
               conditionalPanel(condition="input.input_exp_type == 'Univariée'",
                                fluidRow(
                                  column(3,uiOutput("input_uni_select_var")),
                                  column(3,uiOutput("input_uni_select_viz")),
                                  column(1,uiOutput("bins_input_uni"))

                                ),

               ),
               # Condition:
               # *** Cas d'une exploration Bivariée
               # **********************************
               conditionalPanel(condition="input.input_exp_type == 'Bivariée'",
                                fluidRow(
                                  column(3,uiOutput("input_bi_select_var")),
                                  column(3,uiOutput("input_bi_select_viz")),
                                  column(1,uiOutput("bins_input_bi"))
                                ),

               ),
               fluidRow(
                 column(12,
                        uiOutput("input_valid_exp")
                 )
               ),

               # Visuel:
               # *** Zone réservée au plot de la viz
               # ************************************
               fluidRow(
                 column(12,
                        uiOutput("viz_input")

                 )
               )
      ),
      tabPanel("Apprentissage de Modèles",
               # ---------------------- Eléments de la partie Machine E-learning
               uiOutput("input_model_type"),
               # Condition:
               # *** Cas d'une Classification Binaire
               # ************************************
               conditionalPanel(condition="input.input_model_type == 'Classification Binaire'",
                                  fluidRow(
                   column(3,
                          uiOutput("input_model_type_bin")
                   ),
                   column(2,
                          uiOutput("input_proportion_bin")
                   ),
                   column(2,
                          uiOutput("input_threshold_bin")
                   ),
                   column(2,
                          uiOutput("input_type_unbalanced")
                   ),
                   column(1,
                          uiOutput("input_prop_minclass")

                   ),
                   column(2,
                          uiOutput("input_valid_unbalanced")
                   )
                 ),
                 fluidRow(
                   column(2,
                          uiOutput("input_model_outcome_bin")
                   ),
                   column(2,
                          uiOutput("input_model_poschoice_bin")
                   ),
                   column(2,
                          uiOutput("input_model_features_bin")
                   ),
                   # Sous Condition:
                   # *** Cas d'une Classification Binaire - CHAID ou CART (arbres de décision)
                   # ***************************************************************************
                   conditionalPanel(condition="input.input_model_type_bin == 'Arbre de décision CART'",
                                     column(2,checkboxInput("input_model_pruned_bin", "Arbre élagué ?", value = FALSE))
                   ),
                   conditionalPanel(condition="input.input_model_type_bin == 'Arbre de décision CHAID'",
                                    column(2,selectInput("input_model_bins_bin",label="Nombre de bacs (var. quant.)",choices=1:15,selected=7))
                   )


                 ),
                 fluidRow(
                   column(12,
                          uiOutput("input_valid_model_bin")
                   )
                 ),# Zone pour afficher quelques infos (table de confusion, métriques..)

                 fluidRow(
                   column(5,
                          uiOutput("input_confusion_model_bin")
                   ),
                   column(4,
                          uiOutput("input_metrics_model_bin")
                   )
                 ),
                                fluidRow(
                                  column(12,uiOutput("input_viz_auc_bin"))
                                ),
                                # Zone pour afficher quelques infos des modèles ci-dessous
                                # ***************************************************************************
                                conditionalPanel(condition="input.input_model_type_bin == 'Arbre de décision CART' || input.input_model_type_bin == 'Régression Logistique'",
                                                 fluidRow(column(12,uiOutput("input_viz_informations_bin")))
                                ),
                                # Sous Condition:
                                # *** Zone pour affichage de l'arbre de décision
                                # **********************************************
                                conditionalPanel(condition="input.input_model_type_bin == 'Arbre de décision CART' || input.input_model_type_bin == 'Arbre de décision CHAID'",
                                                 fluidRow(
                                                   column(12,uiOutput("input_viz_model_bin"))
                                                 )
                                )


               )
      )
    ),
  ),
  # Nom de l'application - app
  title = "JaGo",
  
)


