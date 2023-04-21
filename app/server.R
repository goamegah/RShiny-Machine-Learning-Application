##### =============== Serveur - Partie Serveur =================================
#
# Dans cette partie nous définissons la logique dynamique du coté Serveur.
# Les visuels (boutons, champs, widgets) s'adaptent en fonction du choix de l'utilisateur.
# Par conséquent ces derniers réagissent à des évènements déclenchés par l'utilisateur
# Ces événement sont gérés coté serveur afin de fournir les modifications necésaaires pour
# que l'application fonctionne correctement.
#
# Notes: Dans l'optique d'améliorer la lisibilité du code, les éléments présents
#       contiennent des appels à des fonctions définies dans des fichiers
#       séparés dans des dossiers spécifiant les différents rôle des fonctions
#       utilisées.

#       Ces fichiers sont importés via la commande <source>
#
##### ==========================================================================

server = function (input, output, session) {
  # Importation des codes nécessaires aux traitements ***********************
  source("./back/src/dataset/globals_dataset.R")
  source("./back/src/dataset/processing.R")
  source("./back/src/exploration/globals_exploration.R")
  source("./back/src/exploration/vizualizations.R")
  source("./back/src/machine_learning/globals_machine_learning.R")
  source("./back/src/machine_learning/chaid.R")
  source("./back/src/machine_learning/models.R")


  options(shiny.maxRequestSize=25*1024^2) #maximum size for uploading
  # Déclaration de quelques variables réactives pour la synchronisation ***************
  list_reavalues=reactiveValues(
    table=NULL, #initialization
    table_all=NULL,
    type_table=NULL,
    col_categories=NULL,
    col_categories_all=NULL,
    modify_type=c(0,0), #first:exploration and second:machine learning panel
    modify_features=c(0,0),#first:exploration and second:machine learning panel
    modify_rows=c(0,0), #first: dataset panel and second:machine learning panel
    modify_outcome_unbalanced=NULL,
    error=FALSE,
    error_format=FALSE,
    error_csv=FALSE
  )

  # Event:
  # *** Gestion des évènements de l'UI
  # - sep : variable séparateur
  # - personnalFile : variable qui contient le dataframe
  # ************************************

  
  # Description des action lors d'un chargement d'un fichier ou dataset
  observeEvent(input$input_valid_button,{
    if (identical(input$input_file$datapath,NULL) &&
        input$input_typedata_choice != "Fichier intégré"){
      showModal(modalDialog(
        title = "Erreur",
        "Aucun dataset n'a été chargé",
        easyClose = TRUE
      ))

    }else if (input$input_format=="xlsx (Excel)" && 
              list_reavalues$error_format == TRUE &&
              input$input_typedata_choice != "Fichier intégré"){
      showModal(modalDialog(
        title = "Erreur",
        "Le dataset n'a pas été chargé correctement",
        easyClose = TRUE
      ))

    }
    else{
      list_reavalues$error=FALSE
      list_reavalues$error_csv=FALSE
      if(input$input_typedata_choice=="Fichier personnel"){
        # Event:
        # *** Traitement des fichier CSV
        # *********************************
        if(input$input_format=="csv"){
          if (is.null(input$input_file)) personnalFile=NULL
          if (input$input_fileSep == 'autre') sep = input$input_fileSepOther
          else sep = input$input_fileSep
          file_path=input$input_file$datapath
          header=input$input_header_file
          dec=input$input_fileDec
          skip=input$input_fileSkip_csv
        }
        else if(input$input_format=="xlsx (Excel)"){
          file_path=input$input_file$datapath
          header=input$input_header_file_xlsx
          if(! identical(input$input_srow_xlsx,NULL)){
            if(is.na(as.numeric(input$input_srow_xlsx)))
            {
              showModal(modalDialog(
                title = "Erreur",
                "Numéro de la ligne de départ doit être un nombre entier",
                easyClose = TRUE
              ))
              list_reavalues$error=TRUE
            }
          }else{
            showModal(modalDialog(
              title = "Erreur",
              "Mauvaise saisie du numéro de la ligne de départ",
              easyClose = TRUE
            ))
            list_reavalues$error=TRUE
          }
          if(list_reavalues$error==FALSE){

            if(! identical(input$input_erow_xlsx,NULL)){
              if(input$input_erow_xlsx == ""){
              }
              else if(is.na(as.numeric(input$input_erow_xlsx))){
                showModal(modalDialog(
                  title = "Erreur",
                  "Numéro de la ligne de fin doit être un nombre entier",
                  easyClose = TRUE
                ))
                list_reavalues$error=TRUE
              }else if (as.integer(
                input$input_erow_xlsx) < as.integer(input$input_srow_xlsx)){
                showModal(modalDialog(
                  title = "Erreur",
                  "Numéro de la ligne de fin doit être >= Numéro de la ligne de départ",
                  easyClose = TRUE
                ))
                list_reavalues$error=TRUE
              }

            }else{
              showModal(modalDialog(
                title = "Erreur",
                "Mauvaise saisie du numéro de la ligne de fin",
                easyClose = TRUE
              ))
              list_reavalues$error=TRUE
            }
          }

        }

      }

      if (list_reavalues$error ==FALSE){
        if(input$input_typedata_choice=="Fichier intégré"){
          file_name=input$input_intdataset
          file_path=paste("./back/src/data/datasets",file_name,sep="/")
          header=TRUE
          dec="."
          skip=input$input_fileSkip_integrate
          sep=","
        }
        try({
          if(input$input_format != "xlsx (Excel)"){
            personnalFile = tryCatch({
              read.table(
                file_path,
                header = header,
                sep = sep,
                dec = dec ,
                skip = skip,
                stringsAsFactors = FALSE)
            }, warning = function(w) {
              read.table(
                file_path,
                header = header,
                sep = sep,
                dec = dec ,
                skip = skip,
                stringsAsFactors = FALSE)
            }, error = function(e) {
              showModal(modalDialog(
                title = "Erreur lors du chargement du fichier csv",
                paste("Le fichier n'a pas été chargé correctement: ", e),
                easyClose = TRUE
              ))
              list_reavalues$error_csv=TRUE

            }, finally = {

            })

          }else{

            if (input$input_erow_xlsx==""){
              end.row=NULL
            }else{
              end.row=as.integer(input$input_erow_xlsx)
            }
            
            if(input$input_sheet_xlsx =="name"){
              personnalFile=
                as.data.frame(
                  read.xlsx(
                    file_path,
                    sheetName=as.character(input$input_sheet_name_xlsx),
                    header=as.logical(input$input_header_file_xlsx),
                    startRow=as.integer(input$input_srow_xlsx),
                    endRow=end.row))
            }else if(input$input_sheet_xlsx =="number"){
              personnalFile=
                as.data.frame(read.xlsx(
                  file_path,
                  sheetIndex=as.integer(input$input_sheet_value_xlsx),
                  header=as.logical(input$input_header_file_xlsx),
                  startRow=as.integer(input$input_srow_xlsx),
                  endRow=end.row))
            }
            personnalFile=process_columns_xlsx(personnalFile) #remove columns which contains only NA values
          }
          if(nrow(personnalFile) == 0){
            showModal(modalDialog(
              title = "Erreur",
              "Le dataset chargé ne contient pas de lignes !",
              easyClose = TRUE
            ))
          }
          if(ncol(personnalFile) == 0){
            showModal(modalDialog(
              title = "Erreur",
              "Le dataset chargé ne contient pas de colonnes !",
              easyClose = TRUE
            ))
          }


          if (list_reavalues$error_csv == FALSE &&
            (nrow(personnalFile) > 0) && (ncol(personnalFile) > 0)){

            list_reavalues$table_all=personnalFile
            list_reavalues$table=data.frame(list_reavalues$table_all)
            list_reavalues$col_categories_all=
              get_categories(list_reavalues$table_all)
            list_reavalues$col_categories=get_categories(list_reavalues$table)

            list_reavalues$type_table=
              data.frame(
                variable = colnames(list_reavalues$table), 
                type = sapply(list_reavalues$table, 
                              function(x){get_type_columns(x)}),
                category=list_reavalues$col_categories,
                modality=sapply(list_reavalues$table,
                                function(x){length(unique(x))}),
                missing_values=sapply(list_reavalues$table,
                                      function(x){sum(is.na(x))}))
            colnames(list_reavalues$type_table)[5]="Number of Missing Values"
            colnames(list_reavalues$type_table)[4]="Number of Modalities"

            rownames(list_reavalues$type_table)=NULL


            col_names=names(list_reavalues$table)
            names(list_reavalues$modify_type)=c("Exploration",
                                                "Apprentissage de Modèles")
            names(list_reavalues$modify_features)=c("Exploration",
                                                    "Apprentissage de Modèles")
            names(list_reavalues$modify_rows)=c("Dataset",
                                                "Apprentissage de Modèles")
            # Ajout dans la zone prévu le nom des vars. comme option
            output$input_varschoice=renderUI({
              selectInput(
                "input_varschoice",
                label="Choisissez les variables à étudier",
                choices=cbind("#Toutes",col_names),
                selected = "#Toutes",
                multiple = TRUE)
            })
            output$input_varchoice=renderUI({
              selectInput("input_varchoice",
                          label="Choisissez une variable",
                          choices=col_names,
                          selected = col_names[1])
            })
            output$input_type_process=renderUI({
              selectInput("input_type_process",
                          label="Paramètres Dataset",
                          choices=PARAMS,
                          selected=PARAMS[1])
            })

            output$input_type_varchoice=renderUI({
              selectInput(
                "input_type_varchoice",
                label="Choisissez la catégorie de la variable",
                choices=c("quantitative discrète",
                          "quantitative continue",
                          "qualitative ordinale",
                          "qualitative nominale"),
                selected="quantitative discrète")
            })
            output$input_valid_type=renderUI({
              actionButton("input_valid_type",
                           label="Valider la catégorie de la variable")
            })

            output$input_var_outliers=renderUI({
              checkboxInput("input_var_outliers",
                            "Supprimer les outliers",
                            value = FALSE)
            })
            output$input_valid_outliers=renderUI({
              actionButton("input_valid_outliers",
                           label="Valider la Saisie")
            })

            output$input_var_normalize=renderUI({
              checkboxInput("input_var_normalize",
                            "Normaliser la Variable",
                            value = FALSE)
            })
            output$input_valid_normalize=renderUI({
              actionButton("input_valid_normalize",
                           label="Valider la Saisie")
            })


            output$input_var_dummy=renderUI({
              checkboxInput("input_var_dummy",
                            "Dumifier la Variable",
                            value = FALSE)
            })
            output$input_valid_dummy=renderUI({
              actionButton("input_valid_dummy",
                           label="Valider la Saisie")
            })


            output$input_process=renderUI({
              actionButton("input_process",
                           label="Faire Preprocessing")
            })

            #-------------------------------------------------------------------------------------------#
            output$input_valid_exp=renderUI({
              actionButton("input_valid_exp",
                           label="Valider les options")

            })
            output$input_exp_type=renderUI({
              selectInput("input_exp_type",
                          label="Choisissez le type d'exploration",
                          choices=c("Univariée","Bivariée","Multivariée"),
                          selected = "Univariée"
              )
            })
            output$input_multi_select_viz=renderUI({
              selectInput("input_multi_select_viz",
                             label="Selectionner votre visualisation",
                             choices=c("Matrice de corrélations"),
                             multiple=TRUE,
                             selected ="Matrice de corrélations")
            })
            output$input_uni_select_var=renderUI({
              selectInput("input_uni_select_var",
                          label="Selectionner votre variable",
                          choices=col_names,
                          selected =input$input_varchoice )
            })
            output$input_bi_select_var=renderUI({
              selectizeInput("input_bi_select_var",
                             label="Selectionner vos deux variables (x resp y)",
                             choices=col_names,
                             multiple=TRUE,
                             options = list(maxItems = 2),
                             selected =list_reavalues$col_names[1])
            })


            #-------------------------------------------------------------------------------------------#
            output$input_model_type=renderUI({
              selectInput("input_model_type",
                          label="Choix du type de modèle",
                          choices=c("Classification Binaire"),
                          selected ="Classification Binaire")
            })
            output$input_proportion_bin=renderUI({
              sliderInput("input_proportion_bin",
                          label="Choisir la proportion pour le train Dataset",
                          min=0.01,
                          max=1,
                          value=0.7,
                          step=0.01)
            })
            output$input_threshold_bin=renderUI({
              sliderInput("input_threshold_bin",
                          label="Choisir le seuil d'acceptation (probabilités)",
                          min=0,
                          max=1,
                          value=0.5,
                          step=0.01)
            })

            qualitative_vars=list_reavalues$type_table %>%
              filter(category == "qualitative ordinale" | 
                       category == "qualitative nominale" ) %>%
              select(variable)
            qualitative_vars=qualitative_vars[["variable"]]
            features=setdiff(list_reavalues$type_table[["variable"]],
                             qualitative_vars[1])
            col_categoris_who_outco=
              list_reavalues$col_categories[!names(list_reavalues$col_categories)
                                            %in% c(qualitative_vars[1])]
            features_quant=
              features[col_categoris_who_outco %in% 
                         c("quantitative continue","quantitative discrète")]
            if (length(features) != length(features_quant)){
              modeles_type=MODELES_BIN[!MODELES_BIN %in% 
                                         c("Régression Logistique")]
              output$input_model_type_bin=renderUI({
                selectInput("input_model_type_bin",
                            label="Selectionner votre modèle",
                            choices=modeles_type,
                            selected =modeles_type[1])
              })
            }else{
              output$input_model_type_bin=renderUI({
                selectInput("input_model_type_bin",
                            label="Selectionner votre modèle",
                            choices=MODELES_BIN,
                            selected =MODELES_BIN[1])
              })
            }

            if (length(features) && length(qualitative_vars)){
              modality_outcome=unique(list_reavalues$table[[qualitative_vars[1]]])
              output$input_model_outcome_bin=renderUI({
                selectInput("input_model_outcome_bin",
                            label="Variable à prédire",
                            choices=qualitative_vars,
                            selected =qualitative_vars[1])
              })
              output$input_model_poschoice_bin=renderUI({
                selectInput("input_model_poschoice_bin",
                            label="Choix de la modalité positive",
                            choices=modality_outcome,
                            selected =modality_outcome[1])
              })
              output$input_model_features_bin=renderUI({
                selectizeInput("input_model_features_bin",
                               label="Selectionner vos features",
                               choices=features,
                               multiple=TRUE,
                               options = list(maxItems = length(features)),
                               selected =features[1])
              })
              output$input_valid_model_bin=renderUI({
                actionButton("input_valid_model_bin",
                             label="Valider les options")
              })

            }else{
              output$input_model_poschoice_bin=renderUI({
              })
              output$input_model_features_bin=renderUI({
              })
              output$input_valid_model_bin=renderUI({
              })
              output$input_model_outcome_bin=renderUI({
              })
            }

          }
        }, silent = TRUE)
      }
    }

  })



  observeEvent(input$input_file$datapath, {
        result = tryCatch({
          file_path=input$input_file$datapath
          sheets_name=excel_sheets(file_path)
          sheets_number=1:length(sheets_name)
          output$input_sheet_value_xlsx=renderUI({
            selectInput("input_sheet_value_xlsx",
                        label="Choisir le numéro de la feuille",
                        choices=sheets_number,
                        selected=sheets_number[1]
            )
          })
          output$input_sheet_name_xlsx=renderUI({
            selectInput("input_sheet_name_xlsx",
                        label="Saisir le nom de la feuille",
                        choices=sheets_name,
                        selected = sheets_name[1]
            )

          })
          output$input_sheet_xlsx=renderUI({
            radioButtons("input_sheet_xlsx",
                         label = "Identification de la feuille",
                         c("Par Nom" = "name", "Par Numéro" = "number")
            )
          })
          output$input_erow_xlsx=renderUI({
            textInput(
              "input_erow_xlsx",
              label="Numéro de la ligne de fin (laissez la case vide pour avoir toutes les lignes)",
              value=""
            )
          })
          output$input_srow_xlsx=renderUI({
            textInput("input_srow_xlsx",
                      label="Numéro de la ligne de départ",
                      value=1
            )
          })

          output$input_header_file_xlsx=renderUI({
            checkboxInput("input_header_file_xlsx",
                          label = "Noms de variables présents", 
                          value = FALSE)

          })
          list_reavalues$error_format=FALSE
        }, warning = function(w) {

        }, error = function(e) {
          if (input$input_format == "xlsx (Excel)"){
            showModal(modalDialog(
              title = "Erreur lors du chargement du fichier",
              paste("Une erreur lors du chargement du fichier,le fichier semble ne pas être au bon format: ",e),
              easyClose = TRUE
            ))

          }
          list_reavalues$error_format=TRUE
          output$input_sheet_value_xlsx=renderUI({
          })
          output$input_sheet_name_xlsx=renderUI({
          })
          output$input_sheet_xlsx=renderUI({
          })
          output$input_erow_xlsx=renderUI({
          })
          output$input_srow_xlsx=renderUI({
          })
          output$input_header_file_xlsx=renderUI({
          })

        }, finally = {

        })

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  # Event:
  # *** évenement ou l'utilisateur choisit des features
  # **********************************************************************
  observeEvent(input$input_varschoice, {
    if(!"#Toutes" %in% input$input_varschoice){
      list_reavalues$table=as.data.frame(
        list_reavalues$table_all[input$input_varschoice])
      indices_col = match(input$input_varschoice, 
                          names(list_reavalues$col_categories_all))
      
      list_reavalues$col_categories=
        list_reavalues$col_categories_all[indices_col]
      list_reavalues$type_table=data.frame(
        variable = colnames(list_reavalues$table), 
        type = sapply(list_reavalues$table, 
                      function(x){get_type_columns(x)}),
        category=list_reavalues$col_categories,
        modality=sapply(list_reavalues$table,
                        function(x){length(unique(x))}),
        missing_values=sapply(list_reavalues$table,function(x){sum(is.na(x))}))
      
      colnames(list_reavalues$type_table)[5]="Number of Missing Values"
      colnames(list_reavalues$type_table)[4]="Number of Modalities"
      rownames(list_reavalues$type_table) = NULL

      output$input_varchoice=renderUI({
        selectInput("input_varchoice",
                    label="Choisissez une variable",
                    choices=input$input_varschoice,
                    selected = input$input_varschoice[1])
      })
    }
    else{
      list_reavalues$table=data.frame(list_reavalues$table_all)
      list_reavalues$col_categories=list_reavalues$col_categories_all
      list_reavalues$type_table=data.frame(
        variable = colnames(list_reavalues$table), 
        type = sapply(list_reavalues$table, 
                      function(x){get_type_columns(x)}),
        category=list_reavalues$col_categories,
        modality=sapply(list_reavalues$table,
                        function(x){length(unique(x))}),
        missing_values=sapply(list_reavalues$table,
                              function(x){sum(is.na(x))}))
      
      colnames(list_reavalues$type_table)[5]="Number of Missing Values"
      colnames(list_reavalues$type_table)[4]="Number of Modalities"
      rownames(list_reavalues$type_table) = NULL
      col_names=names(list_reavalues$table)
      output$input_varchoice=renderUI({
        selectInput("input_varchoice",
                    label="Choisissez une variable",
                    choices=col_names,
                    selected = col_names[1])
      })
    }
    list_reavalues$modify_features=list_reavalues$modify_features+1
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  observeEvent(list_reavalues$modify_type["Exploration"],{
    if (! identical(input$input_exp_type,NULL)){
      col_select=input$input_varchoice
      col_category=get_type_col_by_name(list_reavalues$type_table,col_select)
      if (input$input_exp_type == "Univariée"){
        if (col_select == input$input_uni_select_var){
          choices=get_vizualizations(col_category)
          selected=choices[1]
          output$input_uni_select_viz=renderUI({
            selectInput("input_uni_select_viz",
                        label="Selectionner votre visualisation",
                        choices=choices,
                        selected=selected)
          })
        }
      } else if (input$input_exp_type == "Bivariée"){
        if (col_select %in% input$input_bi_select_var && 
            length(input$input_bi_select_var) == 2){
          other_col <- setdiff(input$input_bi_select_var, col_select)
          type_col_select=get_type_col_by_name(list_reavalues$type_table,
                                               col_select)
          type_col_other=get_type_col_by_name(list_reavalues$type_table,
                                              other_col)
          choices=get_vizualizations_bi(c(type_col_select,type_col_other))
          selected=choices[1]
          output$input_bi_select_viz=renderUI({
            selectInput("input_bi_select_viz",
                        label="Selectionner votre visualisation",
                        choices=choices,
                        selected=selected)
          })

        }

      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(list_reavalues$modify_type["Apprentissage de Modèles"],{
    qualitative_vars=list_reavalues$type_table %>%
      filter(category == "qualitative ordinale" | 
               category == "qualitative nominale" ) %>%
      select(variable)
    qualitative_vars=qualitative_vars[["variable"]]
    if (length(qualitative_vars)==0){
      output$input_model_outcome_bin=renderUI({
      })
      output$input_model_poschoice_bin=renderUI({
      })
      output$input_model_features_bin=renderUI({
      })
      output$input_valid_model_bin=renderUI({
      })
    }else{
      features=setdiff(list_reavalues$type_table[["variable"]],qualitative_vars[1])
      if(length(features)>0){
        output$input_model_outcome_bin=renderUI({
          selectInput("input_model_outcome_bin",
                      label="Variable à prédire",
                      choices=qualitative_vars,
                      selected =qualitative_vars[1])
        })
        modality_outcome=unique(list_reavalues$table[[qualitative_vars[1]]])
        output$input_model_poschoice_bin=renderUI({
          selectInput("input_model_poschoice_bin",
                      label="Choix de la modalité positive",
                      choices=modality_outcome,
                      selected =modality_outcome[1])
        })
        output$input_model_features_bin=renderUI({
          selectizeInput("input_model_features_bin",
                         label="Selectionner vos features",
                         choices=features,
                         multiple=TRUE,
                         options = list(maxItems = length(features)),
                         selected =features[1])
        })
        output$input_valid_model_bin=renderUI({
          actionButton("input_valid_model_bin",
                       label="Valider les options")
        })
      }else{
        output$input_model_outcome_bin=renderUI({
        })
        output$input_model_poschoice_bin=renderUI({
        })
        output$input_model_features_bin=renderUI({
        })
        output$input_valid_model_bin=renderUI({
        })
      }

    }
    features=setdiff(list_reavalues$type_table[["variable"]],qualitative_vars[1])
    col_categoris_who_outco=
      list_reavalues$col_categories[!names(list_reavalues$col_categories) %in% 
                                      c(qualitative_vars[1])]
    features_quant=features[col_categoris_who_outco %in% 
                              c("quantitative continue","quantitative discrète")]
    if (length(features) != length(features_quant)){
      modeles_type=MODELES_BIN[!MODELES_BIN %in% c("Régression Logistique")]
      output$input_model_type_bin=renderUI({
        selectInput("input_model_type_bin",
                    label="Selectionner votre modèle",
                    choices=modeles_type,
                    selected =modeles_type[1])
      })
    }else{
      output$input_model_type_bin=renderUI({
        selectInput("input_model_type_bin",
                    label="Selectionner votre modèle",
                    choices=MODELES_BIN,
                    selected =MODELES_BIN[1])
      })
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)




  observeEvent(list_reavalues$modify_features["Exploration"],{
    choices=list_reavalues$type_table[["variable"]]
    if (ifelse(identical(input$input_uni_select_var,NULL),
               "",
               input$input_uni_select_var) %in% choices){
      selected_uni=input$input_uni_select_var
    }else{
      selected_uni=choices[1]
    }
    if (ifelse(identical(input$input_bi_select_var,NULL),
               "",
               input$input_bi_select_var) %in% choices){
      selected_bi=input$input_bi_select_var
    }else{
      selected_bi=choices[1]
    }
    
    output$input_uni_select_var=renderUI({
      selectInput("input_uni_select_var",
                  label="Selectionner votre variable",
                  choices=choices,
                  selected = selected_uni)
    })
    output$input_bi_select_var=renderUI({
      selectizeInput("input_bi_select_var",
                     label="Selectionner vos deux variables (x resp y)",
                     choices=choices,
                     multiple=TRUE,
                     options = list(maxItems = 2),
                     selected =selected_bi)
    })
  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(list_reavalues$modify_features["Apprentissage de Modèles"],{
    qualitative_vars=list_reavalues$type_table %>%
      filter(category == "qualitative ordinale" | 
               category == "qualitative nominale" ) %>%
      select(variable)
    qualitative_vars=qualitative_vars[["variable"]]
    if (length(qualitative_vars)==0){
      output$input_model_outcome_bin=renderUI({
      })
      output$input_model_poschoice_bin=renderUI({
      })
      output$input_model_features_bin=renderUI({
      })
      output$input_valid_model_bin=renderUI({
      })
    }else{
      features=setdiff(list_reavalues$type_table[["variable"]],
                       qualitative_vars[1])
      if(length(features)>0){
        output$input_model_outcome_bin=renderUI({
          selectInput("input_model_outcome_bin",
                      label="Variable à prédire",
                      choices=qualitative_vars,
                      selected =qualitative_vars[1])
        })
        modality_outcome=unique(list_reavalues$table[[qualitative_vars[1]]])
        output$input_model_poschoice_bin=renderUI({
          selectInput("input_model_poschoice_bin",
                      label="Choix de la modalité positive",
                      choices=modality_outcome,
                      selected =modality_outcome[1])
        })
        output$input_model_features_bin=renderUI({
          selectizeInput("input_model_features_bin",
                         label="Selectionner vos features",
                         choices=features,
                         multiple=TRUE,
                         options = list(maxItems = length(features)),
                         selected =features[1])
        })
        output$input_valid_model_bin=renderUI({
          actionButton("input_valid_model_bin",
                       label="Valider les options")
        })
      }else{
        output$input_model_outcome_bin=renderUI({
        })
        output$input_model_poschoice_bin=renderUI({
        })
        output$input_model_features_bin=renderUI({
        })
        output$input_valid_model_bin=renderUI({
        })
      }

    }
    features=setdiff(list_reavalues$type_table[["variable"]],qualitative_vars[1])
    col_categoris_who_outco=
      list_reavalues$col_categories[!names(list_reavalues$col_categories) %in% 
                                      c(qualitative_vars[1])]
    features_quant=features[col_categoris_who_outco %in% 
                              c("quantitative continue","quantitative discrète")]
    if (length(features) != length(features_quant)){
      modeles_type=MODELES_BIN[!MODELES_BIN %in% c("Régression Logistique")]
      output$input_model_type_bin=renderUI({
        selectInput("input_model_type_bin",
                    label="Selectionner votre modèle",
                    choices=modeles_type,
                    selected =modeles_type[1])
      })
    }else{
      output$input_model_type_bin=renderUI({
        selectInput("input_model_type_bin",
                    label="Selectionner votre modèle",
                    choices=MODELES_BIN,
                    selected =MODELES_BIN[1])
      })
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_valid_type, {
    col_select=input$input_varchoice
    col_category=input$input_type_varchoice
    list_reavalues$table[,col_select]=
      process_type(list_reavalues$table[col_select],c(col_category))
    list_reavalues$table_all[,col_select]=
      process_type(list_reavalues$table_all[col_select],c(col_category))
    list_reavalues$col_categories_all[col_select]=col_category
    list_reavalues$col_categories[col_select]=col_category
    list_reavalues$type_table=data.frame(
      variable = colnames(list_reavalues$table), 
      type = sapply(list_reavalues$table, function(x){get_type_columns(x)}),
      category=list_reavalues$col_categories,
      modality=sapply(list_reavalues$table,function(x){length(unique(x))}),
      missing_values=sapply(list_reavalues$table,function(x){sum(is.na(x))}))
    
    colnames(list_reavalues$type_table)[5]="Number of Missing Values"
    colnames(list_reavalues$type_table)[4]="Number of Modalities"
    
    list_reavalues$modify_type=list_reavalues$modify_type+1
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  observeEvent(input$input_valid_outliers, {
    if (input$input_var_outliers == TRUE){
      col_select=input$input_varchoice
      category=get_type_col_by_name(list_reavalues$type_table,col_select)
      if (category == "quantitative discrète" || 
          category == "quantitative continue"){
        process=
          process_outliers(list_reavalues$table,
                           list_reavalues$table_all,
                           list_reavalues$type_table,
                           col_select)
        list_reavalues$table=process[["table"]]
        list_reavalues$table_all=process[["table_all"]]
        list_reavalues$type_table=process[["type_table"]]
      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  observeEvent(input$input_valid_normalize, {
    if (input$input_var_normalize == TRUE){
      list_reavalues$modify_type=list_reavalues$modify_type+1
      col_select=input$input_varchoice
      category=get_type_col_by_name(list_reavalues$type_table,col_select)
      if (category == "quantitative discrète" ||
          category == "quantitative continue"){
        process=
          process_normalize(list_reavalues$table,
                            list_reavalues$table_all,list_reavalues$type_table,
                            list_reavalues$col_categories_all,col_select)
        
        list_reavalues$table=process[["table"]]
        list_reavalues$table_all=process[["table_all"]]
        list_reavalues$type_table=process[["type_table"]]
        list_reavalues$col_categories=list_reavalues$type_table[["category"]]
        names(list_reavalues$col_categories)=list_reavalues$type_table[["variable"]]
        list_reavalues$col_categories_all=process[["col_categories_all"]]
      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_valid_dummy, {
    if(!identical(input$input_varchoice,NULL)){
      selected_var=input$input_varchoice
      regex_pattern = paste("^", input$input_varchoice, "_",sep="")[1]
      if (input$input_var_dummy == TRUE &&  
          sum(grepl(regex_pattern, colnames(list_reavalues$table_all))) == 0){
        
        list_reavalues$modify_type=list_reavalues$modify_type+1
        list_reavalues$modify_features=list_reavalues$modify_features+1
        col_select=input$input_varchoice
        category=get_type_col_by_name(list_reavalues$type_table,col_select)
        if (category == "qualitative nominale" || 
            category == "qualitative ordinale"){
          
          process=process_dummy(list_reavalues$table,list_reavalues$table_all,list_reavalues$type_table,
                                list_reavalues$col_categories_all,col_select)
          list_reavalues$table=process[["table"]]
          list_reavalues$table_all=process[["table_all"]]
          list_reavalues$type_table=process[["type_table"]]
          list_reavalues$col_categories=list_reavalues$type_table[["category"]]
          names(list_reavalues$col_categories)=list_reavalues$type_table[["variable"]]
          list_reavalues$col_categories_all=process[["col_categories_all"]]
        }
        
        col_names=list_reavalues$type_table[["variable"]]
        choices_all=c(c("#Toutes"),colnames(list_reavalues$table_all))
        output$input_varschoice=renderUI({
          selectInput("input_varschoice",
                      label="Choisissez les variables à étudier",
                      choices=choices_all,
                      selected = col_names ,
                      multiple = TRUE)
        })
        output$input_varchoice=renderUI({
          selectInput("input_varchoice",
                      label="Choisissez une variable",
                      choices=col_names,
                      selected = selected_var)
        })

      }
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(input$input_process, {
    list_reavalues$table_all=
      process_type(list_reavalues$table_all,list_reavalues$col_categories_all)
    list_reavalues$table=
      process_type(list_reavalues$table,list_reavalues$col_categories)
    list_reavalues$table=
      process_type(list_reavalues$table,list_reavalues$col_categories)
    list_reavalues$type_table=data.frame(
      variable = colnames(list_reavalues$table),
      type=sapply(list_reavalues$table,function(x){get_type_columns(x)}),
      category = list_reavalues$col_categories,
      modality=sapply(list_reavalues$table,function(x){length(unique(x))}),
      missing_values=sapply(list_reavalues$table,function(x){sum(is.na(x))}))
    
    colnames(list_reavalues$type_table)[5]="Number of Missing Values"
    colnames(list_reavalues$type_table)[4]="Number of Modalities"
    
    qualitative_vars=list_reavalues$type_table %>%
      filter(category == "qualitative ordinale" | 
               category == "qualitative nominale" ) %>%
      select(variable)
    qualitative_vars=qualitative_vars[["variable"]]
    
    output$input_model_outcome_bin=renderUI({
      selectInput("input_model_outcome_bin",
                  label="Variable à prédire",
                  choices=qualitative_vars,
                  selected =qualitative_vars[1])
    })
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  #------------------------------------------------------------------------------------------------------------#

  observeEvent(input$input_uni_select_var, {
    if (input$input_exp_type == "Univariée"){
      if (length(input$input_uni_select_var) == 1){
        type_col=get_type_col_by_name(list_reavalues$type_table,
                                      input$input_uni_select_var)
        choices=get_vizualizations(type_col)
        selected=choices[1]
        output$input_uni_select_viz=renderUI({
          selectInput("input_uni_select_viz",
                      label="Selectionner votre visualisation",
                      choices=choices,
                      selected=selected)
        })
      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_uni_select_viz, {
    if (bins_require_viz_uni(input$input_uni_select_viz) == TRUE){
      output$bins_input_uni=renderUI({
        selectInput("bins_input_uni",
                    label="Nombre de bacs",
                    choices=1:15,
                    selected=7)
      })
    } else{
      output$bins_input_uni=renderUI({})
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)




  bins_require_viz_uni
  observeEvent(input$input_bi_select_var, {
    if (input$input_exp_type == "Bivariée"){
      if (length(input$input_bi_select_var)==2){
        col1=input$input_bi_select_var[1]
        col2=input$input_bi_select_var[2]
        type_col1=get_type_col_by_name(list_reavalues$type_table,col1)
        type_col2=get_type_col_by_name(list_reavalues$type_table,col2)
        choices=get_vizualizations_bi(c(type_col1,type_col2))
        selected=choices[1]
        output$input_bi_select_viz=renderUI({
          selectInput("input_bi_select_viz",
                      label="Selectionner votre visualisation",
                      choices=choices,
                      selected=selected)
        })
      } else {
        output$input_bi_select_viz=renderUI({
          selectInput("input_bi_select_viz",
                      label="Selectionner votre visualisation",
                      choices=NULL)
        })

      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_bi_select_viz, {
    if(!identical(input$input_bi_select_var,NULL)){
      if (length(input$input_bi_select_var) ==2){
        if (bins_require_viz_bi(
          input$input_bi_select_viz,
          c(list_reavalues$col_categories[input$input_bi_select_var[1]],
          list_reavalues$col_categories[input$input_bi_select_var[2]])) == TRUE){
          
          output$bins_input_bi=renderUI({
            selectInput("bins_input_bi",
                        label="Nombre de bacs",
                        choices=1:15,
                        selected=7)
          })
        } else{
          output$bins_input_bi=renderUI({})
        }
      }
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(input$input_valid_exp,{
    if (input$input_exp_type == "Univariée") {
      df_col = list_reavalues$table[input$input_uni_select_var]
      viz_type = input$input_uni_select_viz
      if (viz_type == "Statistiques"){
        output$viz_input = renderUI({
          renderTable(statistiques(df_col))
        })
      }
      if (viz_type == "Tableau des Effectifs/Fréquence") {
        type_col=get_type_col_by_name(list_reavalues$type_table,
                                      input$input_uni_select_var)
        if (type_col == "qualitative nominale" || 
            type_col == "qualitative ordinale"){
          output$viz_input = renderUI({
            renderTable(tableau_effectifs_freq_qualitative(df_col))
          })
        } else if(type_col== "quantitative discrète"){
          output$viz_input = renderUI({
            renderTable(create_freq_table_discreet(df_col))
          })
        }

      }else if (viz_type == "Diagramme en bâtons") {
        output$viz_input = renderUI({
          renderPlot({create_bar_plot_discreet(df_col)})
        })
      } else if (viz_type == "Fonction de répartition empirique") {
        type_col=get_type_col_by_name(list_reavalues$type_table,
                                      input$input_uni_select_var)
        if (type_col=="quantitative discrète"){
          output$viz_input = renderUI({
            renderPlot({create_ecdf_plot_discreet(df_col)})
          })
        }else if (type_col=="quantitative continue"){
          output$viz_input = renderUI({
            renderPlot({create_ecdf_plot_continous(df_col)})
          })
        }
      } else if (viz_type == "Histogramme") {
        output$viz_input = renderUI({
          renderPlot({
            create_histogram_continous(df_col,
            nbins= as.integer(input$bins_input_uni))})
        })
      } else if (viz_type == "Tableau des Effectifs/Fréquence par classe") {
        output$viz_input = renderUI({
          renderTable(
            create_freq_table_continous(
              df_col,
              nbins= as.integer(input$bins_input_uni)))
        })
      } else if (viz_type == "Fonction de répartition empirique") {
        type_col=
          get_type_col_by_name(list_reavalues$type_table,
                               input$input_uni_select_var)
        if (type_col == "quantitative discrète"){
          output$viz_input = renderUI({
            renderPlot({create_ecdf_plot_discreet(df_col)})
          })
        }else if (type_col == "quantitative continue"){
          output$viz_input = renderUI({
            renderPlot({create_ecdf_plot_continous(df_col)})
          })
        }

      } else if (viz_type == "Diagramme en barres") {
        output$viz_input = renderUI({
          renderPlot(diag_barres_qualitative(df_col))
        })
      } else if (viz_type == "Diagramme circulaire") {
        output$viz_input = renderUI({
          renderPlot({diag_circulaire_qualitative(df_col)})
        })
      }
    }
    if (input$input_exp_type == "Bivariée" && length(input$input_bi_select_var) == 2){
      df_cols = list_reavalues$table[input$input_bi_select_var]
      viz_type = input$input_bi_select_viz
      type_col1=
        list_reavalues$col_categories[
          match(
          input$input_bi_select_var[1], 
          names(list_reavalues$col_categories))
          ]
      type_col2=
        list_reavalues$col_categories[
          match(input$input_bi_select_var[2], 
                names(list_reavalues$col_categories))
          ]
      type_cols=c(type_col1,type_col2)
      if (viz_type == "Statistiques"){
        table=two_var_statistics(
          df_cols,
          type_cols = c(type_col1,type_col2),
          nbins=as.integer(input$bins_input_bi))
        
        # display
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Tableau de Contingence en Effectif"){
        table=
          two_var_contingency_table(
            df_cols,
            type_cols = c(type_col1,type_col2),
            nbins = as.integer(input$bins_input_bi))
        
        # display
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Tableau de Contingence en Fréquence"){
        table=
          two_var_contingency_table(
            df_cols,
            type_cols = type_cols,
            with_freq=TRUE,
            nbins = as.integer(input$bins_input_bi))
        
        # display
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Boite à Moustaches"){
          #Y\X --> Y:col2 and X:col1
          output$viz_input = renderUI({
            renderPlot(
              two_var_boxplot_qual_var_cond(
                df_cols,
                type_cols = type_cols,
                nbins=as.integer(input$bins_input_bi)))
        })
      } else if (viz_type == "Diagramme en Barres"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_barplot_cond(
            df_cols,
            type_cols = type_cols,
            nbins=as.integer(input$bins_input_bi)))
        })
      } else if (viz_type == "TreeMap"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_treemap_2_var(
            df_cols,
            type_cols = type_cols,
            nbins=as.integer(input$bins_input_bi)))
        })
      } else if (viz_type == "Nuage de points"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_scatterplot(df_cols,type_cols = type_cols))
        })
      }else if (viz_type == "Corrélations"){
        table=two_var_correlations(df_cols,type_cols = c(type_col1,type_col2))
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      }

    }
    if (input$input_exp_type == "Multivariée"){
      df=list_reavalues$table
      if (input$input_multi_select_viz == "Matrice de corrélations"){
        table=multi_var_correlations(df,list_reavalues$col_categories)
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  #------------------------------------------------------------------------------------------------------------#




  observeEvent(input$input_model_outcome_bin, {
    features=setdiff(colnames(list_reavalues$table),
                     c(input$input_model_outcome_bin))
    
    if (!length(features)){
      output$input_model_features_bin=renderUI({
      })
      output$input_valid_model_bin=renderUI({
      })
    }else{
      output$input_model_features_bin=renderUI({
        selectizeInput("input_model_features_bin",
                       label="Selectionner vos features",
                       choices=features,
                       multiple=TRUE,
                       options = list(maxItems = length(features)),
                       selected =features[1])
      })
      output$input_valid_model_bin=renderUI({
        actionButton("input_valid_model_bin",
                     label="Valider les options")
      })
      modality_outcome=unique(list_reavalues$table[[input$input_model_outcome_bin]])
      output$input_model_poschoice_bin=renderUI({
        selectInput("input_model_poschoice_bin",
                    label="Choix de la modalité positive",
                    choices=modality_outcome,
                    selected =modality_outcome[1])
      })
    }
    if (list_reavalues$type_table[
      list_reavalues$type_table["variable"] == 
      input$input_model_outcome_bin, "Number of Modalities"][1] == 2){

      counts=table(list_reavalues$table[input$input_model_outcome_bin])
      all_counts_equal = min(counts) == max(counts)
      if(!all_counts_equal){
        value=counts[which.min(counts)]/sum(counts)
        p_min=round(value,digits = 2)
        
        output$input_prop_minclass=renderUI({
          sliderInput("input_prop_minclass",label="Prop. classe minoritaire",
                      min=p_min,
                      max=0.5,
                      value=p_min,
                      step=.01)
        })
        output$input_type_unbalanced=renderUI({
          selectInput("input_type_unbalanced",
                      label="Choisir le type d'équilibrage",
                      choices=c("Oversampling","Undersampling","Les Deux"),
                      selected="Oversampling")
        })
        output$input_valid_unbalanced=renderUI({
          actionButton("input_valid_unbalanced","Valider Over/Under sampling")
        })
      }else{
        output$input_type_unbalanced=renderUI({
        })
        output$input_prop_minclass=renderUI({
        })
        output$input_valid_unbalanced=renderUI({
        })
      }
    }else{
      output$input_type_unbalanced=renderUI({})
      output$input_prop_minclass=renderUI({})
      output$input_valid_unbalanced=renderUI({})
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(input$input_valid_unbalanced,{

    if(
        !identical(list_reavalues$col_categories,NULL) &&
        !identical(input$input_model_outcome_bin,NULL) &&
        !identical(input$input_type_unbalanced,NULL) &&
        !identical(input$input_varschoice,NULL)
    ){
      outcome=list_reavalues$table_all[[input$input_model_outcome_bin]]
      counts = table(outcome)
      index_min=which.min(counts)
      list_reavalues$table_all=
        process_type(list_reavalues$table_all,
                     list_reavalues$col_categories_all)
      p=input$input_prop_minclass
      method=
        ifelse(input$input_type_unbalanced == "Oversampling",
               "over",
               ifelse(input$input_type_unbalanced == "Undersampling",
                      "under",
                      "both"))
      
      f=paste(input$input_model_outcome_bin,".",sep="~")
      df_for_sample=list_reavalues$table_all
      col_names=colnames(list_reavalues$table_all)
      colnames(df_for_sample)=gsub("-","_",colnames(df_for_sample))
      if (list_reavalues$col_categories[input$input_model_outcome_bin] == 
          "qualitative ordinale"){
        list_reavalues$table_all[input$input_model_outcome_bin]=
          factor(list_reavalues$table_all[[input$input_model_outcome_bin]],
                 ordered=FALSE)
      }else{
      }
      if (method =="both"){
        df_new=do.call(ovun.sample, 
                       list(as.formula(f), 
                            data=df_for_sample,
                            method=method,
                            p=p,
                            seed=1,
                            N=nrow(df_for_sample)))$data
      }else if(method== "over"){
        df_new=do.call(ovun.sample, 
                       list(as.formula(f), 
                            data=df_for_sample,method=method,p=p,seed=1))$data
      }else if (method == "under"){
        minority_values=counts[index_min]
        df_new=do.call(ovun.sample, list(as.formula(f), 
                                         data=df_for_sample,
                                         method=method,
                                         N=minority_values/p,seed=1))$data
      }

      #------------------------------------------------------------------#
      list_reavalues$table_all=df_new
      colnames(list_reavalues$table_all)=col_names
      if (list_reavalues$col_categories[input$input_model_outcome_bin] == 
          "qualitative ordinale"){
        list_reavalues$table_all[input$input_model_outcome_bin]=
          factor(list_reavalues$table_all[[input$input_model_outcome_bin]],
                 ordered=TRUE)
      }else{
      }
      if("#Toutes" %in% input$input_varschoice){
        list_reavalues$table=as.data.frame(list_reavalues$table_all)
      }else{
        list_reavalues$table=as.data.frame(
          list_reavalues$table_all[input$input_varschoice])
      }
      list_reavalues$type_table=data.frame(
        variable = colnames(list_reavalues$table), 
        type = sapply(list_reavalues$table, 
                      function(x){get_type_columns(x)}),
        
        category=list_reavalues$col_categories,
        modality=sapply(list_reavalues$table,
                        function(x){length(unique(x))}),
        missing_values=sapply(list_reavalues$table,
                              function(x){sum(is.na(x))}))
      
      colnames(list_reavalues$type_table)[5]="Number of Missing Values"
      colnames(list_reavalues$type_table)[4]="Number of Modalities"
      rownames(list_reavalues$type_table) = NULL

      #------------------------------------------------------------------#
      counts_new=table(list_reavalues$table[[input$input_model_outcome_bin]])
      index_min_new=which.min(counts_new)
      value=counts_new[index_min_new]/sum(counts_new)
      p_min=round(value,digits = 2)+0.01
      if(p_min>0.5){
        output$input_prop_minclass=renderUI({
        })
        output$input_type_unbalanced=renderUI({
        })
        output$input_valid_unbalanced=renderUI({
        })
      }
      else{
        p=p_min
        output$input_prop_minclass=renderUI({
          sliderInput("input_prop_minclass",label="Prop. classe minoritaire",min=p,
                      max=0.5,value=p,step=.01)
        })
      }

    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_valid_model_bin,{
    outcome=as.character(input$input_model_outcome_bin)
    mod_positive=as.character(input$input_model_poschoice_bin)
    features=input$input_model_features_bin
    df=list_reavalues$table[,c(outcome,features)]
    outcome_model=df[outcome]==mod_positive
    df[,outcome]=outcome_model
    col_categories=list_reavalues$col_categories[
      names(list_reavalues$col_categories) %in% 
        c(outcome,features)][c(outcome,features)]
    
    prop=as.numeric(input$input_proportion_bin)
    threshold=as.numeric(input$input_threshold_bin)
    model=NULL
    model_name=NULL
    true_labels_test=NULL
    pred_prob_test=NULL
    
    if (input$input_model_type_bin == "Arbre de décision CART"){
      
      model_name="Arbre de décision CART"
      prune=input$input_model_pruned_bin
      model_results=decision_tree_model(df[,c(outcome,features)],
                                        outcome,col_categories = 
                                          col_categories,method="CART",
                                        prop=prop,prune=prune)
      
      model=model_results[[1]]
      informations=display_informations(model,"Arbre de décision CART")
      true_labels_test=as.logical(unlist(model_results[2]))
      pred_prob_test=unlist(model_results[3])
    }
    if (input$input_model_type_bin == "Arbre de décision CHAID"){
      nbins=as.integer(input$input_model_bins_bin)
      
      model_name="Arbre de décision CHAID"
      model_results=decision_tree_model(df[,c(outcome,features)], 
                                        outcome, 
                                        col_categories = col_categories,
                                        method= "CHAID", 
                                        prop=prop, 
                                        nbins=nbins)
      model=model_results[[1]]
      informations=display_informations(model,"Arbre de décision CHAID")
      true_labels_test=as.logical(unlist(model_results[2]))
      pred_prob_test=unlist(model_results[3])
    }
    if (input$input_model_type_bin == "Régression Logistique"){
      
      model_name="Régression Logistique"
      model_results=logistic_regression(df[,c(outcome,features)],
                                        outcome,
                                        col_categories = col_categories,
                                        prop=prop)
      model=model_results[[1]]
      informations=display_informations(model,"Régression Logistique")
      true_labels_test=as.logical(unlist(model_results[2]))
      pred_prob_test=unlist(model_results[3])
    }


    metrics_confusion=calculate_metrics(true_labels_test,
                                        pred_prob_test>threshold)
    metrics=as.data.frame(metrics_confusion["metrics"])
    colnames(metrics)=c("Accuracy","Error Rate","Precision","Recall","F1-score")

    output$input_viz_model_bin = renderUI({
      renderPlot(plot_tree_model(model,model_name))
    })
    if (length(levels(as.factor(true_labels_test))) == 2){
      output$input_viz_auc_bin = renderUI({
        renderPlot(plot_roc_auc_curve(true_labels_test,pred_prob_test))
      })
    }else{
      showModal(modalDialog(
        title = "Information",
        paste("Dans le jeu de données de test,",
              outcome,
              " n'a pas exactement deux modalités:",
              mod_positive,
              "et Non",
              mod_positive),
        easyClose = TRUE
      ))
      output$input_viz_auc_bin = renderUI({
      })
    }


    output$input_metrics_model_bin = renderUI({
      renderTable(metrics)
    })
    confusion_matrix=metrics_confusion["confusion_matrix"]
    output$input_confusion_model_bin = renderUI({
      renderTable(confusion_matrix,rownames = TRUE,colnames = TRUE)
    })
    output$input_viz_informations_bin = renderUI({
      renderTable(informations,rownames = TRUE,colnames = TRUE)
    })

  },ignoreNULL = TRUE,ignoreInit = TRUE)


  output$input_doc=  renderUI({
    includeHTML(path="./www/doc/doc.html")
  })
  output$input_fileSkip_csv = renderUI({
    sliderInput("input_fileSkip_csv",
                label="Ignorer les premières lignes?",
                min=0,
                max=15,
                value=0,
                step=1)
  })
  output$input_fileSkip_integrate = renderUI({
    sliderInput("input_fileSkip_integrate",
                label="Ignorer les premières lignes?",
                min=0,
                max=LINES[[as.character(input$input_intdataset)]],
                value=0,
                step=1)
  })


  output$output_table = renderDataTable({
    if(!is.null(list_reavalues$table)) 
      DT::datatable(list_reavalues$table,options = list(scrollX=TRUE))
  })
  output$output_type_table = renderDataTable({
    if(!is.null(list_reavalues$type_table)) 
      DT::datatable(
        list_reavalues$type_table,
        options = list(scrollX=TRUE,scrollY="251px"))
  })
}