if(! require("comprehenr")) install.packages("comprehenr")
library(comprehenr)

server = function (input, output, session) {
  source("./back/src/dataset/globals_dataset.R")
  source("./back/src/dataset/processing.R")
  source("./back/src/exploration/globals_exploration.R")
  source("./back/src/exploration/vizualizations.R")
  source("./back/src/machine_learning/globals_machine_learning.R")
  source("./back/src/machine_learning/models.R")

  options(shiny.maxRequestSize=30*1024^2) #maximum size for uploading
  list_reavalues=reactiveValues(
    table=NULL, #initialization
    table_all=NULL,
    type_table=NULL,
    col_categories=NULL,
    col_categories_all=NULL,
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
          list_reavalues$table_all=personnalFile
          list_reavalues$table=data.frame(list_reavalues$table_all)

          list_reavalues$col_categories_all=get_categories(list_reavalues$table_all)
          list_reavalues$col_categories=get_categories(list_reavalues$table)
          list_reavalues$type_table=data.frame(variable = colnames(list_reavalues$table), type = sapply(list_reavalues$table, function(x){get_type_columns(x)}),
                                               category=list_reavalues$col_categories)

          rownames(list_reavalues$type_table) = NULL
          number_click_process=0 #initialization of process

          col_names=names(list_reavalues$table)
          output$input_varschoice=renderUI({
            selectInput("input_varschoice",label="Choisissez les variables à étudier",choices=cbind("#Toutes",col_names),selected = "#Toutes",multiple = TRUE)
          })
          output$input_varchoice=renderUI({
            selectInput("input_varchoice",label="Choisissez une variable",choices=col_names,selected = col_names[1])
          })
          output$input_type_varchoice=renderUI({
            selectInput("input_type_varchoice",label="Choisissez la catégorie de la variable",choices=c("quantitative discrète","quantitative continue","qualitative ordinale","qualitative nominale"),selected="quantitative discrète")
          })
          output$input_valid_type=renderUI({
            actionButton("input_valid_type",label="Valider la catégorie de la variable")
          })
          output$input_process=renderUI({
            actionButton("input_process",label="Faire le processing du DataFrame final")
          })

          #-------------------------------------------------------------------------------------------#
          output$input_valid_exp=renderUI({
            actionButton("input_valid_exp",label="Valider les options")

          })
          output$input_exp_type=renderUI({
            selectInput("input_exp_type",
                        label="Choisissez le type d'exploration",
                        choices=c("Univariée","Bivariée"),
                        selected = "Univariée"
            )
          })
          output$input_uni_select_viz=renderUI({
            selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=get_vizualizations(get_type_col_by_name(list_reavalues$type_table,input$input_varchoice)),selected =get_vizualizations(get_type_col_by_name(list_reavalues$type_table,input$input_varchoice))[1])
          })
          output$input_bi_select_viz=renderUI({
            selectInput("input_bi_select_viz",label="Selectionner votre visualisation",choices=NULL)
          })
          output$input_uni_select_var=renderUI({
            selectInput("input_uni_select_var",label="Selectionner votre variable",choices=col_names,selected =input$input_varchoice )
          })
          output$input_bi_select_var=renderUI({
            selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=col_names,multiple=TRUE,options = list(maxItems = 2),selected =list_reavalues$col_names[1])
          })
          #-------------------------------------------------------------------------------------------#
          output$input_model_type=renderUI({
            selectInput("input_model_type",label="Choix du type de modèle",choices=c("Classification Binaire","Classification Multi-Classes"),selected ="Classification Binaire")
          })
          quantitative_vars=list_reavalues$type_table %>%
            filter(category == "qualitative ordinale" | category == "qualitative nominale" ) %>%
            select(variable)
          output$input_model_type_bin=renderUI({
            selectInput("input_model_type_bin",label="Selectionner votre modèle",choices=MODELES_BIN,selected =MODELES_BIN[1])
          })
          output$input_proportion_bin=renderUI({
            sliderInput("input_proportion_bin",label="Choisir la proportion pour le train Dataset",min=0.01,max=1,value=0.7,step=0.01)
          })
          output$input_threshold_bin=renderUI({
            sliderInput("input_threshold_bin",label="Choisir le seuil d'acceptation (probabilités)",min=0,max=1,value=0.5,step=0.01)
          })


          output$input_model_outcome_bin=renderUI({
            selectInput("input_model_outcome_bin",label="Variable à prédire",choices=quantitative_vars,selected =quantitative_vars[1])
          })
          modality_outcome=unique(list_reavalues$table[quantitative_vars[1]])
          output$input_model_poschoice_bin=renderUI({
            selectInput("input_model_poschoice_bin",label="Choix de la modalité positive",choices=modality_outcome,selected =modality_outcome[1])
          })


        }, silent = TRUE)
      }
    }
  })
  
  observeEvent(input$input_varschoice, {

    if(!"#Toutes" %in% input$input_varschoice){
      list_reavalues$table=as.data.frame(list_reavalues$table_all[input$input_varschoice])
      indices_col = match(input$input_varschoice, names(list_reavalues$col_categories_all))
      list_reavalues$col_categories=list_reavalues$col_categories_all[indices_col]
      list_reavalues$type_table=data.frame(variable = colnames(list_reavalues$table), type = sapply(list_reavalues$table, function(x){get_type_columns(x)}),
                                           category=list_reavalues$col_categories)
      rownames(list_reavalues$type_table) = NULL
      output$input_varchoice=renderUI({
        selectInput("input_varchoice",label="Choisissez une variable",choices=input$input_varschoice,selected = input$input_varschoice[1])
      })
      #---------------------------------------------------------#
      output$input_uni_select_var=renderUI({
        selectInput("input_uni_select_var",label="Selectionner votre variable",choices=input$input_varschoice,selected = input$input_varschoice[1])
      })
      output$input_bi_select_var=renderUI({
        selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=input$input_varschoice,multiple=TRUE,options = list(maxItems = 2),selected =input$input_varschoice[1])
      })
      #--------------------------------------------------------#
      quantitative_vars=list_reavalues$type_table %>%
        filter(category == "qualitative ordinale" | category == "qualitative nominale" ) %>%
        select(variable)
      output$input_model_outcome_bin=renderUI({
        selectInput("input_model_outcome_bin",label="Variable à prédire",choices=quantitative_vars,selected =quantitative_vars[1])
      })



    }
    else{
      list_reavalues$table=data.frame(list_reavalues$table_all)
      list_reavalues$col_categories=list_reavalues$col_categories_all
      list_reavalues$type_table=data.frame(variable = colnames(list_reavalues$table), type = sapply(list_reavalues$table, function(x){get_type_columns(x)}),
                                           category=list_reavalues$col_categories)
      rownames(list_reavalues$type_table) = NULL
      col_names=names(list_reavalues$table)
      output$input_varchoice=renderUI({
        selectInput("input_varchoice",label="Choisissez une variable",choices=col_names,selected = col_names[1])
      })
      output$input_uni_select_var=renderUI({
        selectInput("input_uni_select_var",label="Selectionner votre variable",choices=col_names,selected = col_names[1])
      })
      output$input_bi_select_var=renderUI({
        selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=col_names,multiple=TRUE,options = list(maxItems = 2),selected =col_names[1])
      })
      #--------------------------------------------------------#
      quantitative_vars=list_reavalues$type_table %>%
        filter(category == "qualitative ordinale" | category == "qualitative nominale" ) %>%
        select(variable)
      output$input_model_outcome_bin=renderUI({
        selectInput("input_model_outcome_bin",label="Variable à prédire",choices=quantitative_vars,selected =quantitative_vars[1])
      })

    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)
  
  observeEvent(input$input_valid_type, {
    col_select=input$input_varchoice
    col_category=input$input_type_varchoice
    list_reavalues$col_categories_all[col_select]=col_category
    list_reavalues$col_categories[col_select]=col_category
    list_reavalues$type_table=data.frame(variable = colnames(list_reavalues$table), type = sapply(list_reavalues$table, function(x){get_type_columns(x)}),
                                           category=list_reavalues$col_categories)
    if (! identical(input$input_exp_type,NULL)){
      if (input$input_exp_type == "Univariée"){
        if (col_select == input$input_uni_select_var){
          choices=get_vizualizations(col_category)
          selected=choices[1]
          output$input_uni_select_viz=renderUI({
            selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
          })
        }
      } else if (input$input_exp_type == "Bivariée"){
          if (col_select %in% input$input_bi_select_var && length(input$input_bi_select_var) == 2){
            other_col <- setdiff(input$input_bi_select_var, col_select)
            type_col_select=get_type_col_by_name(list_reavalues$type_table,col_select)
            type_col_other=get_type_col_by_name(list_reavalues$type_table,other_col)
            choices=get_vizualizations_bi(c(type_col_select,type_col_other))
            selected=choices[1]
            output$input_bi_select_viz=renderUI({
              selectInput("input_bi_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
            })

          }

      }
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_process, {
    number_click_process=number_click_process+1
    if (number_click_process == 1){
      list_reavalues$table_all=process(list_reavalues$table_all,list_reavalues$col_categories_all)

    }
    list_reavalues$table=process(list_reavalues$table,list_reavalues$col_categories)
    list_reavalues$type_table=data.frame(variable = colnames(list_reavalues$table),type=sapply(list_reavalues$table,function(x){get_type_columns(x)}), category = list_reavalues$col_categories)
  },ignoreNULL = TRUE,ignoreInit = TRUE)

  #------------------------------------------------------------------------------------------------------------#

  observeEvent(input$input_uni_select_var, {
    if (input$input_exp_type == "Univariée"){
      if (length(input$input_uni_select_var) == 1){
        type_col=get_type_col_by_name(list_reavalues$type_table,input$input_uni_select_var)
        choices=get_vizualizations(type_col)
        selected=choices[1]
        output$input_uni_select_viz=renderUI({
          selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
        })
      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)



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
          selectInput("input_bi_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
        })
      } else {
        output$input_bi_select_viz=renderUI({
          selectInput("input_bi_select_viz",label="Selectionner votre visualisation",choices=NULL)
        })

      }
    }
  },ignoreNULL = TRUE,ignoreInit = TRUE)


  observeEvent(input$input_bi_select_viz, {
    if (bins_require_viz(input$input_bi_select_viz) == TRUE){
      output$bins_input=renderUI({
        selectInput("bins_input",label="Nombre de bacs",choices=1:15,selected=7)
      })
    } else{
      output$bins_input=renderUI({})
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(input$input_valid_exp,{
    if (input$input_exp_type == "Univariée") {
      df_col = list_reavalues$table[input$input_uni_select_var]
      viz_type = input$input_uni_select_viz
      print(viz_type)

      if (viz_type == "Statistiques"){
        output$viz_input = renderUI({
          renderTable(statistiques(df_col))
        })
      }
      if (viz_type == "Tableau des Effectifs/Fréquence") {
        type_col=get_type_col_by_name(list_reavalues$type_table,input$input_uni_select_var)
        if (type_col == "qualitative nominale" || type_col == "qualitative ordinale"){
          output$viz_input = renderUI({
            renderTable(tableau_effectifs_freq_qualitative(df_col))
          })
        } else if(type_col== "quantitative discrète"){
          output$viz_input = renderUI({
            renderTable(create_freq_table_discreet(df_col))
          })
        }

      } else if (viz_type == "Tableau des Effectifs/Fréquence par classe") {
        output$viz_input = renderUI({
          renderTable(create_freq_table_continous(df_col,nbins = as.integer(input$bins_input)))
        })
      } else if (viz_type == "Diagramme en bâtons") {
        output$viz_input = renderUI({
          renderPlot({create_bar_plot_discreet(df_col)})
        })
      } else if (viz_type == "Fonction de répartition empirique") {
        type_col=get_type_col_by_name(list_reavalues$type_table,input$input_uni_select_var)
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
          renderPlot({ create_histogram_continous(df_col,nbins= as.integer(input$bins_input))})
        })
      } else if (viz_type == "Tableau des Effectifs/Fréquence par classe") {
        output$viz_input = renderUI({
          renderTable(create_freq_table_continous(df_col,nbins= as.integer(input$bins_input)))
        })
      } else if (viz_type == "Fonction de répartition empirique") {
        type_col=get_type_col_by_name(list_reavalues$type_table,input$input_uni_select_var)
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
      type_col1=list_reavalues$col_categories[match(input$input_bi_select_var[1], names(list_reavalues$col_categories))]
      type_col2=list_reavalues$col_categories[match(input$input_bi_select_var[2], names(list_reavalues$col_categories))]
      type_cols=c(type_col1,type_col2)
      if (viz_type == "Statistiques"){
        table=two_var_statistics(df_cols,type_cols = c(type_col1,type_col2),nbins=as.integer(input$bins_input))
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Tableau de Contingence en Effectif"){
        table=two_var_contingency_table(df_cols,type_cols = c(type_col1,type_col2),nbins = as.integer(input$bins_input))
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Tableau de Contingence en Fréquence"){
        print(viz_type)
        table=two_var_contingency_table(df_cols,type_cols = type_cols,with_freq=TRUE,nbins = as.integer(input$bins_input))
        output$viz_input = renderUI({
          renderTable(table,rownames = TRUE,colnames = TRUE)
        })
      } else if (viz_type == "Boite à Moustaches"){
          #Y\X --> Y:col2 and X:col1
          output$viz_input = renderUI({
            renderPlot(two_var_boxplot_qual_var_cond(df_cols,type_cols = type_cols,nbins=as.integer(input$bins_input)))
        })
      } else if (viz_type == "Diagramme en Barres"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_barplot_cond(df_cols,type_cols = type_cols,nbins=as.integer(input$bins_input)))
        })
      } else if (viz_type == "TreeMap"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_treemap_2_var(df_cols,type_cols = type_cols,nbins=as.integer(input$bins_input)))
        })
      } else if (viz_type == "Nuage de points"){
        #Y\X --> Y:col2 and X:col1
        output$viz_input = renderUI({
          renderPlot(two_var_scatterplot(df_cols,type_cols = type_cols))
        })
      }

    }


  },ignoreNULL = TRUE,ignoreInit = TRUE)

  #------------------------------------------------------------------------------------------------------------#




  observeEvent(input$input_model_outcome_bin, {
    features=setdiff(colnames(list_reavalues$table),c(input$input_model_outcome_bin))
    if (!length(features)){
      output$input_model_features_bin=renderUI({
      })
    }else{
      output$input_model_features_bin=renderUI({
        selectizeInput("input_model_features_bin",label="Selectionner vos features",choices=features,multiple=TRUE,options = list(maxItems = length(features)),selected =features[1])
      })
    }

    output$input_valid_model_bin=renderUI({
      actionButton("input_valid_model_bin",label="Valider les options")
    })
    modality_outcome=unique(list_reavalues$table[input$input_model_outcome_bin])
    output$input_model_poschoice_bin=renderUI({
      selectInput("input_model_poschoice_bin",label="Choix de la modalité positive",choices=modality_outcome,selected =modality_outcome[1])
    })

  },ignoreNULL = TRUE,ignoreInit = TRUE)




  observeEvent(input$input_valid_model_bin,{
    outcome=as.character(input$input_model_outcome_bin)
    mod_positive=as.character(input$input_model_poschoice_bin)
    features=input$input_model_features_bin
    df=list_reavalues$table[,c(outcome,features)]
    outcome_model=df[outcome]==mod_positive
    df[,outcome]=outcome_model
    col_categories=list_reavalues$col_categories[names(list_reavalues$col_categories)
                                                   %in% c(outcome,features)][c(outcome,features)]
    prop=as.numeric(input$input_proportion_bin)
    threshold=as.numeric(input$input_threshold_bin)
    model=NULL
    model_name=NULL
    true_labels_test=NULL
    pred_prob_test=NULL
    if (input$input_model_type_bin == "Arbre de décision CART"){
      model_name="Arbre de décision CART"
      prune=input$input_model_pruned_bin
      model_results=decision_tree_model(df[,c(outcome,features)],outcome,col_categories = col_categories,method="CART",prop=prop,prune=prune)
      model=model_results[[1]]
      true_labels_test=as.logical(unlist(model_results[2]))
      pred_prob_test=unlist(model_results[3])
    }
    if (input$input_model_type_bin == "Arbre de décision CHAID"){
      nbins=as.integer(input$input_model_bins_bin)
      model_name="Arbre de décision CHAID"
      model_results=decision_tree_model(df[,c(outcome,features)],outcome,col_categories = col_categories,method="CHAID",prop=prop,nbins=nbins)
      model=model_results[[1]]
      true_labels_test=as.logical(unlist(model_results[2]))
      pred_prob_test=unlist(model_results[3])
    }


    metrics_confusion=calculate_metrics(true_labels_test,pred_prob_test>threshold)
    metrics=as.data.frame(metrics_confusion["metrics"])
    colnames(metrics)=c("Accuracy","Error Rate","Precision","Recall","F1-score")
    output$input_viz_tree_bin = renderUI({
      renderPlot(plot_tree_model(model,model_name))
    })
    output$input_metrics_model_bin = renderUI({
      renderTable(metrics)
    })
    confusion_matrix=metrics_confusion["confusion_matrix"]
    output$input_confusion_model_bin = renderUI({
      renderTable(confusion_matrix,rownames = TRUE,colnames = TRUE)
    })





  },ignoreNULL = TRUE,ignoreInit = TRUE)




  output$output_table = renderDataTable({
    if(!is.null(list_reavalues$table)) DT::datatable(list_reavalues$table,options = list(scrollX=TRUE,scrollY = "200px"))
  })
  output$output_type_table = renderDataTable({
    if(!is.null(list_reavalues$type_table)) DT::datatable(list_reavalues$type_table,options = list(scrollX=TRUE,scrollY = "200px"))
  })


}