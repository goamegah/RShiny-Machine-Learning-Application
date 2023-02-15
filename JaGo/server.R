if(! require("comprehenr")) install.packages("comprehenr")
library(comprehenr)

server = function (input, output, session) {
  source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/dataset/globals_dataset.R")
  source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/dataset/processing.R")
  source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/exploration/globals_exploration.R")
  source("/home/khaldi/Documents/EDA_ML_RShiny/JaGo/back/src/exploration/vizualizations.R")
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
            selectInput("input_type_varchoice",label="Choisissez la catégorie de la variable",choices=c("quantitative discrète","quantitative continue","qualitative ordinale","qualitative nominale"))
          })
          output$input_valid_type=renderUI({
            actionButton("input_valid_type",label="Valider la catégorie de la variable")
          })
          output$input_process=renderUI({
            actionButton("input_process",label="Faire le processing du DataFrame final")
          })

          #-------------------------------------------------------------------------------------------#

          output$input_exp_type=renderUI({
            selectInput("input_exp_type",
                        label="Choisissez le type d'exploration",
                        choices=c("Univarié","Bivarié"),
                        selected = "Univarié"
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
      output$input_uni_select_var=renderUI({
        selectInput("input_uni_select_var",label="Selectionner votre variable",choices=input$input_varschoice,selected = input$input_varschoice[1])
      })
      output$input_bi_select_var=renderUI({
        selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=input$input_varschoice,multiple=TRUE,options = list(maxItems = 2),selected =input$input_varschoice[1])
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
        selectInput("input_uni_select_var",label="Selectionner votre variable",choices=col_names)
      })
      output$input_bi_select_var=renderUI({
        selectizeInput("input_bi_select_var",label="Selectionner vos deux variables (x resp y)",choices=col_names,multiple=TRUE,options = list(maxItems = 2),selected =col_names[1])
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
      print(input$input_exp_type)
      if (input$input_exp_type == "Univarié"){
        if (col_select == input$input_uni_select_var){
          choices=get_vizualizations(col_category)
          selected=get_vizualization(col_category,1)
          output$input_uni_select_viz=renderUI({
            selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
          })
        }
      }
    }

  },ignoreNULL = TRUE,ignoreInit = TRUE)



  observeEvent(input$input_uni_select_var, {
    if (input$input_exp_type == "Univarié"){
      type_col=get_type_col_by_name(list_reavalues$type_table,input$input_uni_select_var)
      choices=get_vizualizations(type_col)
      selected=choices[1]
      output$input_uni_select_viz=renderUI({
        selectInput("input_uni_select_viz",label="Selectionner votre visualisation",choices=choices,selected=selected)
      })
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















  observeEvent(input$input_valid_exp,{
    if (input$input_exp_type == "Univarié") {
      df_col = list_reavalues$table[input$input_uni_select_var]
      viz_type = input$input_uni_select_viz
      print(viz_type)
      print(input$input_uni_select_var)
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
          renderTable(create_freq_table_continous(df_col))
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
          renderPlot({ create_histogram_continous(df_col)})
        })
      } else if (viz_type == "Tableau des Effectifs/Fréquence par classe") {
        output$viz_input = renderUI({
          renderTable(create_freq_table_continous(df_col))
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



  },ignoreNULL = TRUE,ignoreInit = TRUE)
  
  
  output$output_table = renderDataTable({
    if(!is.null(list_reavalues$table)) DT::datatable(list_reavalues$table,options = list(scrollX=TRUE,scrollY = "200px"))
  })
  output$output_type_table = renderDataTable({
    if(!is.null(list_reavalues$type_table)) DT::datatable(list_reavalues$type_table,options = list(scrollX=TRUE,scrollY = "200px"))
  })


}