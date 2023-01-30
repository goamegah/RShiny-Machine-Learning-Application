
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