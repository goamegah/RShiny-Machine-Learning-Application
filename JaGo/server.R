e = new.env()


server = function (input, output, session) {
    donneesTXT <- reactive({
        if (is.null(input$fichierTXT)) return (NULL)
        if (input$fichierSep == 'autre') sep = input$fichierSepAutre
        else sep = input$fichierSep
        don = NULL
        try({
            don = read.table(
                input$fichierTXT$datapath,
                header = input$fichierHeader,
                sep = sep,
                dec = input$fichierDec,
                skip = input$fichierSkipNb,
                stringsAsFactors = FALSE)
        }, silent = TRUE)
        don
    })
    
    observeEvent(input$fichierRDATA, {
        input$fichierRDATA
        
        load(input$fichierRDATA$datapath, envir = e)
        liste = ls(e)
        
        updateSelectInput(session = session, inputId = "choixVar", choices = liste)
    })
    
    donneesRDATA <- reactive({
        don = NULL
        try({
            don = get(input$choixVar, envir = e)
        }, silent = TRUE)
        don
    })
    
    donnees <- reactive({
        don = NULL
        if (input$choixDF == "Fichier texte") {
            # Données à charger
            don = donneesTXT()
        } else {
            if (input$choixDF == "Fichier RData") {
                don = donneesRDATA()
            } else {
                don = get(input$choixDF)
            }
        }
        don
    })
    
    output$table <- DT::renderDataTable({
        donnees()
    })
    output$nbLig <- renderText({ nrow(donnees()) })
    output$nbCol <- renderText({ ncol(donnees()) })
}