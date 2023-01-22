ui = dashboardPage(
    
    # App header 
    dashboardHeader(
        title = "JaGo"
    ),
    
    # App sidebar
    dashboardSidebar(
        
        # sidebar menu
        sidebarMenu(
            id = "gauche",
            menuItem(text = " Dataset ", tabName = "sb_data", icon = icon("database")),
            
            # Si on clique sur le boutton dataset
            conditionalPanel(
                condition = "input.gauche == 'sb_data'", 
                selectInput(
                    inputId = "choixDF",
                    label = "Choix des données",
                    choices = c("mtcars", "iris", "LifeCycleSavings", "Fichier texte", "Fichier RData")
                ),
                # Si en plus de cela on choisit un fichier texte
                conditionalPanel(
                    condition = "input.choixDF == 'Fichier texte'",
                    
                    # choix
                    fileInput(
                        inputId = "fichierTXT", 
                        label = "Fichier à importer", 
                        placeholder = "Choississez votre fichier texte"),
                    
                    # entêtes
                    checkboxInput(
                        inputId = "fichierHeader", 
                        label = "Noms de variables présents",
                        value = FALSE),
                    
                    radioButtons(
                        inputId = "fichierSep",
                        label = "Séparateur", 
                        choices = c("point-virgule" = ";", "virgule" = ",", "espace" = " ", "tabulation" = "\t", "autre" = "autre")),
                    
                    conditionalPanel(
                        condition = "input.fichierSep == 'autre'",
                        textInput(
                            inputId = "fichierSepAutre",
                            label = "Séparateur")
                    ),
                    
                    radioButtons(
                        inputId = "fichierDec", 
                        label = "Séparateur de décimales",
                        choices = c("point" = ".", "virgule" = ",")
                    ),
                    
                    checkboxInput(
                        inputId = "fichierSkip", 
                        label = "Ignorer les premières lignes ?"
                    ),
                    
                    conditionalPanel(
                        condition = "input.fichierSkip",
                        textInput(
                            inputId = "fichierSkipNb", 
                            label = "Nombre de lignes", value = 0)
                    )
                ),
                
                conditionalPanel(
                    condition = "input.choixDF == 'Fichier RData'",
                    
                    fileInput(
                        inputId = "fichierRDATA", 
                        label = "Fichier à importer", 
                        placeholder = "Choississez votre fichier RData"
                    ),
                    
                    selectInput(
                        inputId = "choixVar", 
                        label = "Quelle variable prendre ?", 
                        choices = NULL
                    )
                )
            ),
            
            # ... A suivre 
            menuItem(text = " Analyse Exploratoire", tabName = "sb_anal", icon=icon("chart-line")),
            menuItem(text = " Machine Learning ", tabName = "sb_machine", icon = icon("database"))
        )
    ),
    
    # App Body 
    dashboardBody(
        valueBox(textOutput("nbLig"), subtitle = "Lignes", width = 6, icon = icon("grip-lines")),
        valueBox(textOutput("nbCol"), subtitle = "Colonnes", width = 6, icon = icon("grip-lines-vertical")),
        DT::dataTableOutput("table")
    ),
    title = "Correction"
)
