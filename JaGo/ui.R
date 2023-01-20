ui = dashboardPage(
    dashboardHeader(
        title = "Correction"
    ),
    dashboardSidebar(
        selectInput(
            "choixDF",
            label = "Choix des données",
            choices = c("mtcars", "iris", "LifeCycleSavings", "Fichier texte", "Fichier RData")
        ),
        conditionalPanel(
            condition = "input.choixDF == 'Fichier texte'",
            fileInput("fichierTXT", label = "Fichier à importer", placeholder = "Choississez votre fichier texte"),
            checkboxInput("fichierHeader", label = "Noms de variables présents", value = FALSE),
            radioButtons("fichierSep", label = "Séparateur", 
                         c("point-virgule" = ";", "virgule" = ",", "espace" = " ", "tabulation" = "\t", "autre" = "autre")),
            conditionalPanel(
                condition = "input.fichierSep == 'autre'",
                textInput("fichierSepAutre", label = "Séparateur")
            ),
            radioButtons("fichierDec", label = "Séparateur de décimales",
                         c("point" = ".", "virgule" = ",")),
            checkboxInput("fichierSkip", label = "Ignorer les premières lignes ?"),
            conditionalPanel(
                condition = "input.fichierSkip",
                textInput("fichierSkipNb", label = "Nombre de lignes", value = 0)
            )
        ),
        conditionalPanel(
            condition = "input.choixDF == 'Fichier RData'",
            fileInput("fichierRDATA", label = "Fichier à importer", placeholder = "Choississez votre fichier RData"),
            selectInput("choixVar", label = "Quelle variable prendre ?", choices = NULL)
        )
    ),
    dashboardBody(
        valueBox(textOutput("nbLig"), subtitle = "Lignes", width = 6, icon = icon("grip-lines")),
        valueBox(textOutput("nbCol"), subtitle = "Colonnes", width = 6, icon = icon("grip-lines-vertical")),
        DT::dataTableOutput("table")
    ),
    title = "Correction"
)
