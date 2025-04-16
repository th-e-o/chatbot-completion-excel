ui <- fluidPage(
  titlePanel("Application Budgétaire - Chargement et Calculs Automatisés"),
  sidebarLayout(
    sidebarPanel(
      h4("Étape 1 - Paramètres généraux"),
      textInput("annee", "Année :", value = "2025", placeholder = "e.g., 2025"),
      textInput("code_ministere", "Code Ministère :", value = "38", placeholder = "e.g., 38"),
      textInput("code_programme", "Code Programme (3 chiffres) :", value = "123", placeholder = "e.g., 123"),
      hr(),
      h4("Étape 2 - Déposez les fichiers sources"),
      fileInput("ppes_file", "Fichier PP-E-S", accept = c(".xlsx")),
      fileInput("dpp18_file", "Fichier DPP 18", accept = c(".xlsx")),
      fileInput("bud45_file", "Fichier BUD 45", accept = c(".xlsx")),
      hr(),
      h4("Étape 3 - Déposez le fichier final (vierge)"),
      fileInput("final_file", "Fichier Final à Compléter", accept = c(".xlsx")),
      actionButton("process_button", "Générer le fichier complété", class = "btn-primary"),
      downloadButton("download_final", "Télécharger le fichier final", class = "btn-success"),
      hr(),
      textOutput("status")
    ),
    mainPanel(
      h4("Aperçu - PP-E-S"),
      dataTableOutput("table_ppes"),
      h4("Aperçu - DPP 18"),
      dataTableOutput("table_dpp18"),
      h4("Aperçu - BUD 45"),
      dataTableOutput("table_bud45")
    )
  )
)