library(shiny)
library(openxlsx2)
library(readxl)
library(dplyr)
library(DT)
library(stringr)  # Pour utiliser str_detect

# ----- UI -----
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

# ----- SERVER -----
server <- function(input, output, session) {
  
  # Reactive value to store the final file path
  final_file_path <- reactiveVal(NULL)
  
  # Function to read .xlsx files with error recovery
  read_xlsx_with_recovery <- function(file_path, sheet = NULL) {
    tryCatch({
      # Try reading with readxl
      read_excel(file_path, sheet = sheet)
    }, error = function(e) {
      # If readxl fails, return an empty data frame
      warning("Impossible de lire le fichier .xlsx. Le fichier peut être corrompu ou incompatible.")
      data.frame()
    })
  }
  
  # Observe the process button click
  observeEvent(input$process_button, {
    req(input$ppes_file, input$dpp18_file, input$bud45_file, input$final_file)
    
    output$status <- renderText("Traitement en cours...")
    
    tryCatch({
      # --- Récupération chemins ---
      ppes_path <- input$ppes_file$datapath
      dpp18_path <- input$dpp18_file$datapath
      bud45_path <- input$bud45_file$datapath
      final_template_path <- input$final_file$datapath
      
      code_ministere <- input$code_ministere
      code_programme <- input$code_programme
      
      # --- Feuilles sources ---
      nomFeuilleSource1 <- paste0("MIN_", code_ministere, "_DETAIL_Prog_PP_CATEG")
      nomFeuilleSource2 <- paste0("MIN_", code_ministere, "_DETAIL_Prog_Entrants")
      nomFeuilleSource3 <- paste0("MIN_", code_ministere, "_DETAIL_Prog_Sortants")
      
      # --- Lecture des données avec readxl (read_xlsx_with_recovery déjà fait ça) ---
      df_pp_categ <- read_xlsx_with_recovery(ppes_path, sheet = nomFeuilleSource1)
      df_entrants <- read_xlsx_with_recovery(ppes_path, sheet = nomFeuilleSource2)
      df_sortants <- read_xlsx_with_recovery(ppes_path, sheet = nomFeuilleSource3)
      
      # --- Filtrer sur colonne B (nom_prog) ---
      filtered_pp_categ <- df_pp_categ %>% filter(substr(as.character(nom_prog), 1, 3) == code_programme)
      filtered_entrants <- df_entrants %>% filter(substr(as.character(nom_prog), 1, 3) == code_programme)
      filtered_sortants <- df_sortants %>% filter(substr(as.character(nom_prog), 1, 3) == code_programme)
      
      # --- Limiter les colonnes (A1:AG) = colonnes 1 à 33, A1:AU = colonnes 1 à 47 ---
      filtered_pp_categ <- filtered_pp_categ[, 1:33]
      filtered_entrants <- filtered_entrants[, 1:47]
      filtered_sortants <- filtered_sortants[, 1:47]
      
      # --- Charger le fichier final ---
      wb <- wb_load(final_template_path)
      
      # --- Nettoyer les zones (on remplit directement, sinon ajoute wb_remove_data si tu veux être propre) ---
      # Injecter PP CATEG à partir de C7
      wb <- wb_add_data(wb, sheet = "Données PP-E-S", x = filtered_pp_categ, start_col = 3, start_row = 7)
      # Injecter Entrants à partir de C113
      wb <- wb_add_data(wb, sheet = "Données PP-E-S", x = filtered_entrants, start_col = 3, start_row = 113)
      # Injecter Sortants à partir de C213
      wb <- wb_add_data(wb, sheet = "Données PP-E-S", x = filtered_sortants, start_col = 3, start_row = 213)
      
      # --- Récupération DPP 18 et BUD 45 sur la colonne 1 (comme VBA sur colonne A) ---
      df_dpp18 <- read_xlsx_with_recovery(dpp18_path)
      df_bud45 <- read_xlsx_with_recovery(bud45_path)
      
      filtered_dpp18 <- df_dpp18 %>% filter(str_detect(.[[1]], fixed(code_programme)))
      filtered_bud45 <- df_bud45 %>% filter(str_detect(.[[1]], fixed(code_programme)))
      
      # --- Injecter dans les feuilles correspondantes ---
      wb <- wb_add_data(wb, sheet = "INF DPP 18", x = filtered_dpp18, start_col = 2, start_row = 6)
      wb <- wb_add_data(wb, sheet = "INF BUD 45", x = filtered_bud45, start_col = 2, start_row = 6)
      
      # --- Copier les catégories Indicié dans l'onglet Accueil ---
      df_pp_indicie <- filtered_pp_categ %>% filter(marqueur_masse_indiciaire == "Indicié")
      # Nettoyage
      wb <- wb_add_data(wb, sheet = "Accueil", x = "", dims = paste0("B43:B", 43 + nrow(df_pp_indicie)))
      wb <- wb_add_data(wb, sheet = "Accueil", x = "", dims = paste0("C43:C", 43 + nrow(df_pp_indicie)))
      
      # Injecter les colonnes B (code categ) et F (nom categ)
      wb <- wb_add_data(wb, sheet = "Accueil", x = df_pp_indicie[[2]], start_col = 2, start_row = 43)  # Colonne B
      wb <- wb_add_data(wb, sheet = "Accueil", x = df_pp_indicie[[3]], start_col = 3, start_row = 43)  # Colonne C
      
      # --- Calculs supplémentaires déjà OK ---
      data_socle <- wb_read(wb, sheet = "I - Socle exécution n-1", col_names = FALSE)
      data_hyp <- wb_read(wb, sheet = "III - Hyp. salariales", col_names = FALSE)
      
      # Conversion colonnes numériques
      data_socle[, 3:5] <- lapply(data_socle[, 3:5], as.numeric)
      data_hyp[, 5] <- as.numeric(data_hyp[, 5])
      data_hyp[, 7:10] <- lapply(data_hyp[, 7:10], as.numeric)
      
      val_C67 <- sum(data_socle[c(34, 44, 46, 49), 3], na.rm = TRUE)
      val_C68 <- sum(data_socle[c(35, 45, 47), 3], na.rm = TRUE)
      val_D68 <- sum(data_socle[c(35, 45, 47), 4], na.rm = TRUE)
      val_E68 <- sum(data_socle[c(35, 45, 47), 5], na.rm = TRUE)
      
      val_E40 <- sum(data_hyp[c(113, 114, 115, 116), 5], na.rm = TRUE)
      vals_GHIJ40 <- sapply(7:10, function(col) sum(data_hyp[c(113, 114, 115, 116), col], na.rm = TRUE))
      
      # Injecter dans le fichier final
      wb <- wb_add_data(wb, sheet = "I - Socle exécution n-1", x = val_C67, dims = "C67")
      wb <- wb_add_data(wb, sheet = "I - Socle exécution n-1", x = val_C68, dims = "C68")
      wb <- wb_add_data(wb, sheet = "I - Socle exécution n-1", x = val_D68, dims = "D68")
      wb <- wb_add_data(wb, sheet = "I - Socle exécution n-1", x = val_E68, dims = "E68")
      
      wb <- wb_add_data(wb, sheet = "VI - Facteurs d'évolution MS", x = val_E40, dims = "E40")
      lapply(seq_along(vals_GHIJ40), function(i) {
        wb <<- wb_add_data(wb, sheet = "VI - Facteurs d'évolution MS", x = vals_GHIJ40[i], dims = paste0(LETTERS[7 + i - 1], "40"))
      })
      
      # --- Sauvegarder le fichier ---
      output_file <- tempfile(fileext = ".xlsx")
      wb_save(wb, file = output_file)
      final_file_path(output_file)
      
      output$status <- renderText("✅ Traitement terminé ! Téléchargez le fichier final.")
      
    }, error = function(e) {
      output$status <- renderText(paste("❌ Erreur :", e$message))
    })
  })
  
  
  output$table_ppes <- renderDataTable({
    req(input$ppes_file)
    nomFeuille <- paste0("MIN_", input$code_ministere, "_DETAIL_Prog_PP_CATEG")
    df <- read_xlsx_with_recovery(input$ppes_file$datapath, sheet = nomFeuille)
    df %>% filter(substr(as.character(nom_prog), 1, 3) == input$code_programme)
  }, options = list(pageLength = 2))
  
  output$table_dpp18 <- renderDataTable({
    req(input$dpp18_file)
    df <- read_xlsx_with_recovery(input$dpp18_file$datapath)
    df %>% filter(str_detect(.[[1]], fixed(input$code_programme)))
  }, options = list(pageLength = 2))
  
  output$table_bud45 <- renderDataTable({
    req(input$bud45_file)
    df <- read_xlsx_with_recovery(input$bud45_file$datapath)
    df %>% filter(str_detect(.[[1]], fixed(input$code_programme)))
  }, options = list(pageLength = 2))
  
  
  # Download handler for the final file
  output$download_final <- downloadHandler(
    filename = function() {
      paste0("Fichier_Final_Budget_", Sys.Date(), ".xlsx")
    },
    content = function(file) {
      req(final_file_path())
      file.copy(final_file_path(), file)
    }
  )
}

# ----- Run App -----
shinyApp(ui, server)
