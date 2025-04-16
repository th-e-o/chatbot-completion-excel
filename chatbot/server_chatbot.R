server <- function(input, output, session) {
  
  # ------------------------
  # INITIALISATION DES RÉACTIFS
  # ------------------------
  
  # Historique de la conversation sous forme de liste réactive
  chat_history <- reactiveVal(list())
  
  # Stocke le contenu du dernier fichier chargé (pour traitement de fichiers)
  dernier_fichier_contenu <- reactiveVal(NULL)
  
  # Variable pour suivre le processus d'un outil particulier (si besoin)
  outil_process <- reactiveVal(NULL)
  
  # Indicateur indiquant si le prompt relatif à BPSS est actif
  bpss_prompt_active <- reactiveVal(FALSE)
  
  # Indicateur pour afficher un message de saisie (typing), par exemple pendant le traitement
  typing <- reactiveVal(FALSE)
  
  # Liste réactive pour stocker diverses informations (JSON importé, labels extraits, axes, contexte, etc.)
  rv <- reactiveValues(
    imported_json = NULL,
    extracted_labels = NULL,
    axes = NULL,
    context = NULL,
    chat_history = list()
  )
  
  # Data frame réactif pour stocker les données budgétaires extraites et éventuellement modifiées
  donnees_extraites <- reactiveVal(data.frame(
    Axe = character(),
    Description = character(),
    Montant = numeric(),
    Unité = character(),
    Probabilite = numeric(),
    Feuille_excel = character(),
    SourcePhrase = character(),
    CelluleCible = character(),  # Correspond à la cellule du classeur Excel pour le mapping
    stringsAsFactors = FALSE
  ))
  
  # ------------------------
  # APPELS AUX MODULES EXISTANTS
  # ------------------------
  
  # Module d'analyse des mesures/catégories budgétaires.
  # Lorsqu'une analyse est terminée, le résumé est ajouté au chat.
  mod_mesures_cat_server("cat1", rv, on_analysis_summary = function(summary) {
    messages <- chat_history()
    messages <- append(messages, list(list(role = "assistant", content = summary)))
    chat_history(messages)
    # Envoie une commande JavaScript pour faire défiler vers le bas de la conversation
    session$sendCustomMessage(type = 'scrollToBottom', message = list())
  })
  
  # Module pour l'outil BPSS (Excel budgétaire)
  mod_outil_bpss_server("bpss1")
  
  # Module d'aide au traitement JSON (extraction, parsing, etc.)
  mod_json_helper_server("json_helper", rv)
  
  # Appel du helper regroupant la logique d'extraction budgétaire.
  # Ce helper, défini dans helper_budget_extraction.R, orchestre l'appel à l'API LLM, le mapping automatique, etc.
  setupBudgetExtraction(input, output, session, rv, chat_history, dernier_fichier_contenu, donnees_extraites)
  
  # ------------------------
  # GESTION DES MESSAGES UTILISATEUR
  # ------------------------
  
  # Lors du clic sur le bouton "Envoyer", la fonction handle_user_input est appelée.
  # Cette fonction traite le message de l'utilisateur, le stocke dans l'historique, etc.
  observeEvent(input$send_btn, {
    handle_user_input(input, session, chat_history, typing, bpss_prompt_active)
  })
  

  # ------------------------
  # GESTION DU CHARGEMENT DE FICHIER
  # ------------------------
  
  # Lorsqu'un fichier est chargé via l'input file, la fonction handle_file_message est appelée
  # pour traiter le fichier (par exemple, extraire le contenu) et mettre à jour l'historique.
  observeEvent(input$file_input, {
    req(input$file_input)
    handle_file_message(input$file_input, chat_history, dernier_fichier_contenu, session, typing)
  })
  
  # ------------------------
  # AFFICHAGE INTERACTIF DU TABLEAU BUDGÉTAIRE
  # ------------------------
  
  # Rendu du tableau éditable contenant les données budgétaires extraites.
  output$budget_table <- renderDT({
    datatable(
      donnees_extraites(),
      options = list(scrollX = TRUE),
      editable = TRUE
    )
  })
  
  # Observer les modifications effectuées par l'utilisateur sur le tableau,
  # et mettre à jour le data frame réactif correspondante.
  observeEvent(input$budget_table_cell_edit, {
    info <- input$budget_table_cell_edit
    df <- donnees_extraites()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    donnees_extraites(df)
  })
  
  # Lorsqu'une modification est validée (bouton "Sauver"), le modal est retiré.
  observeEvent(input$save_budget_changes, {
    removeModal()
  })
  
  # ------------------------
  # GESTION DE L'OUTIL BPSS
  # ------------------------
  
  # Affichage du modal pour l'outil BPSS lorsque l'utilisateur clique sur le bouton dédié.
  observeEvent(input$toggle_bpss_ui, {
    showModal(modalDialog(
      title = "🛠️ Outil BPSS",
      size = "l",
      easyClose = TRUE,
      footer = modalButton("Fermer"),
      mod_outil_bpss_ui("bpss1")  # Interface utilisateur du module BPSS
    ))
  })
  
  # Déclenchement de BPSS via un message JavaScript (pour forcer l'affichage du modal)
  observeEvent(input$lancer_outil_bpss, {
    session$sendCustomMessage(type = "jsCode", message = list(code = "$('#toggle_bpss_ui').click();"))
  })
  
  # ------------------------
  # GESTION DE L'IMPORTATION JSON
  # ------------------------
  
  # Lorsqu'un fichier JSON est importé, mettre à jour rv$imported_json et notifier l'utilisateur.
  observeEvent(rv$imported_json, {
    req(rv$imported_json)
    showNotification("📂 JSON importé avec succès !", type = "message")
    
    budgibot_message <- "✅ J'ai bien reçu un fichier JSON décrivant la structure de votre classeur Excel."
    messages <- chat_history()
    messages <- append(messages, list(list(role = "assistant", content = budgibot_message)))
    chat_history(messages)
    
    session$sendCustomMessage(type = 'scrollToBottom', message = list())
  })
  
  # ------------------------
  # ANALYSE DES LABELS ET MISE À JOUR DES AXES
  # ------------------------
  
  # Lorsque rv$extracted_labels change (labels extraits depuis un JSON ou autre),
  # analyser ces labels via une fonction d'analyse (analyze_labels) pour déterminer les axes et le contexte.
  observeEvent(rv$extracted_labels, {
    req(rv$extracted_labels)
    print("🔍 Labels extraits :")
    print(rv$extracted_labels)
    
    # Transformation des labels en une chaîne simple
    labels_flat <- sapply(rv$extracted_labels, function(x) {
      if (is.character(x)) {
        paste(x, collapse = " ")
      } else {
        as.character(x)
      }
    })
    labels_text <- paste(unique(labels_flat), collapse = ", ")
    print(paste("✅ Labels détectés :", labels_text))
    
    # Appel à l'analyse des labels par le LLM
    analysis_result <- analyze_labels(labels_flat)
    
    # Vérification du résultat de l'analyse
    if (is.null(analysis_result) || !is.list(analysis_result)) {
      showNotification("❌ L'analyse des labels par le LLM a échoué.", type = "error")
      return(NULL)
    }
    
    if (!all(c("axes", "contexte_general") %in% names(analysis_result))) {
      showNotification("❌ La réponse du LLM ne contient pas les clés 'axes' et 'contexte_general'.", type = "error")
      return(NULL)
    }
    
    # Mise à jour des axes et du contexte dans rv
    rv$axes <- analysis_result$axes
    rv$context <- analysis_result$contexte_general
    
    # Construction d'un message récapitulatif pour l'utilisateur à partir des axes détectés
    axes_info <- ""
    if (is.data.frame(rv$axes)) {
      if (!is.null(rv$axes$axe) && !is.null(rv$axes$description)) {
        axes_info <- paste("- ", rv$axes$axe, ": ", rv$axes$description, sep = "", collapse = "\n")
      }
    } else if (is.list(rv$axes)) {
      axes_info <- paste(sapply(rv$axes, function(x) {
        if (!is.null(x$axe) && !is.null(x$description)) {
          paste0("- ", x$axe, ": ", x$description)
        } else {
          ""
        }
      }), collapse = "\n")
    }
    
    context_text <- if (is.null(rv$context)) "" else as.character(rv$context)
    
    budgibot_message <- paste(
      "✅ J'ai analysé les labels et identifié les axes suivants :\n", 
      axes_info, 
      "\n\nContexte détecté :\n", 
      context_text, 
      "\n\nSouhaitez-vous que j'analyse un texte budgétaire en fonction de ces axes ?",
      sep = ""
    )
    
    # Mise à jour de l'historique avec le message récapitulatif
    messages <- chat_history()
    messages <- append(messages, list(list(role = "assistant", content = budgibot_message)))
    chat_history(messages)
    
    session$sendCustomMessage(type = 'scrollToBottom', message = list())
  })
  
  # ------------------------
  # TÉLÉCHARGEMENT DE L'HISTORIQUE DU CHAT
  # ------------------------
  
  output$download_chat <- downloadHandler(
    filename = function() {
      paste("chat_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      messages <- chat_history()
      writeLines(sapply(messages, function(m) paste0(m$role, ": ", m$content)), con = file)
    }
  )
  
  # ------------------------
  # RENDU DE L'HISTORIQUE DE CONVERSATION
  # ------------------------
  
  output$chat_history <- renderUI({
    messages <- chat_history()
    if (length(messages) == 0) return(NULL)
    
    # Parcourt chaque message et construit son rendu HTML
    rendered_messages <- lapply(seq_along(messages), function(i) {
      msg <- messages[[i]]
      
      # Ignorer certains types de messages (par exemple le contenu des fichiers)
      if (!is.null(msg$meta) && msg$meta == "fichier_content") return(NULL)
      
      # Cas particulier pour le prompt BPSS
      if (!is.null(msg$type) && msg$type == "bpss_prompt") {
        return(
          div(
            class = "chat-bubble-container bot-message-container",
            div(class = "chat-sender", "BudgiBot"),
            div(class = "chat-message bot-message", "Souhaitez-vous lancer l'outil BPSS ?"),
            div(class = "quick-replies",
                actionButton("lancer_outil_bpss", "🛠️ Lancer l’outil Excel BPSS", class = "btn btn-success")
            )
          )
        )
      }
      
      # Rendu pour un message de l'assistant
      if (msg$role == "assistant") {
        return(
          div(
            class = "chat-bubble-container bot-message-container",
            div(class = "chat-sender", "BudgiBot"),
            div(class = "chat-message bot-message", HTML(msg$content)),
            div(class = "quick-replies",
                actionButton(paste0("btn_detail_", i), "Peux-tu détailler ?",
                             onclick = "Shiny.setInputValue('user_input', 'Peux-tu détailler ?'); $('#send_btn').click();"),
                actionButton(paste0("btn_example_", i), "Donne-moi un exemple",
                             onclick = "Shiny.setInputValue('user_input', 'Donne-moi un exemple'); $('#send_btn').click();"),
                actionButton(paste0("btn_resume_", i), "Résume",
                             onclick = "Shiny.setInputValue('user_input', 'Résume'); $('#send_btn').click();"),
                actionButton(paste0("btn_extract_budget_", i),
                             "Extrait les données budgétaires",
                             onclick = paste0("Shiny.setInputValue('extract_budget_under_bot_clicked', ", i, ", {priority: 'event'});"))
            )
          )
        )
      }
      
      # Rendu pour un message de l'utilisateur
      if (msg$role == "user") {
        return(
          div(
            class = "chat-bubble-container user-message-container",
            div(class = "chat-sender", "Utilisateur"),
            div(class = "chat-message user-message", msg$content)
          )
        )
      }
      
      NULL
    })
    
    # Retourne l'ensemble des messages filtrés et rendus en HTML
    tagList(Filter(Negate(is.null), rendered_messages))
  })
}

# Lancement de l'application Shiny
shinyApp(ui = ui, server = server)
