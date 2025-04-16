# helper_budget_extraction.R

# Cette fonction met en place toute la logique d'extraction des données budgétaires
# dans l'application. Elle observe les actions de l'utilisateur (comme le clic sur le
# bouton d'extraction) et orchestre l'appel à l'API LLM via la fonction get_budget_data,
# ainsi que le traitement et le mapping automatique des données extraites.
setupBudgetExtraction <- function(input, output, session, rv, chat_history, dernier_fichier_contenu, donnees_extraites) {
  
  # --------------------------------------------------------
  # Observation du clic sur le bouton d'extraction budgétaire
  # --------------------------------------------------------
  observeEvent(input$extract_budget_under_bot_clicked, {
    # Récupère l'indice du message utilisateur sur lequel l'extraction doit être effectuée
    index_of_bot <- input$extract_budget_under_bot_clicked
    
    # Récupère le message utilisateur correspondant à l'index fourni
    # get_user_message_for_extraction doit extraire le texte pertinent de l'historique ou du fichier chargé
    user_message <- get_user_message_for_extraction(index_of_bot, chat_history, dernier_fichier_contenu)
    
    # Si aucun message n'est trouvé, affiche une notification d'erreur et arrête le processus
    if (is.null(user_message)) {
      showNotification("Le contenu du message est introuvable.", type = "error")
      return(NULL)
    }
    
    # Notification indiquant le démarrage de la recherche des données budgétaires
    showNotification("Recherche de données budgétaires en cours...", type = "message")
    
    # --------------------------------------------------------
    # Appel asynchrone à la fonction get_budget_data pour extraire les données
    # --------------------------------------------------------
    # On passe le message utilisateur et les axes extraits (isolation pour éviter la réactivité intempestive)
    future({ 
      get_budget_data(user_message, axes = isolate(rv$axes))
    }) %...>% (function(budget_data) {
      
      # Si aucune donnée n'est détectée ou si le data frame retourné est vide, afficher une notification d'avertissement
      if (is.null(budget_data) || nrow(budget_data) == 0) {
        showNotification("Aucune donnée détectée.", type = "warning")
        return(NULL)
      }
      
      # Notification indiquant que la recherche des passages sources va débuter
      showNotification("Recherche des passages sources...", type = "message")
      
      # --------------------------------------------------------
      # Mapping automatique des données budgétaires extraites
      # --------------------------------------------------------
      # Ajout d'une colonne 'CelluleCible' initialisée à NA (inconnu par défaut)
      budget_data$CelluleCible <- NA
      # Si un JSON contenant des tags a été importé (rv$imported_json), on tente de faire le mapping
      if (!is.null(rv$imported_json) && "tags" %in% names(rv$imported_json)) {
        tags_data <- rv$imported_json$tags
        # Parcourt chaque ligne du data frame budget_data
        for (i in 1:nrow(budget_data)) {
          # Pour chaque ligne, recherche dans les tags un label correspondant à l'axe du budget
          matching_tags <- sapply(tags_data, function(tag) {
            if (is.character(tag$labels)) {
              # Comparaison en minuscule pour homogénéiser les valeurs
              tolower(tag$labels) == tolower(budget_data$Axe[i])
            } else if (is.list(tag$labels)) {
              tolower(paste(tag$labels, collapse = ", ")) == tolower(budget_data$Axe[i])
            } else {
              FALSE
            }
          })
          # Si au moins un tag correspond, on prend le premier match et on récupère son adresse de cellule
          if (any(matching_tags)) {
            index_tag <- which(matching_tags)[1]
            budget_data$CelluleCible[i] <- tags_data[[index_tag]]$cell_address
          }
        }
      }
      
      # Stocke les données budgétaires extraites (et mappées) dans le réactif donnees_extraites
      current_data <- donnees_extraites()
      donnees_extraites(rbind(current_data, budget_data))
      
      # Affiche une fenêtre modale contenant un tableau interactif pour la validation/correction du mapping
      showModal(modalDialog(
        title = "Validation du mapping des cellules",
        DT::DTOutput(session$ns("budget_table_mapping")),  # Affichage du tableau via DT
        footer = tagList(
          modalButton("Annuler"),
          actionButton(session$ns("save_mapping"), "Valider le mapping")
        ),
        size = "l",
        easyClose = TRUE
      ))
      
    }) %...!% (function(err) {
      # En cas d'erreur lors de l'appel asynchrone, affiche une notification d'erreur avec le message retourné
      showNotification(paste("Erreur durant l'extraction :", err$message), type = "error")
    })
  })
  
  # --------------------------------------------------------
  # Rendu du tableau interactif de mapping via DT dans le modal
  # --------------------------------------------------------
  output$budget_table_mapping <- DT::renderDT({
    req(donnees_extraites())
    DT::datatable(
      donnees_extraites(),
      options = list(scrollX = TRUE),  # Activation du scroll horizontal pour les tableaux larges
      editable = list(target = "cell", disable = list(columns = c(1:6))) 
      # Les colonnes 1 à 6 sont désactivées de l'édition : adaptez cette configuration selon la structure du data frame
    )
  })
  
  # --------------------------------------------------------
  # Observation des modifications sur le tableau interactif
  # --------------------------------------------------------
  # Lorsqu'une cellule du tableau est éditée, mettre à jour le data frame réactif
  observeEvent(input$budget_table_mapping_cell_edit, {
    info <- input$budget_table_mapping_cell_edit
    df <- donnees_extraites()
    df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
    donnees_extraites(df)
  })
}
