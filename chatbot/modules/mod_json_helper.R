library(shiny)
library(jsonlite)

# ===============================
# UI du Module JSON Helper
# ===============================
mod_json_helper_ui <- function(id) {
  ns <- NS(id)  # Création du namespace pour éviter les conflits d'ID dans l'application
  tagList(
    # Composant pour importer un fichier JSON (.json)
    fileInput(ns("import_json"), "📂 Importer un fichier JSON", accept = c(".json")),
    # Bouton pour recharger le JSON (utile si le fichier est modifié ou rechargé)
    actionButton(ns("reload_json"), "🔄 Recharger le JSON"),
    # Zone d'affichage en texte brut pour prévisualiser le contenu du JSON importé
    verbatimTextOutput(ns("json_preview")),
    # Bouton pour afficher les résultats d'analyse (labels et axes) dans une fenêtre modale
    actionButton(ns("show_labels"), "Afficher les résultats d'analyse"), 
    # Bouton pour actualiser les tags
    actionButton(ns("update_tags"), "🛠️ Actualiser les tags à partir des cellules sources")
    )
}

# ===============================
# Server du Module JSON Helper
# ===============================
mod_json_helper_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns  # Création du namespace pour ce module
    
    # Reactive value pour stocker les données JSON importées
    imported_data <- reactiveVal(NULL)
    
    # -----------------------------------------------------------------------------------------
    # Fonction pour importer un fichier JSON en utilisant jsonlite::fromJSON.
    # La fonction tente de lire le fichier et de stocker le résultat dans imported_data.
    # En cas d'erreur, une notification est affichée.
    # -----------------------------------------------------------------------------------------
    load_json_file <- function(file_path) {
      tryCatch({
        json_data <- fromJSON(file_path)
        imported_data(json_data)
        showNotification("✅ JSON importé avec succès !", type = "message")
      }, error = function(e) {
        showNotification(paste("❌ Erreur lors de l'importation :", e$message), type = "error")
      })
    }
    
    # -----------------------------------------------------------------------------------------
    # Observation de l'importation d'un fichier JSON via le composant fileInput.
    # Lorsque l'utilisateur charge un fichier, on appelle load_json_file et on stocke
    # le résultat dans rv$imported_json pour usage ultérieur dans l'application.
    # -----------------------------------------------------------------------------------------
    observeEvent(input$import_json, {
      req(input$import_json)
      load_json_file(input$import_json$datapath)
      rv$imported_json <- imported_data()
    })
    
    # -----------------------------------------------------------------------------------------
    # Bouton de rechargement du JSON : permet de recharger le même fichier JSON.
    # Cela peut être utile si l'utilisateur souhaite actualiser les données importées.
    # -----------------------------------------------------------------------------------------
    observeEvent(input$reload_json, {
      req(imported_data())
      load_json_file(input$import_json$datapath)
      rv$imported_json <- imported_data()
    })
    
    # -----------------------------------------------------------------------------------------
    # Prévisualisation du JSON importé : affiche le contenu de imported_data dans une zone
    # en texte brut afin que l'utilisateur puisse vérifier le contenu importé.
    # -----------------------------------------------------------------------------------------
    output$json_preview <- renderPrint({
      req(imported_data())
      imported_data()
    })
    
    # -----------------------------------------------------------------------------------------
    # Observer sans condition : met à jour en continu rv$imported_json avec la valeur de imported_data.
    # Cela permet de s'assurer que rv$imported_json reste synchronisé avec le JSON importé.
    # -----------------------------------------------------------------------------------------
    observe({
      rv$imported_json <- imported_data()
    })
    
    # -----------------------------------------------------------------------------------------
    # Extraction des labels à partir du JSON importé.
    # On s'assure ici que le JSON contient bien une clé 'tags'. Si ce n'est pas le cas,
    # on notifie l'utilisateur.
    # Ensuite, on extrait les labels :
    #  - Si 'tags' est un data.frame, on extrait la colonne 'labels'.
    #  - Si 'tags' est une liste, on extrait les labels en les unissant.
    # Les labels extraits sont stockés dans rv$extracted_labels.
    # -----------------------------------------------------------------------------------------
    observe({
      req(imported_data())
      json_data <- imported_data()
      
      if (!is.list(json_data) || !("tags" %in% names(json_data))) {
        showNotification("⚠️ Le fichier JSON n'est pas structuré comme prévu (clé 'tags' manquante).", type = "error")
        return(NULL)
      }
      
      tags_list <- json_data$tags
      if (is.data.frame(tags_list)) {
        labels <- unique(na.omit(tags_list$labels))
      } else if (is.list(tags_list)) {
        labels <- unique(na.omit(unlist(lapply(tags_list, function(tag) tag$labels))))
      } else {
        labels <- character(0)
      }
      rv$extracted_labels <- labels
      
      showNotification("✅ Labels extraits du JSON avec succès !", type = "message")
    })
    
    # -----------------------------------------------------------------------------------------
    # Bouton "Afficher les résultats d'analyse" :
    # Lorsque l'utilisateur clique sur ce bouton, une fenêtre modale est affichée.
    # Le modal présente deux tableaux : l'un pour les labels extraits et l'autre pour les axes.
    # -----------------------------------------------------------------------------------------
    observeEvent(input$show_labels, {
      # Préparation du tableau des labels extraits
      labels <- rv$extracted_labels
      if (is.null(labels) || length(labels) == 0) {
        labels_table <- "Aucun label n'a été extrait."
      } else {
        labels_table <- tags$table(
          class = "table table-striped",
          tags$thead(
            tags$tr(tags$th("Label"))
          ),
          tags$tbody(
            lapply(labels, function(label) {
              tags$tr(
                tags$td(paste(as.character(label), collapse = ", "))
              )
            })
          )
        )
      }
      
      # Préparation du tableau des axes d'analyse
      if (!is.null(rv$axes)) {
        if (is.data.frame(rv$axes)) {
          axes_rows <- lapply(seq_len(nrow(rv$axes)), function(i) {
            tags$tr(
              tags$td(as.character(rv$axes$axe[i])),
              tags$td(as.character(rv$axes$description[i]))
            )
          })
          axes_table <- tags$table(
            class = "table table-striped",
            tags$thead(
              tags$tr(
                tags$th("Axe"),
                tags$th("Description")
              )
            ),
            tags$tbody(axes_rows)
          )
        } else if (is.list(rv$axes)) {
          axes_rows <- lapply(rv$axes, function(x) {
            axe_text <- if (!is.null(x$axe)) as.character(x$axe) else ""
            desc_text <- if (!is.null(x$description)) as.character(x$description) else ""
            tags$tr(
              tags$td(axe_text),
              tags$td(desc_text)
            )
          })
          axes_table <- tags$table(
            class = "table table-striped",
            tags$thead(
              tags$tr(
                tags$th("Axe"),
                tags$th("Description")
              )
            ),
            tags$tbody(axes_rows)
          )
        } else {
          axes_table <- "Aucun axe n'a été extrait."
        }
      } else {
        axes_table <- "Aucun axe n'a été extrait."
      }
      
      # Contenu complet du modal : deux sections distinctes pour les labels et les axes
      modal_content <- tagList(
        h3("Labels extraits"),
        labels_table,
        br(),
        h3("Axes d'analyse"),
        axes_table
      )
      
      # Affichage du modal avec le contenu préparé
      showModal(modalDialog(
        title = "Résultats de l'analyse",
        modal_content,
        easyClose = TRUE,
        footer = modalButton("Fermer")
      ))
      
      observeEvent(input$update_tags, {
        print("il y a les req ?")
        req(imported_data(), rv$imported_json, rv$excel_data)
        print("je rentre")
        json_data <- imported_data()
        excel_data <- rv$excel_data
        
        if (!"tags" %in% names(json_data)) {
          showNotification("⚠️ Clé 'tags' absente dans le JSON.", type = "error")
          return()
        }
        
        if (!is.data.frame(excel_data)) {
          showNotification("⚠️ Le contenu Excel est invalide.", type = "error")
          return()
        }
        
        # Assure-toi que les colonnes de excel_data sont nommées A, B, C, ...
        get_excel_col_letters <- function(df) {
          colnames(df) <- LETTERS[seq_len(ncol(df))]
          df
        }
        excel_df <- get_excel_col_letters(excel_data)
        print("je lance l'update")
        # Mise à jour en ajoutant les labels des source_cells
        updated_tags <- lapply(json_data$tags, function(tag) {
          existing_labels <- tag$labels
          source_labels <- c()
          
          if (!is.null(tag$source_cells)) {
            source_labels <- sapply(tag$source_cells, function(cell_address) {
              if (grepl("^[A-Z]+[0-9]+$", cell_address)) {
                col_letter <- gsub("[0-9]", "", cell_address)
                row_num <- as.integer(gsub("[A-Z]", "", cell_address))
                
                col_index <- match(col_letter, colnames(excel_df))
                if (!is.na(col_index) && row_num <= nrow(excel_df)) {
                  return(as.character(excel_df[[col_index]][row_num]))
                }
              }
              return(NA)
            })
          }
          
          # Ajout des nouveaux labels en évitant les doublons
          tag$labels <- unique(c(existing_labels, na.omit(source_labels)))
          tag
        })
        print("je suis à 252")
        json_data$tags <- updated_tags
        imported_data(json_data)
        rv$imported_json <- json_data
        
        # Résumé des ajouts de labels
        ajouts <- list()
        
        for (i in seq_along(json_data$tags)) {
          old_labels <- imported_data()$tags[[i]]$labels
          new_labels <- json_data$tags[[i]]$labels
          diff <- setdiff(new_labels, old_labels)
          
          if (length(diff) > 0) {
            ajouts[[length(ajouts) + 1]] <- list(
              cell = json_data$tags[[i]]$cell_address,
              added = diff
            )
          }
        }

        showNotification("✅ Tags enrichis avec les labels Excel (sans écraser les existants) !", type = "message")
      })
      
      # Affichage d’un résumé visuel si des ajouts existent
      if (length(ajouts) > 0) {
        resume_lignes <- lapply(ajouts, function(a) {
          tags$p(HTML(paste0("📌 <b>", a$cell, "</b> : + ", paste(shQuote(a$added), collapse = ", "))))
        })
        
        showModal(modalDialog(
          title = "🆕 Labels ajoutés aux tags",
          tagList(
            tags$p(paste0("✅ ", sum(sapply(ajouts, function(a) length(a$added))),
                          " nouveau(x) label(s) ajouté(s) dans ",
                          length(ajouts), " cellule(s).")),
            tags$hr(),
            resume_lignes
          ),
          easyClose = TRUE,
          footer = modalButton("Fermer")
        ))
      } else {
        showNotification("Aucun nouveau label n’a été ajouté.", type = "message")
      }
      
    })
    
  })  # Fin du moduleServer pour mod_json_helper_server
}
