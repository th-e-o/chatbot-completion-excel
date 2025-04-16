library(shiny)
library(DT)

# =============================
# UI du module de mapping
# =============================
mod_budget_mapping_ui <- function(id) {
  # Création du namespace pour ce module afin d'éviter toute collision des IDs
  ns <- NS(id)
  tagList(
    # Affichage du tableau interactif avec DT
    DT::DTOutput(ns("mapping_table")),
    br(),  # Saut de ligne pour espacer les éléments UI
    # Bouton pour valider le mapping effectué sur le tableau
    actionButton(ns("validate_mapping"), "Valider le mapping")
  )
}


# =============================
# Server du module de mapping
# =============================
# Ce module gère l'affichage et la modification en ligne du data frame contenant
# les données budgétaires extraites. Le paramètre 'mapping_data' doit être une reactiveVal ou reactive() 
# contenant le data frame à éditer.
mod_budget_mapping_server <- function(id, mapping_data) {
  moduleServer(id, function(input, output, session) {
    # Création du namespace pour ce module
    ns <- session$ns
    
    # ---------------------
    # Rendu du tableau interactif
    # ---------------------
    # Ce tableau est affiché avec DT et est rendu éditable pour que l'utilisateur puisse modifier
    # directement les cellules du data frame.
    output$mapping_table <- DT::renderDT({
      # On attend que 'mapping_data()' soit disponible
      req(mapping_data())
      DT::datatable(
        mapping_data(),      # Data frame à afficher
        options = list(scrollX = TRUE), # Active le défilement horizontal pour gérer un grand nombre de colonnes
        editable = TRUE       # Autorise l'édition inline de toutes les cellules
      )
    })
    
    # ---------------------
    # Gestion de l'édition du tableau
    # ---------------------
    # Cet observateur détecte toute modification effectuée sur le tableau interactif.
    observeEvent(input$mapping_table_cell_edit, {
      info <- input$mapping_table_cell_edit  # Contient les informations de la modification (ligne, colonne, nouvelle valeur)
      df <- mapping_data()                   # Récupération actuelle du data frame
      # Mise à jour de la valeur modifiée dans le data frame
      df[info$row, info$col] <- DT::coerceValue(info$value, df[info$row, info$col])
      mapping_data(df)  # Stockage de la nouvelle version du data frame dans la reactive
    })
    
    # ---------------------
    # Validation du mapping
    # ---------------------
    # Lors du clic sur le bouton "Valider le mapping",
    # cet observateur déclenche l'action de validation.
    observeEvent(input$validate_mapping, {
      final_mapping <- mapping_data()  # Récupération des données finales après modifications
      # Notification à l'utilisateur indiquant que le mapping est validé
      showNotification("Mapping validé. Le classeur sera mis à jour.", type = "message")
      # Place ici le code nécessaire pour enregistrer ou mettre à jour le classeur Excel avec final_mapping.
      # Par exemple, appeler une fonction de sauvegarde ou d'exportation.
    })
  })
}
