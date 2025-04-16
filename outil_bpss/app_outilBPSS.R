library(shiny)
library(openxlsx2)
library(readxl)
library(dplyr)
library(DT)
library(stringr)  # Pour utiliser str_detect

source("~/work/budgibot/outil_bpss/ui_outilBPSS.R")  # Charger l'UI
source("~/work/budgibot/outil_bpss/server_outilBPSS.R")  # Charger la logique du serveur

shinyApp(ui = ui, server = server)