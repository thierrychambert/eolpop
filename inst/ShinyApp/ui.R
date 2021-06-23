rm(list = ls(all.names = TRUE))

library(shiny)
library(shinyjs)
library(shinyMatrix)
library(tidyverse)

library(eolpop)

# source("./inst/ShinyApp/f_output.R")
source("./inst/ShinyApp/param_fixes.R")

data <- read.csv("./inst/ShinyApp/species_list.csv", sep = ",")
choicesSpecies <- unique(as.character(data$NomEspece))


ui <- fluidPage(
  useShinyjs(),
  titlePanel("EolPop : Impact demographique des eoliennes"),
  fluidRow(column(width = 6,
                  selectInput(inputId = "species_choice",
                              h4(strong("Selectionner une espece")),
                              c(choose = "", choicesSpecies)))),
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 12,

               ## 1. Fatalities
               # h4(strong("Mortalites")),
               fluidRow(
                 actionButton(inputId = "Mortality", label = "Mortalites"),
                 radioButtons(inputId = "morta_type",
                              h4("Type de mortalite"),
                              choices = list(
                                "Non cumulees (1 seul parc eolien)",
                                "Cumulees")),
                 textInput(inputId = "nber_park",
                           label = NULL,
                           placeholder = "--Nombre de parcs--"),
                 textInput(inputId = "nber_wind_turbine",
                           label = NULL,
                           placeholder = "--Nombre d'eoliennes--"),
                 radioButtons(inputId = "data",
                              h4("Source des donnees"),
                              choices = list("Suivi (observations terrains + EolApp)", "Modele predictif (type Band)",
                                             "Dire d'expert")),
                 matrixInput("expert",
                             value = matrix("", 4, 6, dimnames = list(c("#1", "#2", "#3", "#4"), c("Nom", "Poids", "Min", "Best", "Max", "IC" ))),
                             class = "numeric",
                             rows = list(extend = FALSE),
                             cols = list(names = TRUE)),
                 actionButton(inputId = "run_expert", label = "Analyse Experts"),

                 numericInput(inputId = "M1", label = h5("Valeur estimee"), min = 0, max = Inf, step = 1, value = 5),
                 numericInput(inputId = "M1_se", label = h5("Erreur standard"), min = 0, max = Inf, step = 1, value = 0),
                 numericInput(inputId = "M1_ic", label = h5("Intervalle de confiance"), min = Inf, max = Inf, step = 1, value = 0),

                 br(),
                 radioButtons(inputId = "mort_cons",
                              h4("Modelisation"),
                              choices = list("Taux de mortalite (h) constant" = "h",
                                             "Nombre de mortalites (M) constant" = "M"),
                              selected = "h"),
               ),

               br(),
               ## 2. Population size
               # h4(strong("Taille de la population")),
               fluidRow(
                 actionButton(inputId = "pop_size", label = "Taille de la population"),
                 radioButtons(inputId =  "N_type",
                              h4("Unite"),
                              choices = list("Nombre de couple" = "Npair",
                                             "Effectif total" = "Ntotal"),
                              selected = "Npair"),
                 matrixInput("expert_2",
                             value = matrix("", 4, 3, dimnames = list(c("juv 1", "juv 2", "juv 3", "Adulte"), c("Moy", "LCI", "UCI" ))),
                             rows = list(names = TRUE),
                             cols = list(names = TRUE)),
                 numericInput(inputId = "N00_mu", label = h5("Valeur estimee"), min = 0, max = Inf, step = 50, value = 200),
                 numericInput(inputId = "N00_se", label = h5("Erreur standard"), min = 0, max = Inf, step = 1, value = 0),
                 numericInput(inputId = "IC_2", label = h5("Intervalle de confiance"), min = Inf, max = Inf, step = 1, value = 0),
               ),


               br(),
               ## 3. Population trend
               # h4(strong("Tendance de la population")),
               fluidRow(
                 actionButton(inputId = "pop_trend", label = "Tendance de la population"),
                 radioButtons(inputId = "lambda_type", h4("Type"),
                              choices = list("Taux de croissance", "Tendance locale ou regionale",
                                             "Tendance nationale")),

                 numericInput(inputId = "lam0_mu", label = h5("Valeur estimee"), min = 0, max = Inf, step = 0.05, value = 0.95),
                 numericInput(inputId = "lam0_se", label = h5("Erreur standard"), min = 0, max = Inf, step = 1, value = 0),
                 numericInput(inputId = "IC_3", label = h5("Intervalle de confiance"), min = Inf, max = Inf, step = 1, value = 0),
                 radioButtons("trend", h4("Tendance"), choices = list("Croissance", "Stable", "Declin")),
                 radioButtons(inputId = "trend_2", label = NULL, choices = list("Fort", "Moyen", "Faible")),
               ),


               br(),
               ## 4. Vital rates
               # h4(strong("Parametres demographiques")),
               fluidRow(
                 actionButton(inputId = "params_demog", label = "Parametres demographiques"),
                 radioButtons(inputId = "auto", label = "Saisie", choices = list("Automatique", "Manuelle")),
                 matrixInput("mat_params_demog",
                             value = matrix("", 4, 3, dimnames = list(c("juv 1", "juv 2", "juv 3", "Adulte"), c("Moy", "LCI", "UCI" ))),
                             rows = list(names = TRUE),
                             cols = list(names = TRUE)
                             )
               ),
               )
        )
      ),


    ### MAIN PANEL
    mainPanel(
      tabsetPanel(
        tabPanel(title = "Impact population",
                 strong(span(textOutput("message"), style="color:blue; font-size:24px", align = "center")),
                 actionButton(inputId = "run", label = "Lancer l'analyse"),
                 hr(),
                 h4("Graphique : trajectoire demographique", align = "center"),
                 plotOutput("graph", width = "100%", height = "550px"),
                 plotOutput("graph_eli", width = "100%", height = "550px"),
                 ), # tabPanel

        tabPanel(title = "Distribution parametres",

                 textOutput(outputId = "Mean"),
                 textOutput(outputId = "sqsrt_var"),
                 plotOutput(outputId = "plot")
                 ) # tabPanel

      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage

# shinyApp(ui = ui, server = server)
