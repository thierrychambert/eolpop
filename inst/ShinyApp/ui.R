rm(list = ls(all.names = TRUE))

library(shiny)
library(shinyjs)
library(shinyMatrix)
library(tidyverse)

library(eolpop)


#??? Species data / list
species_data <- read.csv("./inst/ShinyApp/species_list.csv", sep = ",")
species_list <- unique(as.character(species_data$NomEspece))

# Fixed parameters (for now)
nsim = 10
coeff_var_environ = 0.10
time_horzion = 30
survivals <- c(0.5, 0.7, 0.8, 0.95)
fecundities <- c(0, 0, 0.05, 0.55)

# Data elicitation, fatalities for cumulated impacts, vital rates and DD_params
data_eli = c("",1, 50, 70, 100, 0.80, "", 0.2, 200, 240, 280, 0.90, "", 0.2, 100, 180, 300, 0.90,"",  0.1, 120, 160, 220, 0.70)
data_fatalities = c(10, 5, 8, 0.05, 0.05, 0.05, 2010, 2015, 2018)
data_vr = c(0.5, 0.7, 0.8, 0.95, 0, 0, 0.05, 0.55)

# DD parameters
theta = 1

# Define theoretical rMAX for the species
rMAX_species <- rMAX_spp(surv = tail(survivals,1), afr = min(which(fecundities != 0)))
rMAX_species


##--------------------------------------------
##  User Interface                          --
##--------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("eolpop : Impact demographique des éoliennes"),

  # Creation of the first page (select species, analysis type choice)

  wellPanel(
    selectInput(inputId = "species_list",
                h4(strong("Sélection d'une espèce ou groupe d'espèces")),
                choices = species_list),
    radioButtons(inputId = "analysis_choice",
                 h4(strong("Sélectionner un type d'analyse")),
                 choices = c("Impacts non cumulés" = "scenario", "Impacts cumulés" = "cumulated"))
  ), # End wellPanel


  ##--------------------------------------------
  ##  General information                     --
  ##--------------------------------------------

  wellPanel(
    fluidRow(
      column(width = 6,
             textOutput(outputId = "specie_name"),
             h4("Mortalités"),
             textOutput(outputId = "fatalities_mean_info"),
             textOutput(outputId = "fatalities_se_info"),
             textOutput(outputId = "fatalities_expert_info"),
             h4("Taille de la population"),
             textOutput(outputId = "pop_size_mean_info"),
             textOutput(outputId = "pop_size_se_info"),
             textOutput(outputId = "pop_size_expert_info")),
      fluidRow(
        column(width = 6,
               h4("Capacité de charge"),
               textOutput(outputId = "carrying_capacity_info"),
               textOutput(outputId = "carrying_cap_expert_info"),
               h4("Tendance de la population"),
               textOutput(outputId = "pop_trend_type_info"),
               textOutput(outputId = "pop_trend_mean_info"),
               textOutput(outputId = "pop_trend_se_info"),
               textOutput(outputId = "pop_trend_expert_info"))
      )
    )
  ), # End wellPanel


  # Paramter Inputs (fatalities, pop size, carrying capacity, pop trend and vital rates).

  sidebarLayout(
    sidebarPanel(

      ##--------------------------------------------
      ##  1. Fatalities                           --
      ##--------------------------------------------

      actionButton(inputId = "button_fatalities",
                   label = "Mortalités"),
      radioButtons(inputId = "fatal_constant",
                   label = h4("Modélisation"),
                   choices = c("Taux de mortalités (h) constant" = "h",
                               "Nombre de mortalités (M) constant" = "M")),

      ### Part for non-cumulated impacts
      # Input type
      radioButtons(inputId = "fatalities_input_type",
                   label = h4("Source des données"),
                   choices = c("Valeurs", "Elicitation d'expert")),

      # Values
      numericInput(inputId = "fatalities_mean",
                   label = "Moyenne des mortalités annuelles",
                   value = 5,
                   min = 0, max = Inf, step = 0.5),
      numericInput(inputId = "fatalities_se",
                   label = "Ecart-type des mortalités annuelles",
                   value = 0.05,
                   min = 0, max = Inf, step = 0.1),

      # Matrix for expert elicitation
      matrixInput(inputId = "fatalities_mat_expert",
                  value = matrix(data = data_eli, 4, 6, dimnames = list(c("#1", "#2", "#3", "#4"), c("Nom", "Poids", "Min", "Meilleure Estimation", "Max", "IC (coverage)" )), byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      ### Part for cumulated impacts

      numericInput(inputId = "farm_number_cumulated",
                   label = "Nombre de parcs éoliens",
                   value = 2, min = 2, max = Inf, step = 1),
      matrixInput(inputId = "fatalities_mat_cumulated",
                  value = matrix(data = data_fatalities, 3, 3,
                                 dimnames = list(c("#1", "#2", "#3"),
                                                 c("Moyennes des mortalités annuelles",
                                                   "Ecart-type des mortalités annuelles",
                                                   "Année de mise en service du parc"))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),


      ##--------------------------------------------
      ##  2. Population Size                      --
      ##--------------------------------------------

      br(" "),
      actionButton(inputId = "button_pop_size",
                   label = "Taille de la population"),

      radioButtons(inputId = "pop_size_type",
                   label = h4("Unité"),
                   choices = c("Nombre de couple" = "Npair", "Effectif total" = "Ntotal")),

      radioButtons(inputId = "pop_size_input_type",
                   label = h4("Type de saisie"),
                   choices = c("Valeurs", "Elicitation d'expert")),

      numericInput(inputId = "pop_size_mean",
                   label = "Moyenne de la taille de la population",
                   value = 200,
                   min = 0, max = Inf, step = 50),

      numericInput(inputId = "pop_size_se",
                   label = "Ecart-type de la taille de la population",
                   value = 25,
                   min = 0, max = Inf, step = 1),

       matrixInput(inputId = "pop_size_mat_expert",
                  value = matrix(data = data_eli, 4, 6,
                                 dimnames = list(c("#1", "#2", "#3", "#4"),
                                                 c("Nom", "Poids", "Min", "Best", "Max", "IC" )),
                                 byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),


      ##--------------------------------------------
      ##  3. Carrying capacity                    --
      ##--------------------------------------------

      br(" "),
      actionButton(inputId = "button_carrying_cap",
                   label = "Capacité de charge"),

      radioButtons(inputId = "carrying_cap_input_type",
                   label = h4("Type d'unité"),
                   choices = c("Valeurs", "Elicitation d'expert")),

      numericInput(inputId = "carrying_capacity",
                   label = "Capacité de charge",
                   value = 1000,
                   min = 0, max = Inf, step = 100),

      matrixInput(inputId = "carrying_cap_mat_expert",
                  value = matrix("", 4, 6, dimnames = list(c("#1", "#2", "#3", "#4"), c("Nom", "Poids", "Min", "Meilleure Estimation", "Max", "IC (coverage)" ))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      ##--------------------------------------------
      ##  4. Population Trend                     --
      ##--------------------------------------------

      br(" "),
      actionButton(inputId = "button_pop_trend",
                   label = "Tendance de la population"),

      radioButtons(inputId = "lambda_input_type",
                   label = h4("Type de tendance"),
                   choices = c("Taux de croissance", "Elicitation d'expert", "Tendance locale ou régionale")),

      numericInput(inputId = "pop_growth_mean",
                   label = "Moyenne de la croissance de la population",
                   value = 1,
                   min = 0, max = Inf, step = 0.01),

      numericInput(inputId = "pop_growth_se",
                   label = "Ecart-type de la croissance de la population",
                   value = 0,
                   min = 0, max = Inf, step = 0.01),

      matrixInput(inputId = "pop_growth_mat_expert",
                  value = matrix("", 4, 6, dimnames = list(c("#1", "#2", "#3", "#4"), c("Nom", "Poids", "Min", "Meilleure Estimation", "Max", "IC (coverage)" ))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      radioButtons(inputId = "pop_trend",
                   label = h4("Tendance de la population"),
                   choices = c("Croissance", "Stable", "Déclin")),

      radioButtons(inputId = "pop_trend_strength",
                   label = NULL,
                   choices = c("Faible", "Moyen", "Fort")),



      ##--------------------------------------------
      ##  5. Vital rates                         --
      ##--------------------------------------------

      br(" "),
      actionButton(inputId = "button_vital_rates",
                   label = "Paramètres démographiques"),

      radioButtons(inputId = "fill_type_vr",
                   label = "Type de saisie",
                   choices = c("Automatique", "Manuelle")),

      matrixInput(inputId = "mat_display_vr",
                  value = matrix("", 4, 2, dimnames = list(c("Juv 1", "Juv 2", "Juv 3", "Adulte"), c("Survie", "Fécondité"))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      matrixInput(inputId = "mat_fill_vr",
                  value = matrix(data = data_vr, 4, 2, dimnames = list(c("Juv 1", "Juv 2", "Juv 3", "Adulte"), c("Survie", "Fécondité"))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE))

    ), # End sidebarPanel


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    # Creation of outputs parts

    mainPanel(
      tabsetPanel(
        tabPanel(title = "Impact population",
                 strong(span(textOutput("message"), style="color:blue; font-size:24px", align = "center")),
                 br(),
                 numericInput(inputId = "nsim", label = "Nombre de simulations",
                              value = 50, min = 0, max = Inf, step = 10),
                 br(),
                 actionButton(inputId = "run", label = "Lancer l'analyse"),
                 hr(),
                 h4("Graphique : Impact relatif de chaque scénario", align = "center"),
                 plotOutput("graph_impact", width = "100%", height = "550px"),
                 hr(),
                 h4("Graphique : Trajectoire démographique", align = "center"),
                 plotOutput("graph_traj", width = "100%", height = "550px")),

        tabPanel(title = "Distribution paramètres",
                 br(),
                 actionButton(inputId = "run_expert", label = "Analyse"),
                 br(),
                 hr(),
                 h4("#Graphe élicitation d'expert pour les mortalités", align = "center"),
                 textOutput(outputId = "fatalities_expert_mean"),
                 textOutput(outputId = "fatalities_expert_sqrt_var"),
                 plotOutput(outputId = "fatalities_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la taille de la population", align = "center"),
                 textOutput(outputId = "pop_size_expert_mean"),
                 textOutput(outputId = "pop_size_expert_sqrt_var"),
                 plotOutput(outputId = "pop_size_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la capacité de charge", align = "center"),
                 textOutput(outputId = "carrying_cap_expert_mean"),
                 textOutput(outputId = "carrying_cap_expert_sqrt_var"),
                 plotOutput(outputId = "carrying_cap_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la tendance de la population", align = "center"),
                 textOutput(outputId = "pop_growth_expert_mean"),
                 textOutput(outputId = "pop_growth_expert_sqrt_var"),
                 plotOutput(outputId = "pop_growth_expert_plot"),
        ),

        tabPanel(title = "Rapport",
                 br(),
                 radioButtons(inputId = "lifestyle",
                              h4("Mode de vie de l'espèce"),
                              choices = c("Sédentaire", "Non-sédentaire nicheur", "Non-sédentaire hivernant", "Migrateur de passage")),
                 numericInput(inputId = "wind_turbines",
                              h4("Nombre d'éoliennes"),
                              value = 5, min = 0, max = Inf, step = 1),
                 numericInput(inputId = "farm_number",
                              h4("Nombre de parcs"),
                              value = 1, min = 0, max = Inf, step = 1),
                 numericInput(inputId = "wind_turbines_2",
                              h4("Nombre d'éoliennes"),
                              value = 1, min = 0, max = Inf, step = 1)

        ) # End tabPanel
      ) # End tabSetPanel
    ) # End mainPanel
  ) # sidebarLayout
) # FluidPage

# End UI

# shinyApp(ui = ui, server = server)
