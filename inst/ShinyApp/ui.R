rm(list = ls(all.names = TRUE))

## Load libraries
library(shiny)
library(shinyjs)
library(shinyMatrix)
library(tidyverse)
library(eolpop)


## Load species list
species_data <- read.csv("./inst/ShinyApp/species_list.csv", sep = ",")
species_list <- unique(as.character(species_data$NomEspece))

## Load survival and fecundities data
data_sf <- read.csv("./inst/ShinyApp/survivals_fecundities_species.csv", sep = ",")#, encoding = "UTF-8")
(data_sf)

# Fixed parameters (for now)
nsim = 10
coeff_var_environ = 0.10
time_horzion = 30

#####################
### Pre-fill data ###
#####################

## Data elicitation pre-fill data
# fatalities
eli_fatalities <- c(1.0, 2, 5, 8,  0.80,
                    0.2, 0, 3, 6,  0.90,
                    0.2, 2, 4, 10, 0.90,
                    0.1, 1, 3, 7,  0.70)

# population size
eli_pop_size <-   c(1.0, 150, 200, 250, 0.80,
                    0.5, 120, 180, 240, 0.90,
                    0.8, 170, 250, 310, 0.90,
                    0.3, 180, 200, 230, 0.70)

# carrying capacity
eli_carrying_cap <- c(1.0, 500, 700, 1000, 0.80,
                      0.5, 1000, 1500, 2000, 0.90,
                      0.8, 800, 1200, 1600, 0.90,
                      0.3, 100, 1200, 1500, 0.70)

# population growth rate
eli_pop_growth <- c(1 , 0.95, 0.98, 1.00, 0.95,
                    0.2, 0.97, 1.00, 1.01, 0.90,
                    0.5, 0.92, 0.96, 0.99, 0.90,
                    0.3, 0.90, 0.95, 0.98, 0.70)

## Other pre-fill data
# fatalities for several wind farms (cumulated impacts)
init_cumul <- c(10, 5, 8,
                0.05, 0.05, 0.05,
                2010, 2015, 2018)

init_cumul_add <- c(3, 0.05, 2020)


# DD parameters
theta = 1

# Define theoretical rMAX for the species
#rMAX_species <- rMAX_spp(surv = tail(init_survivals,1), afr = min(which(init_fecundities != 0)))


##--------------------------------------------
##  User Interface                          --
##--------------------------------------------
ui <- fluidPage(
  useShinyjs(),
  titlePanel("eolpop : Impact demographique des éoliennes"),


  # Creation of the first page (select species, analysis type choice)
  wellPanel(

    selectInput(inputId = "species_choice", selected = 1,
                h4(strong("Sélectionner une espèce")),
                choices = species_list),

    radioButtons(inputId = "analysis_choice",
                 h4(strong("Sélectionner un type d'analyse")),
                 choices = c("Impacts non cumulés" = "scenario", "Impacts cumulés" = "cumulated"))

  ), # End wellPanel


  ##--------------------------------------------
  ##  General information                     --
  ##--------------------------------------------

  wellPanel(
    h2("Valeurs actuelles"),

    fluidRow(
      column(width = 4,
        div( textOutput(outputId = "species_name") ,
             style="color:black; font-size:18px; font-weight: bold", align = "left"),
      )
    ),

    fluidRow(
      column(width = 4,

             br(),
             h3("Mortalités"),
             textOutput(outputId = "fatalities_mean_info"),
             textOutput(outputId = "fatalities_se_info"),

             br(),
             h3("Taille de la population"),
             textOutput(outputId = "pop_size_type_info"),
             textOutput(outputId = "pop_size_mean_info"),
             textOutput(outputId = "pop_size_se_info")),

        column(width = 4,

               br(),
               h3("Capacité de charge"),
               textOutput(outputId = "carrying_capacity_info"),

               br(),
               h3("Tendance de la population"),
               textOutput(outputId = "pop_trend_type_info"),
               textOutput(outputId = "pop_growth_mean_info"),
               textOutput(outputId = "pop_growth_se_info")),

          column(width = 4,

                 br(),
                 h3("Paramètres démographiques"),
                 tableOutput(outputId = "vital_rates_info"))

    ) # # End wellPanel
  ), # End fluidRow


  # Paramter Inputs (fatalities, pop size, carrying capacity, pop trend and vital rates).

  sidebarLayout(
    sidebarPanel(

      h2("Modifier les paramètres"),

      br(" "),
      ##--------------------------------------------
      ##  1. Fatalities                           --
      ##--------------------------------------------
      tags$style(HTML('#button_fatalities{background-color:#C2C8D3}')),
      actionButton(inputId = "button_fatalities",
                   label = tags$span("Mortalités annuelles", style = "font-weight: bold; font-size: 18px;")
      ),

      radioButtons(inputId = "fatal_constant",
                   label = h4("Modélisation"),
                   choices = c("Taux de mortalités (h) constant" = "h",
                               "Nombre de mortalités (M) constant" = "M")),

      ### Part for non-cumulated impacts
      # Input type
      radioButtons(inputId = "fatalities_input_type",
                   label = h4("Type de saisie"),
                   choices = c("Valeurs", "Elicitation d'expert")),

      # Values
      numericInput(inputId = "fatalities_mean",
                   label = "Moyenne des mortalités annuelles",
                   value = 5,
                   min = 0, max = Inf, step = 0.5),
      numericInput(inputId = "fatalities_se",
                   label = "Erreur-type des mortalités annuelles",
                   value = 0.05,
                   min = 0, max = Inf, step = 0.1),

      # Matrix for expert elicitation
      matrixInput(inputId = "fatalities_mat_expert",
                  value = matrix(data = eli_fatalities, 4, 5,
                                 dimnames = list(c("#1", "#2", "#3", "#4"),
                                                 c("Poids", "Min", "Best", "Max", "% IC" )),
                                 byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      actionButton(inputId = "fatalities_run_expert", label = "Analyse"),

      ### Part for cumulated impacts

      numericInput(inputId = "farm_number_cumulated",
                   label = "Nombre de parcs éoliens",
                   value = 3, min = 2, max = Inf, step = 1),

      matrixInput(inputId = "fatalities_mat_cumulated",
                  value = matrix(init_cumul, 3, 3,
                                 dimnames = list(c(paste0("Parc n°", c(1:3))),
                                                 c("Moyenne",
                                                   "Erreur-type",
                                                   "Année de mise en service du parc"))),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),


      hr(),
      ##--------------------------------------------
      ##  2. Population Size                      --
      ##--------------------------------------------
      tags$style(HTML('#button_pop_size{background-color:#C2C8D3}')),
      actionButton(inputId = "button_pop_size",
                   label = tags$span("Taille de la population", style = "font-weight: bold; font-size: 18px;")
      ),

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
                   label = "Erreur-type de la taille de la population",
                   value = 25,
                   min = 0, max = Inf, step = 1),

      matrixInput(inputId = "pop_size_mat_expert",
                  value = matrix(data = eli_pop_size, 4, 5,
                                 dimnames = list(c("#1", "#2", "#3", "#4"),
                                                 c("Poids", "Min", "Best", "Max", "% IC" )),
                                 byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      actionButton(inputId = "pop_size_run_expert", label = "Analyse"),


      hr(),
      ##--------------------------------------------
      ##  3. Carrying capacity                    --
      ##--------------------------------------------
      tags$style(HTML('#button_carrying_cap{background-color:#C2C8D3}')),
      actionButton(inputId = "button_carrying_cap",
                   label = tags$span("Capacité de charge", style = "font-weight: bold; font-size: 18px;")
      ),

      radioButtons(inputId = "carrying_cap_input_type",
                   label = h4("Type de saisie"),
                   choices = c("Valeurs", "Elicitation d'expert")),

      numericInput(inputId = "carrying_capacity",
                   label = "Capacité de charge",
                   value = 1000,
                   min = 0, max = Inf, step = 100),

      matrixInput(inputId = "carrying_cap_mat_expert",
                  value = matrix(data = eli_carrying_cap, 4, 5,
                                 dimnames = list(c("#1", "#2", "#3", "#4"),
                                                 c("Poids", "Min", "Best", "Max", "% IC" )),
                                 byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      actionButton(inputId = "carrying_cap_run_expert", label = "Utiliser valeurs experts"),


      hr(),
      ##--------------------------------------------
      ##  4. Population Trend                     --
      ##--------------------------------------------
      tags$style(HTML('#button_pop_trend{background-color:#C2C8D3}')),
      actionButton(inputId = "button_pop_trend",
                   label = tags$span("Tendance de la population", style = "font-weight: bold; font-size: 18px;")
                   ),

      radioButtons(inputId = "lambda_input_type",
                   label = h4("Type de saisie"),
                   choices = c("Taux de croissance", "Elicitation d'expert", "Tendance locale ou régionale")),

      numericInput(inputId = "pop_growth_mean",
                   label = "Moyenne de la croissance de la population",
                   value = 1,
                   min = 0, max = Inf, step = 0.01),

      numericInput(inputId = "pop_growth_se",
                   label = "Erreur-type de la croissance de la population",
                   value = 0,
                   min = 0, max = Inf, step = 0.01),

      matrixInput(inputId = "pop_growth_mat_expert",
                  value = matrix(data = eli_pop_growth, 4, 5,
                                 dimnames = list(c("#1", "#2", "#3", "#4"),
                                                 c("Poids", "Min", "Best", "Max", "% IC" )),
                                 byrow = TRUE),
                  class = "numeric",
                  rows = list(names = TRUE),
                  cols = list(names = TRUE)),

      actionButton(inputId = "pop_growth_run_expert", label = "Analyse"),

      radioButtons(inputId = "pop_trend",
                   label = NULL,
                   choices = c("Croissance", "Stable", "Déclin")),

      radioButtons(inputId = "pop_trend_strength",
                   label = NULL,
                   choices = c("Faible", "Moyen", "Fort")),

      # tags$style("#pop_trend_strength {position:fixed; top: 600px; right: 100px;}"),


      hr(),
      ##--------------------------------------------
      ##  5. Vital rates                         --
      ##--------------------------------------------
      tags$style(HTML('#button_vital_rates{background-color:#C2C8D3}')),
      actionButton(inputId = "button_vital_rates",
                   label = tags$span("Paramètres démographiques", style = "font-weight: bold; font-size: 18px;")
      ),

      br(),
      matrixInput(inputId = "mat_fill_vr",
                  label = "",
                  value = matrix(data = NA, 3, 2,
                                 dimnames = list(c("Juv 1", "Juv 2", "Adulte"), c("Survie", "Fécondité"))),
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
                 hr(),
                 h4("#Graphe élicitation d'expert pour les mortalités", align = "center"),
                 plotOutput(outputId = "fatalities_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la taille de la population", align = "center"),
                 plotOutput(outputId = "pop_size_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la capacité de charge", align = "center"),
                 plotOutput(outputId = "carrying_cap_expert_plot"),
                 hr(),
                 h4("#Graphe élicitation d'expert pour la tendance de la population", align = "center"),
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
