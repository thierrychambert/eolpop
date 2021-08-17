rm(list = ls(all.names = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial stuff
{
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
  theta = 1 # DD parameter theta
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-fill data
{
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
}
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##  User Interface
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# fluidPage
{ui <- fluidPage(

  useShinyjs(),
  titlePanel("eolpop : Impact demographique des éoliennes"),


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 1 : type of analysis and species
  {wellPanel(
    {fluidRow(

      # Select type of analysis : cumulated impacted or not
      {column(width = 4,
              radioButtons(inputId = "analysis_choice",
                           h4(strong("Sélectionner un type d'analyse")),
                           choices = c("Impacts non cumulés" = "scenario", "Impacts cumulés" = "cumulated")),

              selectInput(inputId = "species_choice",
                          selected = 1, width = '80%',
                          label = h4(strong("Sélectionner une espèce")),
                          choices = species_list),
      )}, # close column

      # Show vital rate values
      {column(width = 4,
              fluidRow(
                h4(strong("Paramètres démographiques")),
                tableOutput(outputId = "vital_rates_info"),
              ),
      )}, # close column


      ## Modify vital rates (if needed)
      {column(width = 4,
              tags$style(HTML('#button_vital_rates{background-color:#C2C8D3}')),
              actionButton(inputId = "button_vital_rates",
                           label = tags$span("Modifier les paramètres démographiques",
                                             style = "font-weight: bold; font-size: 18px;")
              ),

              br(),
              matrixInput(inputId = "mat_fill_vr",
                          label = "",
                          value = matrix(data = NA, 3, 2,
                                         dimnames = list(c("Juv 1", "Juv 2", "Adulte"), c("Survie", "Fécondité"))),
                          class = "numeric",
                          rows = list(names = TRUE),
                          cols = list(names = TRUE)
              )

      )}, # close column

    )}, # End fluidRow
  )}, # End wellPanel
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 2 : Model parameters
  {wellPanel(

    p("Saisie des paramètres", style="font-size:28px"),

    {fluidRow(

      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  1. Fatalities
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              tags$style(HTML('#button_fatalities{background-color:#C2C8D3}')),
              actionButton(inputId = "button_fatalities", width = '100%',
                           label = tags$span("Mortalités annuelles", style = "font-weight: bold; font-size: 18px;")
              ),

              ### Part for non-cumulated impacts
              # Input type
              {conditionalPanel("output.hide_fatalities",
                                br(),

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "fatalities_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Valeurs" = "val", "Elicitation d'expert" = "eli_exp")),

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
                                                       value = matrix(data = eli_fatalities, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "% IC" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "fatalities_run_expert", label = "Utiliser valeurs experts"),

                                           ### Part for cumulated impacts

                                           numericInput(inputId = "farm_number_cumulated",
                                                        label = "Nombre de parcs éoliens",
                                                        value = 3, min = 2, max = Inf, step = 1),

                                           matrixInput(inputId = "fatalities_mat_cumulated",
                                                       value = matrix(init_cumul, 3, 3,
                                                                      dimnames = list(c(paste0("Parc num.", c(1:3))),
                                                                                      c("Moyenne",
                                                                                        "Erreur-type",
                                                                                        "Année de mise en service du parc"))),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),
                                )}, # close wellPanel

              )}, # close conditional panel

      )}, # end column "mortalité"

      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  2. Population Size
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              tags$style(HTML('#button_pop_size{background-color:#C2C8D3}')),
              actionButton(inputId = "button_pop_size", width = '100%',
                           label = tags$span("Taille de la population", style = "font-weight: bold; font-size: 18px;")
              ),

              {conditionalPanel("output.hide_pop_size",
                              br(),

                              {wellPanel(style = "background:#FFF8DC",
                                 radioButtons(inputId = "pop_size_unit", inline = TRUE,
                                              label = "Unité",
                                              choices = c("Nombre de couple" = "Npair", "Effectif total" = "Ntotal"),
                                              selected = "Ntotal"),
                              )}, # close wellPanel 1

                              {wellPanel(style = "background:#F0F8FF",

                                         radioButtons(inputId = "pop_size_input_type",
                                                      label = "Type de saisie",
                                                      choices = c("Valeurs" = "val", "Elicitation d'expert" = "eli_exp")),

                                         numericInput(inputId = "pop_size_mean",
                                                      label = "Moyenne de la taille de la population",
                                                      value = 200,
                                                      min = 0, max = Inf, step = 50),

                                         numericInput(inputId = "pop_size_se",
                                                      label = "Erreur-type de la taille de la population",
                                                      value = 25,
                                                      min = 0, max = Inf, step = 1),

                                         matrixInput(inputId = "pop_size_mat_expert",
                                                     value = matrix(data = eli_pop_size, nrow = 4, ncol = 5,
                                                                    dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                    c("Poids", "Min", "Best", "Max", "% IC" )),
                                                                    byrow = TRUE),
                                                     class = "numeric",
                                                     rows = list(names = TRUE),
                                                     cols = list(names = TRUE)),

                                         actionButton(inputId = "pop_size_run_expert", label = "Utiliser valeurs experts"),
                              )}, # close wellPanel 2

              )}, # close conditional panel

      )}, # end column "mortalite"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  3. Population Growth
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              tags$style(HTML('#button_pop_growth{background-color:#C2C8D3}')),
              actionButton(inputId = "button_pop_growth", width = '100%',
                           label = tags$span("Tendance de la population", style = "font-weight: bold; font-size: 18px;")
              ),

              {conditionalPanel("output.hide_pop_growth",
                                br(),

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "pop_growth_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Taux de croissance" = "val",
                                                                    "Elicitation d'expert" = "eli_exp",
                                                                    "Tendance locale ou régionale" = "trend")),

                                           ## Input values: mean and se
                                           numericInput(inputId = "pop_growth_mean",
                                                        label = "Moyenne de la croissance de la population",
                                                        value = 1.1,
                                                        min = 0, max = Inf, step = 0.01),

                                           numericInput(inputId = "pop_growth_se",
                                                        label = "Erreur-type de la croissance de la population",
                                                        value = 0.01,
                                                        min = 0, max = Inf, step = 0.01),

                                           ## Input expert elicitation: table
                                           matrixInput(inputId = "pop_growth_mat_expert",
                                                       value = matrix(data = eli_pop_growth, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "% IC" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "pop_growth_run_expert", label = "Utiliser valeurs experts"),

                                           ## Input trend: radio buttons
                                           {fluidRow(
                                             # Trend
                                             column(6,
                                                    radioButtons(inputId = "pop_trend",
                                                                 label = "Tendance",
                                                                 choices = c("Croissance" = "growth",
                                                                             "Stable" = "stable",
                                                                             "Déclin" = "decline")),
                                             ),

                                             # Strength of trend
                                             column(6,
                                                    radioButtons(inputId = "pop_trend_strength",
                                                                 label = "Force",
                                                                 choices = c("Faible" = "weak",
                                                                             "Moyen" = "average",
                                                                             "Fort" = "strong")),
                                             ),
                                           )}, # close fluidRow

                                )}, # close wellPanel

              )}, # close conditional panel

      )}, # end column "mortalite"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  4. Carrying capacity
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              tags$style(HTML('#button_carrying_cap{background-color:#C2C8D3}')),
              actionButton(inputId = "button_carrying_cap", width = '100%',
                           label = tags$span("Capacité de charge", style = "font-weight: bold; font-size: 18px;")
              ),

              {conditionalPanel("output.hide_carrying_cap",
                                br(),

                                {wellPanel(style = "background:#FFF8DC",
                                           span(textOutput(outputId = "carrying_cap_unit_info"), style="font-size:16px"),

                                )}, # close wellPanel 1

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "carrying_cap_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Valeurs" = "val", "Elicitation d'expert" = "eli_exp")),

                                           numericInput(inputId = "carrying_capacity",
                                                        label = "Capacité de charge",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 100),

                                           matrixInput(inputId = "carrying_cap_mat_expert",
                                                       value = matrix(data = eli_carrying_cap, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "% IC" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "carrying_cap_run_expert", label = "Utiliser valeurs experts"),

                                )}, # close wellPanel 2

              )}, # close conditional panel

      )}, # end column "fatalities"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


    )}, # # End fluidRow

  )}, # # End wellPanel
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  ##  Side Panel : Parameter information
  {sidebarLayout(
    {sidebarPanel(

      p("Valeurs utilisées", style="font-size:28px"),

      # Mortalites annuelles
      {wellPanel(style = "background:#DCDCDC",
                 p("Mortalités annuelles", style="font-size:20px; font-weight: bold"),
                 span(textOutput(outputId = "fatalities_mean_info"), style="font-size:16px"),
                 span(textOutput(outputId = "fatalities_se_info"), style="font-size:16px"),
      )},

      # Taille de population
      {wellPanel(style = "background:#DCDCDC",
                 p("Taille de la population", style="font-size:20px; font-weight: bold"),
                 shiny::tags$u(textOutput(outputId = "pop_size_unit_info"), style="font-size:16px"),
                 p(""),
                 span(textOutput(outputId = "pop_size_mean_info"), style="font-size:16px"),
                 span(textOutput(outputId = "pop_size_se_info"), style="font-size:16px"),

      )},

      # Tendance de la population
      {wellPanel(style = "background:#DCDCDC",
                 p("Tendance de la population", style="font-size:20px; font-weight: bold"),
                 span(textOutput(outputId = "pop_growth_mean_info"), style="font-size:16px"),
                 span(textOutput(outputId = "pop_growth_se_info"), style="font-size:16px"),
      )},

      # Capacite de charge
      {wellPanel(style = "background:#DCDCDC",
                 p("Capacité de charge", style="font-size:20px; font-weight: bold"),
                 span(textOutput(outputId = "carrying_capacity_info"), style="font-size:16px"),
      )},


    )}, # End sidebarPanel


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    # Creation of outputs parts

    {mainPanel(
      tabsetPanel(
        tabPanel(title = "Distribution paramètres",
                 br(),
                 hr(),

                 #h3("Mortalites annuelles", align = "center"),
                 span(textOutput(outputId = "title_distri_plot"), style="font-size:24px; font-weight:bold"),
                 plotOutput(outputId = "distri_plot"),

        ), # End tabPanel


        tabPanel(title = "Impact population",

                 br(""),
                 numericInput(inputId = "nsim", label = "Nombre de simulations",
                              value = 10, min = 0, max = Inf, step = 10),

                 radioButtons(inputId = "fatal_constant",
                              label = h4("Modélisation"),
                              choices = c("Taux de mortalités (h) constant" = "h",
                                          "Nombre de mortalités (M) constant" = "M")),

                 br(),

                 strong(span(textOutput("message"), style="color:blue; font-size:24px", align = "center")),
                 br(),

                 actionButton(inputId = "run", label = "Lancer l'analyse"),
                 hr(),
                 h4("Graphique : Impact relatif de chaque scénario", align = "center"),
                 plotOutput("graph_impact", width = "100%", height = "550px"),
                 hr(),
                 h4("Graphique : Trajectoire démographique", align = "center"),
                 plotOutput("graph_traj", width = "100%", height = "550px")
        ), # End tabPanel


        tabPanel(title = "Rapport",
                 br(),
                 radioButtons(inputId = "lifestyle",
                              h4("Mode de vie de l'espèce"),
                              choices = c("Sédentaire", "Non-sédentaire nicheur", "Non-sédentaire hivernant", "Migrateur de passage")),
                 numericInput(inputId = "wind_farm_nb",
                              h4("Nombre de parcs"),
                              value = 1, min = 0, max = Inf, step = 1),
                 numericInput(inputId = "wind_turbine_nb",
                              h4("Nombre d'éoliennes"),
                              value = 1, min = 0, max = Inf, step = 1)

        ) # End tabPanel

      ) # End tabSetPanel
    )} # End mainPanel

  )} # sidebarLayout

)} # FluidPage

# End UI #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

