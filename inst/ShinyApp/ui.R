rm(list = ls(all.names = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial stuff
{
  ## Load libraries
  library(shiny)
  library(shinyjs)
  library(shinyBS)
  library(shinyMatrix)
  library(tidyverse)
  library(eolpop)
  library(popbio)

  ## Load species list
  species_data <- read.csv("./inst/ShinyApp/species_list.csv", sep = ",")
  species_list <- unique(as.character(species_data$NomEspece))

  ## Load survival and fecundities data
  data_sf <- read.csv("./inst/ShinyApp/survivals_fecundities_species.csv", sep = ",")#, encoding = "UTF-8")
  (data_sf)

  # Fixed parameters (for now)
  nsim = 10
  coeff_var_environ = 0.03
  time_horzion = 30
  theta = 1 # DD parameter theta
  CP = 0.99 # Coverage probability for lower - upper values
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
  # fatality table for cumulated impacts (several wind farms)
  set.seed(seed = 200)
  init_cumul <-
    matrix(
      round(c(
        runif(n = 20, 1, 10),
        runif(n = 20, 0.01, 0.20),
        sort(sample(x = 2000:2050, size = 20, replace = TRUE))
      ), 2),
    nrow = 20, ncol = 3, byrow = FALSE)

  # Undo last 'set.seed'
  set.seed(  ((((Sys.time() %>% as.numeric) %% 1e10) * 1e9) %% 1e5) %>% round  )
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##  User Interface
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# fluidPage
{ui <- fluidPage(

  useShinyjs(),
  titlePanel("eolpop : Impact demographique des collisions aviaires avec les éoliennes"),


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 1 : type of analysis and species
  {wellPanel(
    {fluidRow(

      # Select type of analysis : cumulated impacted or not
      {column(width = 4,
              # Choose analysis type (radioButton)
              {radioButtons(inputId = "analysis_choice",
                           label = h4(strong("Type d'analyse"),
                                      bsButton("Q_analysis_choice", label = "", icon = icon("question"), size = "extra-small"),
                                      bsPopover(id = "Q_analysis_choice",
                                                title = "Choix du type d\\'analyse",
                                                content = HTML(
                                                  "<b>Impacts non cumulés</b> : pour analyser l\\'impact d\\'<b>un seul parc éolien</b>. <br><br> <b>Impact cumulés</b> : pour analyser l\\'impact de <b>plusieurs parcs éoliens</b> (attention : il faudra fournir des valeurs de mortalités propres à chaque parc)."
                                                ),
                                                placement = "right",
                                                trigger = "click",
                                                options = list(container='body')
                                      )
                           ),
                           choices = c("Impacts non cumulés" = "scenario", "Impacts cumulés" = "cumulated")
              )},

              # Choose species (selectInput)
              {selectInput(inputId = "species_choice",
                          selected = "Faucon crécerellette", width = '80%',
                          label = h4(strong("Sélectionner une espèce"),
                                     bsButton("Q_species_choice", label = "", icon = icon("question"), size = "extra-small"),
                                     bsPopover(id = "Q_species_choice",
                                               title = "Choix de l\\'espèce",
                                               content = HTML(
                                                 "Nécessaire pour fixer les valeurs de <b>paramètres démographiques</b> (survie, fécondité). <br> La liste fournie correspond à une liste d\\'espèces prioritaires. Au besoin, une option \\'espèce générique\\' est disponible à la fin de la liste."
                                               ),
                                               placement = "right",
                                               trigger = "click",
                                               options = list(container='body')
                                     )
                          ),
                          choices = species_list)},
      )}, # close column

      # Show vital rate values (tableOutput)
      {column(width = 4,
              fluidRow(
                h4(strong("Paramètres démographiques"),
                   bsButton("Q_vital_rates_info", label = "", icon = icon("question"), size = "extra-small"),
                   bsPopover(id = "Q_vital_rates_info",
                             title = "Paramètres démographiques",
                             content = HTML(
                               "Valeurs de <b>survie et fécondités par classe d\\'âge</b>, pour l\\'espèce sélectionnée. <br><br><b>Juv 0</b> correspond à un individu né dans l\\'année, n\\'ayant <u>pas encore</u> 1 an révolu.<br><b>Juv 1</b> correspond à un individu ayant 1 an révolu, donc dans sa 2<sup>e</sup> année de vie.<br>Etc."
                               ),
                               placement = "right",
                             trigger = "click",
                             options = list(container='body')
                   )
                ),
                tableOutput(outputId = "vital_rates_info"),
              ), # close fluidRow


              # Display the intrinsic lambda(i.e., based solely on the Leslie matrix)
              # NOTE : the first piece(tags$head...) ensures that output is left aligned
              tags$head(
                tags$style(HTML("
                    div.MathJax_Display{
                    text-align: left !important;
                    }
                "))
              ),

              # Output display (intrinsic lambda)
              h5(strong("Taux de croissance intrinsèque")),
              span(uiOutput(outputId = "lambda0_info"), style = "color:black; font-size:16px"),

      )}, # close column


      ## Modify vital rates, if needed (actionButton and matrixInput)
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

    ## Enter parameter values (TITLE)
    {p("Saisie des paramètres", style="font-size:28px",
      bsButton("Q_param_enter", label = "", icon = icon("question"), size = "extra-small"),
      bsPopover(id = "Q_param_enter",
                title = "Saisie des paramètres pour l\\'analyse",
                content = HTML(
                "Cliquer sur les boutons ci-dessous pour saisir les valeurs des quatre paramètres requis pour l\\'analyse : <br>(1) Mortalités annuelles, <br>(2) Taille de la population, <br>(3) Tendance de la population, <br>(4) Capacité de charge."
                ),
                placement = "right",
                trigger = "click",
                options = list(container='body')
      )
    )},

    {fluidRow(

      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  1. Fatalities
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              tags$style(HTML('#button_fatalities{background-color:#C2C8D3}')),
              actionButton(inputId = "button_fatalities", width = '100%',
                           label = tags$span("Mortalités annuelles", style = "font-weight: bold; font-size: 18px;")
              ),
              bsPopover(id = "button_fatalities",
                        title = "Mortalités annuelles",
                        content = HTML(
                        "Nombre de mortalités totales <b><u>annuelles</u> (cad. sur 12 mois) </b> attendues, pour l\\'espèce sélectionnée, sur chaque parc éolien concerné (somme des mortalités attendues sur toutes les éoliennes d\\'un parc)."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
              ),

              ### Part for non-cumulated impacts
              # Input type
              {conditionalPanel("output.hide_fatalities",
                                br(),

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "fatalities_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Intervalle" = "itvl",
                                                                    "Valeurs" = "val",
                                                                    "Elicitation d'expert" = "eli_exp")),

                                           # Interval
                                           numericInput(inputId = "fatalities_lower",
                                                        label = "Borne inférieure (mortalités annuelles)",
                                                        value = 4.9,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_upper",
                                                        label = "Borne supérieure (mortalités annuelles)",
                                                        value = 38.7,
                                                        min = 0, max = Inf, step = 0.5),

                                           # Values
                                           numericInput(inputId = "fatalities_mean",
                                                        label = "Moyenne (mortalités annuelles)",
                                                        value = 2.2,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_se",
                                                        label = "Erreur-type (mortalités annuelles)",
                                                        value = 0.5,
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
                                                       label = span("Mortalités dans chaque parc",
                                                                      bsButton("Q_fatalities_mat_cumulated", label = "", icon = icon("question"), size = "extra-small"),
                                                                      bsPopover(id = "Q_fatalities_mat_cumulated",
                                                                                title = "Mortalités cumulées",
                                                                                content = HTML(
                                                                                  "1 ligne = 1 parc <br><br>Les parcs doivent être fournis dans l\\'<b>ordre chronologique</b> de leur mise en service (\\'Année début\\'). <br><br>Pour chaque parc, veuillez indiquer la <u>moyenne</u> et l\\'<u>erreur-type</u> du nombre de mortalités estimées, ainsi que son <u>année de mise en service</u>."
                                                                                  ),
                                                                                placement = "right",
                                                                                trigger = "click",
                                                                                options = list(container='body')
                                                                      )
                                                       ),
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
              bsPopover(id = "button_pop_size",
                        title = "Taille de la population",
                        content = HTML(
                        "Effectif de la population cible pour l\\'analyse d\\'impact. <br> Il peut s\\'agir soit du <b>nombre de couples</b>, soit de l\\'<b>effectif total</b> de la population (cad. toutes classes d\\'âge incluses)."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
              ),

              {conditionalPanel("output.hide_pop_size",
                              br(),

                              {wellPanel(style = "background:#FFF8DC",
                                 radioButtons(inputId = "pop_size_unit", inline = TRUE,
                                              label = "Unité",
                                              choices = c("Nombre de couples" = "Npair", "Effectif total" = "Ntotal"),
                                              selected = "Npair"),
                              )}, # close wellPanel 1

                              {wellPanel(style = "background:#F0F8FF",

                                         radioButtons(inputId = "pop_size_input_type",
                                                      label = "Type de saisie",
                                                      choices = c("Intervalle" = "itvl",
                                                                  "Valeurs" = "val",
                                                                  "Elicitation d'expert" = "eli_exp")),

                                         # Interval
                                         numericInput(inputId = "pop_size_lower",
                                                      label = "Borne inférieure (taille population)",
                                                      value = 220,
                                                      min = 0, max = Inf, step = 10),

                                         numericInput(inputId = "pop_size_upper",
                                                      label = "Borne supérieure (taille population)",
                                                      value = 230,
                                                      min = 0, max = Inf, step = 10),

                                         # Values
                                         numericInput(inputId = "pop_size_mean",
                                                      label = "Moyenne de la taille de la population",
                                                      value = 200,
                                                      min = 0, max = Inf, step = 50),

                                         numericInput(inputId = "pop_size_se",
                                                      label = "Erreur-type de la taille de la population",
                                                      value = 25,
                                                      min = 0, max = Inf, step = 1),

                                         # Matrix for expert elicitation
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


                              # Display matrix for stable age distribution
                              h5(strong("Effectifs par classe d'âge")),
                              tableOutput("pop_size_by_age"),

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
              bsPopover(id = "button_pop_growth",
                        title = "Tendance de la population",
                        content = HTML(
                        "<b>Taux de croissance annuel (&lambda;) </b> de la population (avec &lambda; = 1 pour une population stable). <br>A défaut, on pourra juste cocher la <b>tendance globale</b> (déclin, stabilité ou croissance) et l\\'intensité de cette tendance (faible, moyenne, forte)."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
              ),

              {conditionalPanel("output.hide_pop_growth",
                                br(),

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "pop_growth_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Intervalle" = "itvl",
                                                                    "Taux d'accroissement" = "val",
                                                                    "Elicitation d'expert" = "eli_exp",
                                                                    "Tendance locale ou régionale" = "trend")),
                                           # Interval
                                           numericInput(inputId = "pop_growth_lower",
                                                        label = "Borne inférieure (taux d'accroissement)",
                                                        value = 1.05,
                                                        min = 0, max = Inf, step = 0.01),

                                           numericInput(inputId = "pop_growth_upper",
                                                        label = "Borne supérieure (taux d'accroissement)",
                                                        value = 1.10,
                                                        min = 0, max = Inf, step = 0.01),

                                           ## Input values: mean and se
                                           numericInput(inputId = "pop_growth_mean",
                                                        label = "Moyenne (taux d'accroissement)",
                                                        value = 0.99,
                                                        min = 0, max = Inf, step = 0.01),

                                           numericInput(inputId = "pop_growth_se",
                                                        label = "Erreur-type (taux d'accroissement)",
                                                        value = 0,
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

              bsPopover(id = "button_carrying_cap",
                        title = "Capacité de charge",
                        content = HTML(
                          "La capacité de charge correspond à la <b>taille maximale que peut atteindre la population</b> dans son environnement et les limites spatiales considérées. <br><br><u>Note:</u> Ce chiffre sera exprimé dans la <b>même unité</b> que la taille de population (cad. nombre de couples ou effectif total). <br>Il n\\'a pas besoin d\\'être très précis&nbsp;; il doit simplement fournir un ordre de grandeur de la taille limite au-delà de laquelle la population ne peut plus croître (environnement local «saturé»)."
                          ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
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
                                                        value = 1000,
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

      p("Valeurs sélectionnées", style="font-size:28px",
        bsButton("Q_selected_values", label = "", icon = icon("question"), size = "extra-small"),
        bsTooltip(id = "Q_selected_values",
                  title = "Rappel des valeurs de paramètres actuellement sélectionnées.",
                  placement = "right",
                  trigger = "click",
                  options = list(container='body')
        )
      ),

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

                 br(),
                 numericInput(inputId = "nsim",
                              label = "Nombre de simulations",
                              value = 10, min = 0, max = Inf, step = 10),

                 radioButtons(inputId = "fatal_constant",
                              label = "Modélisation",
                              choices = c("Taux de mortalités (h) constant" = "h",
                                          "Nombre de mortalités (M) constant" = "M")),

                 br(),

                 actionButton(inputId = "run", label = "Lancer l'analyse"),
                 hr(),

                 span(textOutput("title_impact_result"), align = "left", style = "font-weight: bold; font-size: 18px;"),
                 br(),
                 strong(span(textOutput("impact_text"), style="color:blue; font-size:18px", align = "left")),
                 strong(span(tableOutput("impact_table"), style="color:blue; font-size:18px", align = "left")),
                 br(),

                 hr(),

                 tags$h4(textOutput("title_impact_plot"), align = "center"),
                 plotOutput("impact_plot", width = "100%", height = "550px"),
                 hr(),

                 tags$h4(textOutput("title_traj_plot"), align = "center"),
                 plotOutput("traj_plot", width = "100%", height = "550px")
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

