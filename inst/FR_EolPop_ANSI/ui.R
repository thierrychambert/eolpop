rm(list = ls(all.names = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial stuff
{
  # Pour vérifier si tous les packages requis sont installés (et les installer sinon)
  list.of.packages <- c("shiny", "shinyjs", "shinyBS", "shinyMatrix", "tidyverse", "SHELF",
                        "promises", "future", "ipc", "popbio", "knitr", "kableExtra", "rmarkdown")
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages) > 0) install.packages(new.packages)


  ## Load libraries
  library(shiny)
  library(shinyjs)
  library(shinyBS)
  library(shinyMatrix)
  library(tidyverse)
  library(eolpop)
  library(popbio)
  library(knitr)
  library(kableExtra)
  library(rmarkdown)

  library(promises)
  library(future)
  library(ipc)
  plan(multisession)

  options(knitr.table.format = "latex")

  ## Load species list
  species_data <- read.csv("species_list.csv", sep = ";")
  species_list <- unique(as.character(species_data$Species_fr)) %>% sort
  species_list <- c(species_list, "Espèce générique")

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
  eli_pop_growth <- c(1.0, -5, -2, 0, 0.95,
                      0.2, -3, 0, 1, 0.90,
                      0.5, -8, -4, -1, 0.90,
                      0.3, -10, -5, -2, 0.70)
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
##  User Interface
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Define User Interface ########################################################
{ui <- navbarPage(
  "EolPop v1.9",

  # Graphic Design Parameters
  header =
    {
      tags$head(
        tags$style(HTML("

body {
    font-family: 'Roboto', Sans-serif!important;
    background-color: rgb(240, 244, 247) !important;
}
.navbar-default {
    padding-right: 40px;
    padding-left: 40px;
    padding-top: 120px;
    background-image: url(https://mape.cnrs.fr/webapp/images/logo-mape-2.png);
    background-size: 600px;
    background-repeat: no-repeat;
    background-position: left top;
}
.navbar-default .navbar-brand {
    font-size: 40px;
    font-family: 'Roboto', Sans-serif;
    font-weight: 300;
    color:#005697 !important;

}
.navbar>.container-fluid .navbar-brand {
    margin-left: -35px;
}
.container-fluid>.navbar-header {
    padding-top: 10px;
}
.navbar-default .navbar-nav>.active>a{
    background-color: #2898f3 !important;
    border-color: #fff;
    color: #fff!important;
    border-radius: 50px;
    margin:10px;
}
.navbar-default .navbar-nav>li>a{
	background-color: #005697 !important;
    border-color: #fff;
    color: #fff!important;
    border-radius: 50px;
    margin:10px;
}
.navbar-default .navbar-nav>li>a:hover{
    background-color: #2898f3 !important;
    border-color: #fff;
    color: #fff!important;
    border-radius: 50px;
    margin:10px;
}
.navbar-default {
	color:#fff;
    background-color: rgb(240, 244, 247) !important;
    border-color:rgb(240, 244, 247)!important;
}
h2{
	width:100%;
	background-color:#77B82A;
	padding:7px 15px 7px 15px;
	border-radius: 15px;
	margin-bottom:40px;
	margin-top:20px;
	font-size: 30px;
  font-family: 'Roboto', Sans-serif;
  font-weight: 300;
  color:#ffffff !important;
}
.footermape{
	box-shadow: 0px 0px 7px 0px rgb(0 0 0 / 10%);
	background-image: url('https://mape.cnrs.fr/webapp/images/logos.png');
	background-position: center;
	background-size: contain;
	background-repeat: no-repeat;
	background-color:#fff;
	margin-bottom:30px!important;
	margin-top:30px!important;
	display: inline-block!important;
    width: 100%;
    height: 130px;
    content:''!important;
    border:15px #fff solid;
    border-radius: 15px;
}
.well {
    margin-bottom: 15px!important;
    background-color: #fff!important;
    border: 1px solid #fff!important;
    border-radius: 15px!important;
    box-shadow: 0px 0px 7px 0px rgb(0 0 0 / 10%)!important;
}
.btn-default{
	background-color: #005697!important;
	border-color: #fff!important;
	color: #fff!important;
    border-radius: 50px!important;
    border-color:#fff!important;
    padding:10px!important;
    margin-bottom:10px!important;
}
.btn:hover {
    background-color: #2898f3!important;
    border-color: #fff!important;
	color: #fff!important;
    border-radius: 50px!important;
}
.btn-group-xs>.btn, .btn-xs{
	background-color:#cfcfcf!important;
	padding:3px 7px 3px 7px!important;
}
.btn-group-xs>.btn, .btn-xs:focus{
	background-color:#cfcfcf!important;
	padding:3px 7px 3px 7px!important;
}
.btn-group-xs>.btn, .btn-xs:hover{
	background-color:#acacac!important;
	padding:3px 7px 3px 7px!important;
}
.nav-tabs {
	border-color:#fff!important;
    background-color:white!important;
    border-radius:15px!important;
	background-color: #e1e1e2!important;
	border: 15px #005697!important;
    margin:15px!important;
    padding:5px!important;
}
.nav-tabs>li {
	background-color: #e1e1e2!important;
	border: 0!important;

}
.nav-tabs>li.active>a {
    color: #fff!important;
    cursor: default!important;
    background-color: #2898f3!important;
    border: 0px!important;
    border-radius:12px!important;
    border-bottom-color: transparent!important;
}
.nav-tabs>li>a {
	border: 0px!important;
    border-radius:12px!important;
    color:#636363!important;
}
.nav-tabs>li>a:hover {
	color: #fff!important;
    background-color: #c7c7c8!important;
    border: 0px!important;
    border-radius:12px!important;
    border-bottom-color: transparent!important;
        color:#636363!important;
}
.nav-tabs>li>a:focus {
	color: #fff!important;
    background-color: #2898f3!important;
    border: 0px!important;
    border-radius:12px!important;
    border-bottom-color: transparent!important;
	color:#fff!important;
}
.tab-content>.active {
    padding:20px!important;
}
hr {
    margin-bottom:40px!important;
    border: none!important;
}
h4{
	font-weight:700!important;
}
.col-sm-8{
	margin-bottom: 40px!important;
    background-color: #fff!important;
    border: 1px solid #fff!important;
    border-radius: 15px!important;
    box-shadow: 0px 0px 7px 0px rgb(0 0 0 / 10%)!important;
    padding:20px!important;
}
.lien{
	background-color: #909090!important;
	border-color: #fff!important;
	color: #fff!important;
    border-radius: 50px!important;
    border-color:#fff!important;
    padding:10px!important;
}

                    "))
      )},

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Tab/Page 1 : About ########################################################
{tabPanel(

  "A propos",


  {wellPanel(

    h4("Présentation"),

    p(HTML("<i>EolPop</i> est un outil permettant de quantifier l'<b>impact démographique</b>
    des mortalités aviaires causées par les collisions avec les éoliennes.<br>
    Il s'agit d'un outil d'aide à la décision, à vocation <i><b>informative</b></i>.
    Il vise à <i>fournir une évaluation objective</i> des conséquences de la mortalités
         aviaire à l'échelle des populations.")),

    br(),

    h5(HTML("Cette application a été développée par
       <a href='https://sites.google.com/view/fr-thierrychambert/home'>Thierry Chambert</a>
       dans le cadre du programme de recherche
            <a href='https://mape.cnrs.fr'>MAPE</a>.")),

    p(strong("Contact: aurelien.besnard@cefe.cnrs.fr")),

    br(),

    p(tags$a(href="https://mape.cnrs.fr/applications/", target="_blank", "Comment utiliser cette application ?", class="lien")),

    br(),

    p(tags$a(href="https://mape.cnrs.fr", target="_blank", "Visiter le site du projet MAPE", class="lien")),



  )}, # End wellpanel #########################################################
  p(class="footermape"),
  p("Avec la participation des opérateurs éoliens partenaires du projet MAPE"),
  br(),
  h5("Mentions légales"),
  p("Propriétaire du site : CNRS, 1919 route de Mende 34090 Montpellier"),
  p("Site internet hébergé par la plateforme SIE du CEFE"),
  br(),
  p(HTML(
    '<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />Cette oeuvre est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Licence Creative Commons Attribution - Pas d&#39;Utilisation Commerciale - Pas de Modification 4.0 International</a>'
  )
  )


)}, # End Page 1 #########################################################



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Tab/Page 2 : Analytical tool #############################################
{tabPanel(
  HTML("Outil démographique <i>EolPop</i>"),

  useShinyjs(),
  titlePanel("EolPop : Impact démographique des collisions aviaires avec les éoliennes"),


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 1 : type of analysis and species
  {wellPanel(
    p("Choix d'analyse et espèce", style="font-size:28px"),

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
                                                   "<b>Impacts non cumulés</b> : pour analyser l\\'impact d\\'<b>un seul parc éolien</b>. <br><br> <b>Impact cumulés</b> : pour analyser l\\'impact de <b>plusieurs parcs éoliens</b> (attention : il faudra fournir des valeurs de mortalités propres à chaque parc).<br><br> <b>Scénarios hypothétiques</b> : permet d'évaluer plusieurs scénarios de mortalités hypothétiques en une seule analyse."
                                                 ),
                                                 placement = "right",
                                                 trigger = "click",
                                                 options = list(container='body')
                                       )
                            ),
                            choices = c("Impacts non cumulés" = "single_farm",
                                        "Impacts cumulés" = "cumulated",
                                        "Scénarios hypothétiques" = "multi_scenario"
                            )
              )},

              # Choose species (selectInput)
              {selectInput(inputId = "species_choice",
                           selected = "Aigle de Bonelli", width = '80%',
                           label = h4(strong("Sélectionner une espèce"),
                                      bsButton("Q_species_choice", label = "", icon = icon("question"), size = "extra-small"),
                                      bsPopover(id = "Q_species_choice",
                                                title = "Choix de l\\'espèce",
                                                content = HTML(
                                                  "Nécessaire pour fixer les valeurs de <b>paramètres démographiques</b> (survie, fécondité). <br><br> La liste fournie correspond à une liste d\\'espèces prioritaires. Au besoin, une option \\'espèce générique\\' est disponible à la fin de la liste."
                                                ),
                                                placement = "right",
                                                trigger = "click",
                                                options = list(container='body')
                                      )
                           ),
                           choices = species_list)},

              br(),
              # Show dispersal distances : mean and d = 5%
              h4(strong("Distances de dispersion"),
                 bsButton("Q_dispersal_info", label = "", icon = icon("question"), size = "extra-small"),
                 bsPopover(id = "Q_dispersal_info",
                           title = "Distances de dispersion",
                           content = HTML(
                             "(1) <b>Distance moyenne de dispersion</b> de l\\'espèce, estimée à partir des relations allométriques publiées dans l\\'article de Claramunt (2021).<br><br> (2) Distance équivalente à un <b>taux de dispersion relatif de 3%, 5% et 10%</b>, sous l\\'hypothèse que la distance de dispersion suit une loi exponentielle.<br><br><u>Reference citée</u> : Claramunt, S. (2021). Flight efficiency explains differences in natal dispersal distances in birds. <i>Ecology</i>, e03442."
                           ),
                           placement = "right",
                           trigger = "click",
                           options = list(container='body')
                 )
              ),
              #br(),
              span(textOutput(outputId = "dispersal_mean_info"), style="font-size:14px"),
              br(),
              span(textOutput(outputId = "dispersal_d03p_info"), style="font-size:12px"),
              span(textOutput(outputId = "dispersal_d05p_info"), style="font-size:12px"),
              span(textOutput(outputId = "dispersal_d10p_info"), style="font-size:12px"),

      )}, # close column

      # Show vital rate values (tableOutput)
      {column(width = 4,
              fluidRow(
                h4(strong("Paramètres démographiques"),
                   bsButton("Q_vital_rates_info", label = "", icon = icon("question"), size = "extra-small"),
                   bsPopover(id = "Q_vital_rates_info",
                             title = "Paramètres démographiques",
                             content = HTML(
                               "Valeurs de <b>survie et fécondités par classe d\\'âge</b>, pour l\\'espèce sélectionnée. <br><br><b>L\\'âge 0</b> (ex : Juv 0) correspond à un individu né dans l\\'année, n\\'ayant <u>pas encore</u> 1 an révolu.<br><b>L\\'âge 1</b> correspond à un individu ayant 1 an révolu, donc dans sa 2<sup>e</sup> année de vie.<br>Etc."
                               #"Valeurs de <b>survie et fécondités par classe d\\'âge</b>, pour l\\'espèce sélectionnée. <br><br><b>Juv 0</b> correspond à un individu né dans l\\'année, n\\'ayant <u>pas encore</u> 1 an révolu.<br><b>Juv 1</b> correspond à un individu ayant 1 an révolu, donc dans sa 2<sup>e</sup> année de vie.<br>Etc."
                             ),
                             placement = "right",
                             trigger = "click",
                             options = list(container='body')
                   )
                ),
                tableOutput(outputId = "vital_rates_info"),

              ), # close fluidRow


              # Display the intrinsic lambda(i.e., based solely on the Leslie matrix)
              # Output display (intrinsic lambda)
              h5(strong("Taux de croissance intrinsèque"),
                 bsButton("Q_lambda0_info", label = "", icon = icon("question"), size = "extra-small"),
                 bsPopover(id = "Q_lambda0_info",
                           title = "Taux de croissance intrinsèque",
                           content = HTML(
                             "Taux de croissance basé seulement sur la matrice de Leslie (survies et fécondités de l\\'espèce), <b> avant considération de la tendance de population locale</b>. <br><br>Ce taux de croissance est fourni simplement à titre informatif. La valeur qui sera utilisée dans les simulations correspond au taux de croissance fourni dans la partie \\'Taux de croissance\\'."
                           ),
                           placement = "right",
                           trigger = "click",
                           options = list(container='body')
                 )
              ),
              div(textOutput(outputId = "lambda0_info", inline = TRUE), style = "color:black; font-size:16px"),

              br(),

              span("* Les valeurs marquées d'une astérisque ne sont pas issues de la littérature.
                  Elles ont été inférées à partir des autres valeurs de paramètres.
                  Il y a donc plus d'incertitude quant à leur exactitude.",
                   style = "color:black; font-size:12px"),


      )}, # close column


      ## Modify vital rates, if needed (actionButton and matrixInput)
      {column(width = 4,
              actionButton(inputId = "button_vital_rates",
                           label = tags$span("Modifier les paramètres démographiques",
                                             style = "font-weight: bold; font-size: 18px;")
              ),

              br(" "),
              numericInput(inputId = "vr_mat_number_age_classes",
                           label = "Nombre de classes d'age",
                           value = 3, min = 2, max = Inf, step = 1),

              #br(),
              matrixInput(inputId = "mat_fill_vr",
                          label = "",
                          value = matrix(data = NA, 3, 2,
                                         dimnames = list(c("Juv 0", "Sub 1", "Adulte"), c("Survie", "Fécondité"))),
                          class = "numeric",
                          rows = list(names = TRUE),
                          cols = list(names = TRUE)
              ),

              actionButton(inputId = "button_use_custom_vr",
                           label = tags$span("Appliquer ces paramètres")
              ),

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

                                {wellPanel(style = "background:#FFF8DC",
                                           radioButtons(inputId = "fatalities_unit", inline = FALSE,
                                                        label = "Unité",
                                                        choices = c("Nombre de mortalités" = "M",
                                                                    "Taux de mortalité (%)" = "h"),
                                                        selected = "M"),
                                )}, # close wellPanel 1

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "fatalities_input_type",
                                                        label = "Type de saisie",
                                                        choices = c("Intervalle" = "itvl",
                                                                    "Valeurs" = "val",
                                                                    "Elicitation d'expert" = "eli_exp"),
                                                        selected = "itvl"),

                                           # Interval
                                           numericInput(inputId = "fatalities_lower",
                                                        label = "Borne inférieure (mortalités annuelles)",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_upper",
                                                        label = "Borne supérieure (mortalités annuelles)",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           # Values
                                           numericInput(inputId = "fatalities_mean",
                                                        label = "Moyenne (mortalités annuelles)",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_se",
                                                        label = "Erreur-type (mortalités annuelles)",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 0.1),

                                           # Matrix for expert elicitation
                                           numericInput(inputId = "fatalities_number_expert",
                                                        label = "Nombre d'experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "fatalities_mat_expert",
                                                       value = matrix(data = eli_fatalities, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "%IC" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "fatalities_run_expert",
                                                        label = "Utiliser valeurs experts"),

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
                                                       value = matrix(c(1, 0, 2011,
                                                                        2, 0, 2012,
                                                                        3, 0, 2013),
                                                                      nrow = 3, ncol = 3, byrow = TRUE,
                                                                      dimnames = list(c(paste0("Parc num.", c(1:3))),
                                                                                      c("Valeur centrale",
                                                                                        "Erreur-type",
                                                                                        "Année (début)"))),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),


                                           ### Part for "scenarios option"
                                           selectizeInput(inputId = "fatalities_vec_scenario",
                                                          label = HTML(
                                                            "Saisir chaque valeur de mortalité<br>
                                               (séparer par un espace)"
                                                          ),
                                                          choices = NULL,
                                                          multiple = TRUE,
                                                          options = list(
                                                            create = TRUE,
                                                            delimiter = ' ',
                                                            create = I("function(input, callback){
                                                              return {
                                                              value: input,
                                                              text: input
                                                            };
                                                          }")
                                                          )
                                           ),






                                )}, # close wellPanel

              )}, # close conditional panel

      )}, # end column "fatalities"

      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  2. Population Size
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

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
                                           radioButtons(inputId = "pop_size_unit", inline = FALSE,
                                                        label = "Unité",
                                                        choices = c("Nombre de couples" = "Npair",
                                                                    "Effectif total" = "Ntotal"),
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
                                                        value = 500,
                                                        min = 0, max = Inf, step = 10),

                                           numericInput(inputId = "pop_size_upper",
                                                        label = "Borne supérieure (taille population)",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 10),

                                           # Values
                                           numericInput(inputId = "pop_size_mean",
                                                        label = "Moyenne de la taille de la population",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 50),

                                           numericInput(inputId = "pop_size_se",
                                                        label = "Erreur-type de la taille de la population",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 1),

                                           # Matrix for expert elicitation
                                           numericInput(inputId = "pop_size_number_expert",
                                                        label = "Nombre d'experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "pop_size_mat_expert",
                                                       value = matrix(data = eli_pop_size, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "%IC" )),
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

      )}, # end column "pop size"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  3. Population Growth
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              actionButton(inputId = "button_pop_growth", width = '100%',
                           label = tags$span("Taux de croissance", style = "font-weight: bold; font-size: 18px;")
              ),
              bsPopover(id = "button_pop_growth",
                        title = "Taux de croissance",
                        content = HTML(
                          "Taux d\\'accroissement annuel de la population <b>en %</b> : valeur positive pour une population en croissance; valeur <b>négative</b> pour une population en <b>déclin</b> (ex : « -4 » pour un déclin de 4% par an) ; 0 pour une population stable.<br><br>A défaut, on pourra juste cocher la <b>tendance globale</b> (déclin, stabilité ou croissance) et l\\'intensité de cette tendance (faible, moyenne, forte)."
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
                                                                    "Valeurs" = "val",
                                                                    "Elicitation d'expert" = "eli_exp",
                                                                    "Tendance population" = "trend")),
                                           # Interval
                                           numericInput(inputId = "pop_growth_lower",
                                                        label = HTML("Borne inférieure<br>(taux d'accroissement en %)"),
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           numericInput(inputId = "pop_growth_upper",
                                                        label = HTML("Borne supérieure<br>(taux d'accroissement en %)"),
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           ## Input values: mean and se
                                           numericInput(inputId = "pop_growth_mean",
                                                        label = "Moyenne (taux d'accroissement en %)",
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           numericInput(inputId = "pop_growth_se",
                                                        label = "Erreur-type (aussi en %)",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 0.5),

                                           ## Input expert elicitation: table
                                           numericInput(inputId = "pop_growth_number_expert",
                                                        label = "Nombre d'experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "pop_growth_mat_expert",
                                                       value = matrix(data = eli_pop_growth, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "%IC" )),
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
                                                                             "En déclin" = "decline")),
                                             ),

                                             # Strength of trend
                                             column(6,
                                                    radioButtons(inputId = "pop_trend_strength",
                                                                 label = "Intensité",
                                                                 choices = c("Faible" = "weak",
                                                                             "Moyenne" = "average",
                                                                             "Forte" = "strong")),
                                             ),
                                           )}, # close fluidRow

                                           br(),
                                           actionButton(inputId = "button_calibrate_vr", label = "Calibrer survies et fécondités"),


                                )}, # close wellPanel

              )}, # close conditional panel

      )}, # end column "pop growth"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  4. Carrying capacity
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              actionButton(inputId = "button_carrying_cap", width = '100%',
                           label = tags$span("Capacité de charge", style = "font-weight: bold; font-size: 18px;")
              ),

              bsPopover(id = "button_carrying_cap",
                        title = "Capacité de charge (K)",
                        content = HTML(
                          "La capacité de charge (K) correspond à la <b>taille maximale que peut atteindre la population</b> dans son environnement et les limites spatiales considérées. <br><br><u>Note:</u> Ce chiffre sera exprimé dans la <b>même unité</b> que la taille de population (cad. nombre de couples ou effectif total). <br>Il n\\'a pas besoin d\\'être très précis&nbsp;; il doit simplement fournir un ordre de grandeur de la taille limite au-delà de laquelle la population ne peut plus croître (environnement local «saturé»)."
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
                                                        choices = c("Intervalle" = "itvl",
                                                                    "Valeur" = "val",
                                                                    "Elicitation d'expert" = "eli_exp",
                                                                    "Absence de K" = "no_K")),

                                           # Interval
                                           numericInput(inputId = "carrying_capacity_lower",
                                                        label = "Borne inférieure (capacité de charge)",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           numericInput(inputId = "carrying_capacity_upper",
                                                        label = "Borne supérieure (capacité de charge)",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           # Values
                                           numericInput(inputId = "carrying_capacity_mean",
                                                        label = "Moyenne de la capacité de charge",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           numericInput(inputId = "carrying_capacity_se",
                                                        label = "Erreur-type de la capacité de charge",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 50),


                                           # Expert Elicitation Matrix
                                           numericInput(inputId = "carrying_cap_number_expert",
                                                        label = "Nombre d'experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "carrying_cap_mat_expert",
                                                       value = matrix(data = eli_carrying_cap, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Poids", "Min", "Best", "Max", "%IC" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "carrying_cap_run_expert", label = "Utiliser valeurs experts"),

                                )}, # close wellPanel 2

              )}, # close conditional panel

      )}, # end column "carrying capacity"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


    )}, # # End fluidRow

  )}, # # End wellPanel
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  {sidebarLayout(

    ##  Side Panel : Parameter information
    {sidebarPanel(

      p("Valeurs sélectionnées", style="font-size:24px",
        bsButton("Q_selected_values", label = "", icon = icon("question"), size = "extra-small"),
        bsTooltip(id = "Q_selected_values",
                  title = "Rappel des valeurs de paramètres actuellement sélectionnées.",
                  placement = "right",
                  trigger = "click",
                  options = list(container='body')
        )
      ),

      # Mortalites annuelles
      span("Mortalités annuelles", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "fatalities_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "fatalities_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "fatalities_se_info"), style="font-size:15px"),

      br(),
      # Taille de population
      span("Taille de la population", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "pop_size_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_size_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_size_se_info"), style="font-size:15px"),

      br(),
      # Tendance de la population
      span(HTML("Taux de croissance (&lambda;)"), style="font-size:18px; font-weight: bold"),
      span(textOutput(outputId = "pop_growth_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_growth_se_info"), style="font-size:15px"),

      br(),
      # Capacite de charge
      span("Capacité de charge", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "carrying_capacity_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "carrying_capacity_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "carrying_capacity_se_info"), style="font-size:15px"),


    )}, # End sidebarPanel


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###  Main Panel

    {mainPanel(
      tabsetPanel(id = NULL, selected = "Impact population",

                  ## Parameter distribution
                  {tabPanel(title = "Distribution paramètres",
                            actionButton("show_ditri_fatalities", label = "Mortalités annuelles"),
                            actionButton("show_ditri_pop_size", label = "Taille de la population"),
                            actionButton("show_ditri_pop_growth", label = "Taux de croissance"),
                            actionButton("show_ditri_carrying_cap", label = "Capacité de charge"),
                            span(textOutput(outputId = "title_distri_plot"), style="font-size:24px; font-weight:bold"),
                            plotOutput(outputId = "distri_plot"),

                  )}, # End tabPanel


                  ## Population Impact : simulations
                  {tabPanel(title = "Impact population",

                            br(),
                            numericInput(inputId = "time_horizon",
                                         label = "Nombre d'années",
                                         value = 30, min = 5, max = Inf, step = 10),

                            br(),
                            numericInput(inputId = "nsim",
                                         label = "Nombre de simulations",
                                         value = 10, min = 0, max = Inf, step = 10),

                            br(),
                            actionButton(inputId = "run", label = "Lancer l'analyse"),
                            actionButton(inputId = "cancel", "Annuler"),
                            actionButton(inputId = "status", "Verifier l'état"),
                            actionButton(inputId = 'clear', 'Effacer les résultats'),

                            br(),
                            span(textOutput("msg_run"), align = "left", style = "font-weight: bold; font-size: 16px;"),

                            br(" "),
                            span(textOutput("run_time"), align = "left", style = "font-weight: normal; font-size: 12px;"),

                            #hr(),
                            #br(),
                            #actionButton(inputId = "show_results", label = "Résultats"),
                            #p("Résultats", style="font-size:28px"),



                  )}, # End tabPanel


                  ## Report
                  {tabPanel(title = "Rapport",
                            br(" "),
                            textAreaInput("intro_report", label = "Contexte de l'étude",
                                          value = "Analyse réalisée dans le cadre de ...", width = "1000px"),

                            br(" "),
                            textAreaInput("def_pop_text", label = "Délimitation de la population",
                                          value = "Veuillez décrire ici l'approche et les informations utilisées pour délimiter la population concernée par cette étude d'impact démographique",
                                          width = "1000px"),

                            br(" "),
                            downloadButton("report", "Produire un rapport")
                  )} # End tabPanel

      ) # End tabSetPanel
    )} # End mainPanel

  )}, # sidebarLayout



  ###############################################################################################################################
  ## RESULTS
  ###############################################################################################################################
  hr(),

  conditionalPanel("output.hide_RES_TITLE",
                   h1("Résultats")
  ),

  #hr(),

  ## Results : text
  {sidebarLayout(

    ##  Side Panel
    {conditionalPanel("output.hide_show_CI",
                      {sidebarPanel(
                        ## Choose CI
                        sliderInput("show_CI", label = "Intervalle de confiance (%)", min = 0, max = 100, value = 95, step = 1),
                        hr(),
                      )}, # End sidebarPanel
    )}, # close conditional panel

    #######################################################

    {conditionalPanel("output.hide_results",
                      {mainPanel(

                        # Table : Global impact
                        {column(
                          width = 7,
                          span(textOutput("title_impact_result"), align = "left", style = "font-weight: bold; font-size: 18px;"),
                          strong(span(tableOutput("impact_table"), style="color:blue; font-size:18px", align = "left")),
                        )},

                        # Table : Probability of extinction
                        {column(
                          width = 5,
                          span(textOutput("title_PrExt_result"), align = "left", style = "font-weight: bold; font-size: 18px;"),
                          strong(span(tableOutput("PrExt_table"), style="color:orange; font-size:18px", align = "left")),
                        )},

                      )}, # End mainPanel
    )}, # close conditional panel


  )}, # sidebarLayout


  ###############################################################################################################################
  #hr(),

  ## Results : Graphs
  {sidebarLayout(

    ##  Side Panel
    {conditionalPanel("output.hide_graph_choice",
                      {sidebarPanel(
                        ##
                        ## Choose which graph to show
                        radioButtons("choose_graph", label = h5(strong("Graphique (choix)")),
                                     choices = c(
                                       "Impact final : densité de probabilité" = "show_PDF",
                                       "Impact final : probabilité cumulée" = "show_ECDF",
                                       "Impact relatif au cours du temps" = "show_impact_time",
                                       "Projections démographiques" = "show_demog_proj"
                                     )),

                        br(),
                        ## Choose "scenario" to show
                        radioButtons("show_scenario", label = h5(strong("Choix du scénario")),
                                     choices = c("all")),

                        br(),
                        hr(),
                        br(),

                        ## Choose "quantile" : level of under-estimation risk (1 - QT)
                        conditionalPanel("output.hide_risk_A",
                                         wellPanel(style = "background:#F0F8FF",
                                                   sliderInput("risk_A", label = "Risque (%) de sous-estimation de l'impact", min = 0, max = 100, value = 5, step = 0.5),

                                                   br(),

                                                   h5(strong("Valeur de l'impact au quantile choisi")),
                                                   #, style = "font-weight: bold; font-size: 18px;")
                                                   span(verbatimTextOutput("quantile_impact_result"), align = "left", style = "font-weight: bold; font-size: 18px;"),
                                         )
                        ), # close conditional panel

                        ##
                      )}, # End sidebarPanel
    )}, # close conditional panel

    #######################################################

    {conditionalPanel("output.hide_graphs",
                      {mainPanel(
                        ## Graph : Probability Density (PDF)
                        {conditionalPanel("output.hide_graph_PDF",
                                          tags$h4(textOutput("title_PDF_plot"), align = "center"),
                                          plotOutput("PDF_plot", width = "100%", height = "550px"),
                        )}, # close conditional panel

                        ## Graph : Cumulative Distibution (ECDF)
                        {conditionalPanel("output.hide_graph_ECDF",
                                          tags$h4(textOutput("title_ECDF_plot"), align = "center"),
                                          plotOutput("ECDF_plot", width = "100%", height = "550px"),
                        )}, # close conditional panel

                        ## Graph : Relative Impact over time
                        {conditionalPanel("output.hide_graph_impact_time",
                                          tags$h4(textOutput("title_impact_plot"), align = "center"),
                                          plotOutput("impact_plot", width = "100%", height = "550px"),
                        )}, # close conditional panel

                        ## Graph : Population trajectory (pop size over time)
                        {conditionalPanel("output.hide_graph_demog_proj",
                                          tags$h4(textOutput("title_traj_plot"), align = "center"),
                                          br(),

                                          wellPanel(
                                            span(textOutput("warning_traj_plot"), align = "left", style = "font-size: 14px;"),
                                          ),

                                          br(" "),
                                          radioButtons(inputId = "age_class_show",
                                                       label = "Classes d'âge à inclure sur le graphe",
                                                       choices = c("Tous âges sauf juvéniles" = "NotJuv0",
                                                                   "Tous âges, y compris juvéniles" = "all",
                                                                   "Nombre de couples" = "pairs"),
                                                       inline = TRUE
                                          ),
                                          plotOutput("traj_plot", width = "100%", height = "550px")
                        )}, # close conditional panel

                      )}, # End mainPanel
    )}, # close conditional panel (of mainpanel)








  )}, # sidebarLayout


  p(class="footermape"),
  p("Avec la participation des opérateurs éoliens partenaires du projet MAPE"),
  br(),
  h5("Mentions légales"),
  p("Propriétaire du site : CNRS, 1919 route de Mende 34090 Montpellier"),
  p("Site internet hébergé par la plateforme SIE du CEFE"),
  br(),
  p(HTML(
    '<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a><br />Cette oeuvre est mise à disposition selon les termes de la <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Licence Creative Commons Attribution - Pas d&#39;Utilisation Commerciale - Pas de Modification 4.0 International</a>'
  )
  )

)} # End Page 2 ###################################################


)} # End "NavBarPage ###################################################

# End UI #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

