rm(list = ls(all.names = TRUE))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initial stuff
{
  # Check if all required packages are laready installad (and install missing ones)
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
  species_list <- unique(as.character(species_data$Species_en)) %>% sort
  species_list <- c(species_list, "Generic species")

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
  "EolPop (en) v1.6",

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

  "About",


  {wellPanel(

    h4("Overview"),

    p(HTML("<i>EolPop</i> is a tool to quantify the <b> demographic impact</b>
    of avian fatalities caused by wind turbine collisions.<br>
    It is an <i><b>informative</b></i> tool that can be used to falicitate decisions.
    It aims to <i>provide an objective assessment</i> of the consequences of
    avian fatalities at the scale of a population.")),

    br(),

    h5(HTML("This application was developed by
       <a href='https://sites.google.com/view/fr-thierrychambert/home'>Thierry Chambert</a>
       in the framework of the
       <a href='https://mape.cnrs.fr'>MAPE</a>
             research program.")),

    p(strong("Contact: aurelien.besnard@cefe.cnrs.fr")),

    br(),

    p(tags$a(href="https://mape.cnrs.fr/applications/", target="_blank",
             "How to use this application ?", class="lien")),

    br(),

    p(tags$a(href="https://mape.cnrs.fr",
             target="_blank", "Visit the website of the MAPE project", class="lien")),



  )}, # End wellpanel #########################################################
  p(class="footermape"),
  p("With the participation of the wind power operators which are partners of the MAPE project"),
  br(),
  h5("Terms of use"),
  p("This webiste is the property of : CNRS, 1919 route de Mende 34090 Montpellier"),
  p("This webiste is hosted by the SIE platform of the CEFE"),
  br(),
  p(HTML(
    '<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a>
    <br />This work is made available under the terms of the <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Licence Creative Commons Attribution - Non Commercial - No Derivatives 4.0 International</a>'
  )
  )


)}, # End Page 1 #########################################################



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Tab/Page 2 : Analytical tool #############################################
{tabPanel(
  HTML("Demographic tool <i>EolPop</i>"),

  useShinyjs(),
  titlePanel("EolPop : Demographic impact of bird collisions with wind turbines"),


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 1 : type of analysis and species
  {wellPanel(
    p("Choose type of analysis and species", style="font-size:28px"),

    {fluidRow(

      # Select type of analysis : cumulated impacted or not
      {column(width = 4,
              # Choose analysis type (radioButton)
              {radioButtons(inputId = "analysis_choice",
                            label = h4(strong("Type of analysis"),
                                       bsButton("Q_analysis_choice", label = "", icon = icon("question"), size = "extra-small"),
                                       bsPopover(id = "Q_analysis_choice",
                                                 title = "Choose type of analysis",
                                                 content = HTML(
                                                   "<b>Single wind farm</b> : analyze the impact of <b>a single wind farm</b>. <br><br> <b>Cumulative impacts</b> : analyze the impact of <b>several wind farms</b> on the same population (one value of fatality per each farm is required). <br><br> <b>Hypothetical scenarios</b> : allows assessing several hypothetical scenarios of mortality in a single analysis."
                                                 ),
                                                 placement = "right",
                                                 trigger = "click",
                                                 options = list(container='body')
                                       )
                            ),
                            choices = c("Single wind farm" = "single_farm",
                                        "Cumulative impacts" = "cumulated",
                                        "Hypothetical scenarios" = "multi_scenario"
                            )
              )},

              # Choose species (selectInput)
              {selectInput(inputId = "species_choice",
                           selected = "Bonelli's Eagle", width = '80%',
                           label = h4(strong("Select a species"),
                                      bsButton("Q_species_choice", label = "", icon = icon("question"), size = "extra-small"),
                                      bsPopover(id = "Q_species_choice",
                                                title = "Choose the species",
                                                content = HTML(
                                                  "Required to populate <b>vital rate</b> values (survivals, fecundities). <br><br> The species provided here correspond to the priority list of the MAPE project. If needed, an option \\'generic species\\' is available at the end of the list."
                                                ),
                                                placement = "right",
                                                trigger = "click",
                                                options = list(container='body')
                                      )
                           ),
                           choices = species_list)},

              br(),
              # Show dispersal distances : mean and d = 5%
              h4(strong("Dispersal distances"),
                 bsButton("Q_dispersal_info", label = "", icon = icon("question"), size = "extra-small"),
                 bsPopover(id = "Q_dispersal_info",
                           title = "Dispersal distances",
                           content = HTML(
                             "(1) <b>Mean distance of dispersal</b> of the species, estimated using allometric relationships published in the article of Claramunt (2021).<br><br> (2) Distance equivalent to a <b> relative dispersal rate of 3%, 5% et 10%</b>, assuming that dispersal distances follow an exponential distribution.<br><br><u>Reference</u> : Claramunt, S. (2021). Flight efficiency explains differences in natal dispersal distances in birds. <i>Ecology</i>, e03442."
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
                h4(strong("Vital rates"),
                   bsButton("Q_vital_rates_info", label = "", icon = icon("question"), size = "extra-small"),
                   bsPopover(id = "Q_vital_rates_info",
                             title = "Vital rates",
                             content = HTML(
                               "Values of <b>survival and fecundities for each age class</b>, for the selected species. <br><br><b>Age 0</b> (ex : Juv 0) corresponds to an individual born within the year (< 1 year old).<br><b>Age 1</b> corresponds to an individual of 1 year old, which is thus in its 2<sup>nd</sup> year of life.<br>Etc."
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
              h5(strong("Intrinsic population growth rate"),
                 bsButton("Q_lambda0_info", label = "", icon = icon("question"), size = "extra-small"),
                 bsPopover(id = "Q_lambda0_info",
                           title = "Intrinsic population growth rate",
                           content = HTML(
                             "Population growth rate value solely based on the Leslie matrix (survivals et fecundities of the species), <b> before considering the local population trend</b>. <br><br>This growth rate is provided only for information. The value that will be used in the simulations corresponds to the growth rate provided in the section (below) called \\'Growth rate\\'."
                           ),
                           placement = "right",
                           trigger = "click",
                           options = list(container='body')
                 )
              ),
              div(textOutput(outputId = "lambda0_info", inline = TRUE), style = "color:black; font-size:16px"),

              br(),

              span("* Values marked with an asterisk do not come from the literature.
                  They were inferred using estimated relashionships based on other vital rates and species mass and age at first reproduction.
                  Therefore, they carry more uncertainty.",
                   style = "color:black; font-size:12px"),


      )}, # close column


      ## Modify vital rates, if needed (actionButton and matrixInput)
      {column(width = 4,
              actionButton(inputId = "button_vital_rates",
                           label = tags$span("Modify vital rate values",
                                             style = "font-weight: bold; font-size: 18px;")
              ),

              br(" "),
              numericInput(inputId = "vr_mat_number_age_classes",
                           label = "Number of age classes",
                           value = NA, min = 2, max = Inf, step = 1),

              #br(),
              matrixInput(inputId = "mat_fill_vr",
                          label = "",
                          value = matrix(data = NA, 3, 2,
                                         dimnames = list(c("Juv 0", "Sub 1", "Adult"), c("Survival", "Fecundity"))),
                          class = "numeric",
                          rows = list(names = TRUE),
                          cols = list(names = TRUE)
              ),

              actionButton(inputId = "button_use_custom_vr",
                           label = tags$span("Apply these values")
              ),

      )}, # close column

    )}, # End fluidRow
  )}, # End wellPanel
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  # Head Panel 2 : Model parameters
  {wellPanel(

    ## Enter parameter values (TITLE)
    {p("Parameter entry", style="font-size:28px",
       bsButton("Q_param_enter", label = "", icon = icon("question"), size = "extra-small"),
       bsPopover(id = "Q_param_enter",
                 title = "Parameter entry for the analysis",
                 content = HTML(
                   "Click on the buttons below to enter values for the four parameters required for the analysis: <br>(1) Annual fatalities, <br>(2) Population size, <br>(3) Population growth rate, <br>(4) Carrying capacity."
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
                           label = tags$span("Annual fatalities", style = "font-weight: bold; font-size: 18px;")
              ),
              bsPopover(id = "button_fatalities",
                        title = "Annual fatalities",
                        content = HTML(
                          "Expected number of fatalities occurring <b><u>annually</u> (i.e. over 12 months) </b>, for the selected species, on each wind farm (i.e. the sum of fatalities across all the wind turbines of a given wind farm)."
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
                                                        label = "Unit",
                                                        choices = c("Number of fatalities" = "M",
                                                                    "Mortality rate (%)" = "h"),
                                                        selected = "M"),
                                )}, # close wellPanel 1

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "fatalities_input_type",
                                                        label = "Type of entry",
                                                        choices = c("Interval" = "itvl",
                                                                    "Values" = "val",
                                                                    "Expert elicitation" = "eli_exp"),
                                                        selected = "itvl"),

                                           # Interval
                                           numericInput(inputId = "fatalities_lower",
                                                        label = "Lower bound",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_upper",
                                                        label = "Upper bound",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           # Values
                                           numericInput(inputId = "fatalities_mean",
                                                        label = "Mean",
                                                        value = 1,
                                                        min = 0, max = Inf, step = 0.5),

                                           numericInput(inputId = "fatalities_se",
                                                        label = "Standard error",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 0.1),

                                           # Matrix for expert elicitation
                                           numericInput(inputId = "fatalities_number_expert",
                                                        label = "Number of experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "fatalities_mat_expert",
                                                       value = matrix(data = eli_fatalities, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Weight", "Min", "Best", "Max", "%CI" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "fatalities_run_expert",
                                                        label = "Use experts values"),


                                           ### Part for cumulated impacts

                                           numericInput(inputId = "farm_number_cumulated",
                                                        label = "Number of wind farms",
                                                        value = 3, min = 2, max = Inf, step = 1),

                                           matrixInput(inputId = "fatalities_mat_cumulated",
                                                       label = span("Fatalities on each wind farm",
                                                                    bsButton("Q_fatalities_mat_cumulated", label = "", icon = icon("question"), size = "extra-small"),
                                                                    bsPopover(id = "Q_fatalities_mat_cumulated",
                                                                              title = "Cumulatives fatalities",
                                                                              content = HTML(
                                                                                "1 row = 1 wind farm <br><br>Wind farms must be provided by <b>commissioning chronological order</b>(\\'Start year\\'). <br><br>For each wind farm, please prvide the <u>mean</u> and <u>standard error</u> of the estimated number (or rate) of fatalities, as well as its <u>year of commissioning</u>."
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
                                                                      dimnames = list(c(paste0("Farm num.", c(1:3))),
                                                                                      c("Mean",
                                                                                        "Standard Error",
                                                                                        "Start year"))),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),


                                           ### Part for "scenarios option"
                                           selectizeInput(inputId = "fatalities_vec_scenario",
                                                          label = HTML(
                                                            "Enter each value of mortality<br>
                                               (separate by a space)"
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
                           label = tags$span("Population size", style = "font-weight: bold; font-size: 18px;")
              ),
              bsPopover(id = "button_pop_size",
                        title = "Population size",
                        content = HTML(
                          "Size of the target population for the impact assessment. <br> Population size can be provided either as a <b>number of reproductive pairs</b>, or as the <b>total headcount</b> (i.e. all age classes included)."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
              ),

              {conditionalPanel("output.hide_pop_size",
                                br(),

                                {wellPanel(style = "background:#FFF8DC",
                                           radioButtons(inputId = "pop_size_unit", inline = FALSE,
                                                        label = "Unit",
                                                        choices = c("Numbre of pairs" = "Npair",
                                                                    "Total headcount" = "Ntotal"),
                                                        selected = "Npair"),
                                )}, # close wellPanel 1

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "pop_size_input_type",
                                                        label = "Type of entry",
                                                        choices = c("Interval" = "itvl",
                                                                    "Values" = "val",
                                                                    "Expert elicitation" = "eli_exp")),

                                           # Interval
                                           numericInput(inputId = "pop_size_lower",
                                                        label = "Lower bound",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 10),

                                           numericInput(inputId = "pop_size_upper",
                                                        label = "Upper bound",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 10),

                                           # Values
                                           numericInput(inputId = "pop_size_mean",
                                                        label = "Mean",
                                                        value = 500,
                                                        min = 0, max = Inf, step = 50),

                                           numericInput(inputId = "pop_size_se",
                                                        label = "Standard error",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 1),

                                           # Matrix for expert elicitation
                                           numericInput(inputId = "pop_size_number_expert",
                                                        label = "Number of experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "pop_size_mat_expert",
                                                       value = matrix(data = eli_pop_size, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Weight", "Min", "Best", "Max", "%CI" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "pop_size_run_expert",
                                                        label = "Use experts values"),
                                )}, # close wellPanel 2


                                # Display matrix for stable age distribution
                                h5(strong("Age class sizes")),
                                tableOutput("pop_size_by_age"),

              )}, # close conditional panel

      )}, # end column "pop size"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  3. Population Growth
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              actionButton(inputId = "button_pop_growth", width = '100%',
                           label = tags$span("Growth rate", style = "font-weight: bold; font-size: 18px;")
              ),
              bsPopover(id = "button_pop_growth",
                        title = "Growth rate",
                        content = HTML(
                          "Annual rate of population increase (<b>%</b>) : use a positive value for a growing population ; use a <b>negative</b> value for a <b>declining</b> population (e.g. : « -4 » for a 4% annual decline) ; 0 for a stable population.<br><br>As an alternative, you may simply pick the <b>overall trend</b> (decline, stability or growth) and the intensity of that trend (low, medium, high), under the option \\'Population trend\\'."
                        ),
                        placement = "top",
                        trigger = "hover",
                        options = list(container='body')
              ),

              {conditionalPanel("output.hide_pop_growth",
                                br(),

                                {wellPanel(style = "background:#F0F8FF",

                                           radioButtons(inputId = "pop_growth_input_type",
                                                        label = "Type of entry",
                                                        choices = c("Interval" = "itvl",
                                                                    "Values" = "val",
                                                                    "Expert elicitation" = "eli_exp",
                                                                    "Population trend" = "trend")),
                                           # Interval
                                           numericInput(inputId = "pop_growth_lower",
                                                        label = HTML("Lower bound (%)"),
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           numericInput(inputId = "pop_growth_upper",
                                                        label = HTML("Upper bound (%)"),
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           ## Input values: mean and se
                                           numericInput(inputId = "pop_growth_mean",
                                                        label = "Mean (%)",
                                                        value = -2,
                                                        min = -100, max = Inf, step = 1),

                                           numericInput(inputId = "pop_growth_se",
                                                        label = "Standard error (%)",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 0.5),

                                           ## Input expert elicitation: table
                                           numericInput(inputId = "pop_growth_number_expert",
                                                        label = "Number of experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "pop_growth_mat_expert",
                                                       value = matrix(data = eli_pop_growth, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Weight", "Min", "Best", "Max", "%CI" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "pop_growth_run_expert",
                                                        label = "Use experts values"),

                                           ## Input trend: radio buttons
                                           {fluidRow(
                                             # Trend
                                             column(6,
                                                    radioButtons(inputId = "pop_trend",
                                                                 label = "Trend",
                                                                 choices = c("Growth" = "growth",
                                                                             "Stable" = "stable",
                                                                             "Decline" = "decline")),
                                             ),

                                             # Strength of trend
                                             column(6,
                                                    radioButtons(inputId = "pop_trend_strength",
                                                                 label = "Intensity",
                                                                 choices = c("Low" = "weak",
                                                                             "Medium" = "average",
                                                                             "High" = "strong")),
                                             ),
                                           )}, # close fluidRow

                                           br(),
                                           actionButton(inputId = "button_calibrate_vr", label = "Calibrate survivals and fecundities"),


                                )}, # close wellPanel

              )}, # close conditional panel

      )}, # end column "pop growth"
      ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      ##  4. Carrying capacity
      ##~~~~~~~~~~~~~~~~~~~~~~~~~
      {column(width = 3,

              actionButton(inputId = "button_carrying_cap", width = '100%',
                           label = tags$span("Carrying capacity", style = "font-weight: bold; font-size: 18px;")
              ),

              bsPopover(id = "button_carrying_cap",
                        title = "Carrying capacity (K)",
                        content = HTML(
                          "The carrying capacity (K) corresponds to the <b>maximum size</b> that the population can reach in its environment and the spatial limits considered.<br><br><u>Note:</u> This figure should be expressed in the <b>same unit</b> as the population size (i.e., number of pairs or total population size). <br>It does not need to be very precise. It should simply provide an order of magnitude of the size limit beyond which the population can no longer grow (local \\'saturated\\' environment)."
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
                                                        label = "Type of entry",
                                                        choices = c("Interval" = "itvl",
                                                                    "Value" = "val",
                                                                    "Expert elicitation" = "eli_exp",
                                                                    "No carrying capacity" = "no_K")),

                                           # Interval
                                           numericInput(inputId = "carrying_capacity_lower",
                                                        label = "Lower bound",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           numericInput(inputId = "carrying_capacity_upper",
                                                        label = "Upper bound",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           # Values
                                           numericInput(inputId = "carrying_capacity_mean",
                                                        label = "Mean",
                                                        value = 1000,
                                                        min = 0, max = Inf, step = 100),

                                           numericInput(inputId = "carrying_capacity_se",
                                                        label = "Standard error",
                                                        value = 0,
                                                        min = 0, max = Inf, step = 50),


                                           # Expert Elicitation Matrix
                                           numericInput(inputId = "carrying_cap_number_expert",
                                                        label = "Number of experts",
                                                        value = 4, min = 1, max = Inf, step = 1),

                                           matrixInput(inputId = "carrying_cap_mat_expert",
                                                       value = matrix(data = eli_carrying_cap, nrow = 4, ncol = 5,
                                                                      dimnames = list(c("#1", "#2", "#3", "#4"),
                                                                                      c("Weight", "Min", "Best", "Max", "%CI" )),
                                                                      byrow = TRUE),
                                                       class = "numeric",
                                                       rows = list(names = TRUE),
                                                       cols = list(names = TRUE)),

                                           actionButton(inputId = "carrying_cap_run_expert",
                                                        label = "Use experts values"),

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

      p("Chosen values", style="font-size:24px",
        bsButton("Q_selected_values", label = "", icon = icon("question"), size = "extra-small"),
        bsTooltip(id = "Q_selected_values",
                  title = "Parameter values currently active.",
                  placement = "right",
                  trigger = "click",
                  options = list(container='body')
        )
      ),

      # Mortalites annuelles
      span("Annual fatalities", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "fatalities_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "fatalities_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "fatalities_se_info"), style="font-size:15px"),

      br(),
      # Taille de population
      span("Population size", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "pop_size_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_size_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_size_se_info"), style="font-size:15px"),

      br(),
      # Tendance de la population
      span(HTML("Population growth rate (&lambda;)"), style="font-size:18px; font-weight: bold"),
      span(textOutput(outputId = "pop_growth_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "pop_growth_se_info"), style="font-size:15px"),

      br(),
      # Capacite de charge
      span("Carrying capacity", style="font-size:18px; font-weight: bold"),
      shiny::tags$i(textOutput(outputId = "carrying_capacity_unit_info"), style="font-size:15px"),
      span(textOutput(outputId = "carrying_capacity_mean_info"), style="font-size:15px"),
      span(textOutput(outputId = "carrying_capacity_se_info"), style="font-size:15px"),


    )}, # End sidebarPanel


    #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ###  Main Panel

    {mainPanel(
      tabsetPanel(id = NULL, selected = "Population impact",

                  ## Parameter distribution
                  {tabPanel(title = "Parameters distribution",
                            actionButton("show_ditri_fatalities", label = "Annual fatalities"),
                            actionButton("show_ditri_pop_size", label = "Population size"),
                            actionButton("show_ditri_pop_growth", label = "Growth rate"),
                            actionButton("show_ditri_carrying_cap", label = "Carrying capacity"),
                            span(textOutput(outputId = "title_distri_plot"), style="font-size:24px; font-weight:bold"),
                            plotOutput(outputId = "distri_plot"),

                  )}, # End tabPanel


                  ## Population Impact : simulations
                  {tabPanel(title = "Population impact",

                            br(),
                            numericInput(inputId = "time_horizon",
                                         label = "Number of years",
                                         value = 30, min = 5, max = Inf, step = 10),

                            br(),
                            numericInput(inputId = "nsim",
                                         label = "Number of simulations",
                                         value = 10, min = 0, max = Inf, step = 10),

                            br(),
                            actionButton(inputId = "run", label = "Run"),
                            actionButton(inputId = "cancel", "Cancel"),
                            actionButton(inputId = "status", "Check status"),
                            actionButton(inputId = 'clear', 'Clear results'),

                            br(),
                            span(textOutput("msg_run"), align = "left", style = "font-weight: bold; font-size: 16px;"),

                            br(" "),
                            span(textOutput("run_time"), align = "left", style = "font-weight: normal; font-size: 12px;"),


                  )}, # End tabPanel


                  ## Report
                  {tabPanel(title = "Report",
                            br(" "),
                            textAreaInput("intro_report", label = "Context of the study",
                                          value = "Analysis performed as part of...", width = "1000px"),

                            br(" "),
                            textAreaInput("def_pop_text", label = "Population delineation",
                                          value = "Please describe here the approach and information used to delineate the population of interest for this demographic impact study",
                                          width = "1000px"),

                            br(" "),
                            downloadButton("report", "Build report")
                  )} # End tabPanel

      ) # End tabSetPanel
    )} # End mainPanel

  )}, # sidebarLayout



  ###############################################################################################################################
  ## RESULTS
  ###############################################################################################################################
  hr(),

  conditionalPanel("output.hide_RES_TITLE",
                   h1("Results")
  ),

  #hr(),

  ## Results : text
  {sidebarLayout(

    ##  Side Panel
    {conditionalPanel("output.hide_show_CI",
                      {sidebarPanel(
                        ## Choose CI
                        sliderInput("show_CI", label = "Confidence Interval (%)", min = 0, max = 100, value = 95, step = 1),
                        hr(),
                      )}, # End sidebarPanel
    )}, # close conditional panel

    #######################################################

    {conditionalPanel("output.hide_results",
                      {mainPanel(

                        # Table : Global impact
                        {column(
                          width = 6,
                          span(textOutput("title_impact_result"), align = "left", style = "font-weight: bold; font-size: 18px;"),
                          strong(span(tableOutput("impact_table"), style="color:blue; font-size:18px", align = "left")),
                        )},

                        # Table : Probability of extinction
                        {column(
                          width = 6,
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
                        radioButtons("choose_graph", label = h5(strong("Plot to display")),
                                     choices = c(
                                       "Final impact: probability density" = "show_PDF",
                                       "Final impact: cumulative probability" = "show_ECDF",
                                       "Relative impact across time" = "show_impact_time",
                                       "Population projections" = "show_demog_proj"
                                     )),

                        br(),
                        ## Choose "scenario" to show
                        radioButtons("show_scenario", label = h5(strong("Choose scenario to show")),
                                     choices = c("all")),

                        br(),
                        hr(),
                        br(),

                        ## Choose "quantile" : level of under-estimation risk (1 - QT)
                        conditionalPanel("output.hide_risk_A",
                                         wellPanel(style = "background:#F0F8FF",
                                                   sliderInput("risk_A", label = "Risk (%) of under-estimation of the impact", min = 0, max = 100, value = 5, step = 0.5),

                                                   br(),

                                                   h5(strong("Value of impact at the chosen quantile")),
                                                   #, style = "font-weight: bold; font-size: 18px;")
                                                   span(verbatimTextOutput("quantile_impact_result"), align = "left",
                                                        style = "font-weight: bold; font-size: 18px;"),
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
                                                       label = "Age classes to be included on the plot",
                                                       choices = c("All ages except juveniles" = "NotJuv0",
                                                                   "All ages, including juveniles" = "all",
                                                                   "Number of pairs" = "pairs"),
                                                       inline = TRUE
                                          ),
                                          plotOutput("traj_plot", width = "100%", height = "550px")
                        )}, # close conditional panel

                      )}, # End mainPanel
    )}, # close conditional panel (of mainpanel)








  )}, # sidebarLayout

  p(class="footermape"),
  p("With the participation of the wind power operators which are partners of the MAPE project"),
  br(),
  h5("Terms of use"),
  p("This webiste is the property of : CNRS, 1919 route de Mende 34090 Montpellier"),
  p("This webiste is hosted by the SIE platform of the CEFE"),
  br(),
  p(HTML(
    '<a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/"><img alt="Licence Creative Commons" style="border-width:0" src="https://i.creativecommons.org/l/by-nc-nd/4.0/88x31.png" /></a>
    <br />This work is made available under the terms of the <a rel="license" href="http://creativecommons.org/licenses/by-nc-nd/4.0/">Licence Creative Commons Attribution - Non Commercial - No Derivatives 4.0 International</a>'
  )
  )


)} # End Page 2 ###################################################


)} # End "NavBarPage ###################################################

# End UI #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

