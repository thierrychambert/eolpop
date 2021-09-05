server <- function(input, output, session){

  ##############################################
  ##  Hide/Show : level 1
  ##--------------------------------------------

  ## Fatalities
  output$hide_fatalities <- eventReactive({
    input$button_fatalities
  },{
    if(input$button_fatalities%%2 == 1) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_fatalities", suspendWhenHidden = FALSE)


  ## Population Size
  output$hide_pop_size <- eventReactive({
    input$button_pop_size
  },{
    if(input$button_pop_size%%2 == 1) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_pop_size", suspendWhenHidden = FALSE)


  ## Population Growth
  output$hide_pop_growth <- eventReactive({
    input$button_pop_growth
  },{
    if(input$button_pop_growth%%2 == 1) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_pop_growth", suspendWhenHidden = FALSE)


  ## Carrying capacity
  output$hide_carrying_cap <- eventReactive({
    input$button_carrying_cap
  },{
    if(input$button_carrying_cap%%2 == 1) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_carrying_cap", suspendWhenHidden = FALSE)

  # Display Carrying capacity Unit Info
  output$carrying_cap_unit_info <- renderText({
    if(input$pop_size_unit == "Npair"){
      paste0("Nombre de couple")
    } else {
      paste0("Effectif total")
    }
  })



  ##############################################
  ##  Hide/Show : level 2
  ##--------------------------------------------
  observe({

    #------------
    # Hide all
    #------------
    #shinyjs::hide("fatal_constant")
    #shinyjs::hide("fatalities_input_type")
    shinyjs::hide("fatalities_mean")
    shinyjs::hide("fatalities_se")
    shinyjs::hide("fatalities_mat_expert")
    shinyjs::hide("fatalities_run_expert")
    shinyjs::hide("farm_number_cumulated")
    shinyjs::hide("fatalities_mat_cumulated")

    #shinyjs::hide("pop_size_unit")
    #shinyjs::hide("pop_size_input_type")
    shinyjs::hide("pop_size_mean")
    shinyjs::hide("pop_size_se")
    shinyjs::hide("pop_size_mat_expert")
    shinyjs::hide("pop_size_run_expert")

    #shinyjs::hide("pop_growth_input_type")
    shinyjs::hide("pop_growth_mean")
    shinyjs::hide("pop_growth_se")
    shinyjs::hide("pop_growth_mat_expert")
    shinyjs::hide("pop_growth_run_expert")
    shinyjs::hide("pop_trend")
    shinyjs::hide("pop_trend_strength")


    #shinyjs::hide("carrying_cap_input_type")
    shinyjs::hide("carrying_capacity")
    shinyjs::hide("carrying_cap_mat_expert")
    shinyjs::hide("carrying_cap_run_expert")

    shinyjs::hide("mat_fill_vr")

    #------------
    # Show some
    #------------
    # Show inputs for fatalities part
    if(input$button_fatalities%%2 == 1){
      #shinyjs::show("fatal_constant")

      # Show inputs for none cumulated impacts scenario

      if(input$analysis_choice == "scenario"){
        shinyjs::show("fatalities_input_type")

        if(input$fatalities_input_type == "val"){
          shinyjs::show("fatalities_mean")
          shinyjs::show("fatalities_se")
        }
        if(input$fatalities_input_type == "eli_exp"){
          shinyjs::show("fatalities_mat_expert")
          shinyjs::show("fatalities_run_expert")
        }
      }

      # Show inputs for cumulated scenario

      if(input$analysis_choice == "cumulated"){
        shinyjs::hide("fatalities_input_type")
        shinyjs::show("farm_number_cumulated")
        shinyjs::show("fatalities_mat_cumulated")
      }

    }

    # Show inputs for population size part
    if(input$button_pop_size%%2 == 1){
      #shinyjs::show("pop_size_unit")
      shinyjs::show("pop_size_input_type")
      if(input$pop_size_input_type == "val"){
        shinyjs::show("pop_size_mean")
        shinyjs::show("pop_size_se")
      }
      if(input$pop_size_input_type == "eli_exp"){
        shinyjs::show("pop_size_mat_expert")
        shinyjs::show("pop_size_run_expert")
      }
    }

    # Show inputs for population trend/growth part
    if(input$button_pop_growth%%2 == 1){
      shinyjs::show("pop_growth_input_type")
      if(input$pop_growth_input_type == "val"){
        shinyjs::show("pop_growth_mean")
        shinyjs::show("pop_growth_se")
      }
      if(input$pop_growth_input_type == "eli_exp"){
        shinyjs::show("pop_growth_mat_expert")
        shinyjs::show("pop_growth_run_expert")
      }
      if(input$pop_growth_input_type == "trend"){
        shinyjs::show("pop_trend")
        if(input$pop_trend != "stable"){
          shinyjs::show("pop_trend_strength")
        }
      }
    }

    # Show inputs for carrying capacity part
    if(input$button_carrying_cap%%2 == 1){
      shinyjs::show("carrying_cap_input_type")
      if(input$carrying_cap_input_type == "val"){
        shinyjs::show("carrying_capacity")
      }
      if(input$carrying_cap_input_type == "eli_exp"){
        shinyjs::show("carrying_cap_mat_expert")
        shinyjs::show("carrying_cap_run_expert")
      }
    }

    # Show inputs vital rates part
    if(input$button_vital_rates%%2 == 1){
      shinyjs::show("mat_fill_vr")
    }

  }) # en observe show/hide
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  #####

  ##############################################
  ##  Reactive value
  ##--------------------------------------------
  out <- reactiveValues(run = NULL, msg = NULL)

  ready <- reactiveValues(fatalities = TRUE, pop_size = TRUE, pop_growth = TRUE, carrying_capacity = TRUE)

  param <- reactiveValues(N1 = NULL,
                          nsim = NULL,
                          cumulated_impacts = NULL,

                          fatalities_mean = NULL,
                          fatalities_se = NULL,
                          onset_time = NULL,
                          onset_year = NULL,

                          pop_size_mean = NULL,
                          pop_size_se = NULL,
                          pop_size_unit = NULL,

                          pop_growth_mean = NULL,
                          pop_growth_se = NULL,

                          fecundities = NULL,
                          survivals = NULL,
                          s_calibrated = NULL,
                          f_calibrated = NULL,
                          vr_calibrated = NULL,

                          carrying_capacity = NULL,
                          theta = NULL,
                          rMAX_species = NULL,

                          model_demo = NULL,
                          time_horzion = NULL,
                          coeff_var_environ = NULL,
                          fatal_constant = NULL,

                          fatalities_eli_result = NULL,
                          pop_size_eli_result = NULL,
                          pop_growth_eli_result = NULL,
                          carrying_cap_eli_result = NULL
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  #####

  ################################################
  ## Update the vital rate matrix (mat_fill_vr)
  ##   when changing species in the list
  ##----------------------------------------------
  # Function to create the matrix
  create.matrice <- function(data_sf, species){
    out_mat <- data_sf %>%
      filter(species == data_sf$Nom_espece) %>%
      select(classes_age, survie, fecondite)
    return(out_mat)
  }

  # Update the vital rate matrix (mat_fill_vr) when changing species in the list
  observeEvent({
    input$species_choice
  }, {

    if(input$species_choice == "Espèce générique") {} else {

      tab_species <- create.matrice(data_sf = data_sf, species = input$species_choice)

      if(all(is.na(tab_species))) {
        updateMatrixInput(session, inputId = "mat_fill_vr",
                          value = matrix(data = NA,
                                         nrow = 4,
                                         ncol = 2,
                                         dimnames = list(c("Juv 1", "Juv 2", "Juv 3", "Adulte"), c("Survie", "Fécondité"))))

      } else {
        number_age_class <- nrow(tab_species)
        ages <- tab_species$classes_age
        survivals <- tab_species$survie
        fecundities <- tab_species$fecondite

        updateMatrixInput(session, inputId = "mat_fill_vr",
                          value = matrix(data = c(survivals, fecundities),
                                         nrow = number_age_class,
                                         ncol = 2,
                                         dimnames = list(ages, c("Survie", "Fécondité"))))
      } # end if 2
    } # end if 1

  }) # end observeEvent species_list
  #####

  ##############################################
  ## Update matrix cumulated impact
  ##-------------------------------------------
  observeEvent({input$farm_number_cumulated}, {
    rows_names <- function(n){
      v <- c(paste0("Parc n°", c(1:n)))
      return(v)
    }

    nrow <- input$farm_number_cumulated
    number_parks <- rows_names(nrow)

    init_cumul_new <- rep(init_cumul_add, nrow)

    updateMatrixInput(session, inputId = "fatalities_mat_cumulated",
                      value =  matrix(init_cumul_new, nrow = nrow, ncol = 3, byrow = TRUE,
                                      dimnames = list(number_parks,
                                                      c("Moyenne",
                                                        "Erreur-type",
                                                        "Année de mise en service du parc"))))
  })
  #####


  #####
  ##--------------------------------------------
  ##  Run expert elicitation
  ##--------------------------------------------
  # Function to run the elication analysis
  func_eli <- function(mat_expert){
    t_mat_expert <- t(mat_expert)
    vals <- t_mat_expert[2:4,]
    Cp <- t_mat_expert[5,]
    weights <- t_mat_expert[1,]

    out <- elicitation(vals, Cp, weights)
    return(list(out = out, mean = out$mean_smooth, SE = sqrt(out$var_smooth)))
  }

  # Function to plot the elication analysis output
  plot_expert <- function(out, show_se = TRUE, ...){
    plot_elicitation(out, ylab = "", xlab = "Valeur du paramètre", cex.lab = 1.2, yaxt = "n")
    mtext(text = "Densité de probabilité", side = 2, line = 2, cex = 1.2)

    y2 <- dgamma(x = out$mean_smooth, shape = out$shape_smooth, rate = out$rate_smooth)
    xx <- qgamma(p = c(0.01,0.99), shape = out$shape_smooth, rate = out$rate_smooth)
    clip(xx[1], xx[2], -100, y2)
    abline(v = out$mean_smooth, lwd = 3, col = "darkblue")

    mtext(text = paste("Moyenne = ", round(out$mean_smooth,2)), side = 3, line = 2.5, cex = 1.2, adj = 0)
    if(show_se) mtext(text = paste("Erreur-type = ", round(sqrt(out$var_smooth), 2)), side = 3, line = 1, cex = 1.2, adj = 0)
  }

  ########################
  ## Fatalities
  ##----------------------
  observeEvent({
    input$fatalities_run_expert
  }, {
    if( all(!is.na(input$fatalities_mat_expert)) ) {

      ## run elicitation analysis
      param$fatalities_eli_result <- func_eli(input$fatalities_mat_expert)

      ## plot distribution
      output$title_distri_plot <- renderText({ "Mortalités annuelles" })
      output$distri_plot <- renderPlot({ plot_expert(param$fatalities_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent

  ########################
  ## Population size
  ##----------------------
  observeEvent({
    input$pop_size_run_expert
  }, {
    if(all(!is.na(input$pop_size_mat_expert))) {

      ## run elicitation analysis
      param$pop_size_eli_result <- func_eli(input$pop_size_mat_expert)

      ## plot distribution
      output$title_distri_plot <- renderText({ "Taille de population" })
      output$distri_plot <- renderPlot({ plot_expert(param$pop_size_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent

  ########################
  ## Population growth
  ##----------------------
  observeEvent({
    input$pop_growth_run_expert
  },{
    if(all(!is.na(input$pop_growth_mat_expert))) {

      ## run elicitation analysis
      param$pop_growth_eli_result <- func_eli(input$pop_growth_mat_expert)

      ## plot distribution
      output$title_distri_plot <- renderText({ "Taux de croissance de la population" })
      output$distri_plot <- renderPlot({ plot_expert(param$pop_growth_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent

  ########################
  ## Carrying capacity
  ##----------------------
  observeEvent({
    input$carrying_cap_run_expert
  },{
    if(all(!is.na(input$carrying_cap_mat_expert))) {

      ## run elicitation analysis
      param$carrying_cap_eli_result <- func_eli(input$carrying_cap_mat_expert)

      ## run elicitation analysis
      output$title_distri_plot <- renderText({ "Capacité de charge" })
      output$distri_plot <- renderPlot({ plot_expert(param$carrying_cap_eli_result$out, show_se = FALSE) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent

  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  #####


  #####
  ##--------------------------------------------
  ## Select parameter values for simulations
  ##--------------------------------------------

  #################################
  ## Cumulated impacts or not ?
  ##-------------------------------
  observe({
    if(input$analysis_choice == "scenario"){
      param$cumulated_impacts = FALSE
    } else {
      param$cumulated_impacts = TRUE
    } # end if
  }) # end observeEvent


  #################################
  ## Fatalities
  ##-------------------------------
  observeEvent({
    input$run
  }, {
    # Case 1 : Not cumulated effects (if1)
    if(input$analysis_choice == "scenario"){

      # Case 1.1 : Values from expert elicitation (if2)
      if(input$fatalities_input_type == "eli_exp"){
        if(!(is.null(param$fatalities_eli_result))){
          param$fatalities_mean <- c(0, round(param$fatalities_eli_result$mean))
          param$onset_time <- NULL
          param$fatalities_se <- c(0, round(param$fatalities_eli_result$SE))
          ready$fatalities <- TRUE
        } else {
          print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
          ready$fatalities <- FALSE
        }

      } else {

        # Case 1.2 : Values directly provided (i.e., not from expert elicitation)
        ready$fatalities <- TRUE
        param$fatalities_mean <- c(0, input$fatalities_mean)
        param$onset_time = NULL
        param$fatalities_se <- c(0, input$fatalities_se)
      } # end (if2)

      # Case 2 : Cumulated effects (if-else 1)
    } else {
      ready$fatalities <- TRUE
      param$fatalities_mean <- c(0, input$fatalities_mat_cumulated[,1])
      param$fatalities_se <- c(0, input$fatalities_mat_cumulated[,2])
      param$onset_year <- c(min(input$fatalities_mat_cumulated[,3]), input$fatalities_mat_cumulated[,3])
      param$onset_time <- param$onset_year - min(param$onset_year) + 1
    } # end (if1)

  }) # end observeEvent
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  #################################
  ## Population size
  ##-------------------------------
  observeEvent({
    input$run
  },{

    # Case 1 : Values from expert elicitation
    if(input$pop_size_input_type == "eli_exp"){
      if(!(is.null(param$pop_size_eli_result))){
        param$pop_size_mean <- round(param$pop_size_eli_result$mean)
        param$pop_size_se <- round(param$pop_size_eli_result$SE)
        ready$pop_size <- TRUE
      } else {
        print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
        ready$pop_size <- FALSE
      }

      # Case 2 : Values directly provided (i.e., not from expert elicitation)
    } else {
      ready$pop_size <- TRUE
      param$pop_size_mean <- input$pop_size_mean
      param$pop_size_se <- input$pop_size_se
    }
    param$pop_size_unit <- input$pop_size_unit
  })


  #################################
  ## Population growth
  ##-------------------------------
  observeEvent({
    input$run
  }, {

    # Case 1 : Values from expert elicitation
    if(input$pop_growth_input_type == "eli_exp"){
      if(!(is.null(param$pop_growth_eli_result))){
        param$pop_growth_mean <- round(min(1 + param$rMAX_species, round(param$pop_growth_eli_result$mean, 2)), 2)
        param$pop_growth_se <- round(param$pop_growth_eli_result$SE, 2)
        ready$pop_growth <- TRUE
      } else {
        print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
        ready$pop_growth <- FALSE
      }

    } else {

      # Case 2 : Trend information
      if(input$pop_growth_input_type == "trend"){
        ready$pop_growth <- TRUE

        if(input$pop_trend == "growth") {
          if(input$pop_trend_strength == "weak") {
            param$pop_growth_mean <- 1.01
          } else if(input$pop_trend_strength == "average"){
            param$pop_growth_mean <- 1.03
          } else {
            param$pop_growth_mean <- 1.06
          }
        } else if(input$pop_trend == "decline"){
          if(input$pop_trend_strength == "weak") {
            param$pop_growth_mean <- 0.99
          } else if(input$pop_trend_strength == "average"){
            param$pop_growth_mean <- 0.97
          } else {
            param$pop_growth_mean <- 0.94
          }
        } else {
          param$pop_growth_mean <- 1
        }
        param$pop_growth_se <- 0.03


      # Case 3 : Values directly provided (i.e., not from expert elicitation)
      } else {
        ready$pop_growth <- TRUE
        param$pop_growth_mean <- round(min(1 + param$rMAX_species, input$pop_growth_mean), 2)
        param$pop_growth_se <- input$pop_growth_se
      }
    }
  })



  #################################
  ## Carrying capacity
  ##------------------------------
  observeEvent({
    input$run
  }, {
    if(input$carrying_cap_input_type == "eli_exp"){
      if(!(is.null(param$carrying_cap_eli_result))){
        param$carrying_capacity <- round(param$carrying_cap_eli_result$mean)
        ready$carrying_capacity <- TRUE
      } else {
        print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
        ready$carrying_capacity <- FALSE
      }
    } else {
      ready$carrying_capacity <- TRUE
      param$carrying_capacity <- input$carrying_capacity
    }
  })
  #############################################
  ## Survivals, fecundities and rMAX_species
  ##-------------------------------------------
  observeEvent({input$run}, {
    param$survivals <- input$mat_fill_vr[,1]
    param$fecundities <- input$mat_fill_vr[,2]
    param$rMAX_species <- rMAX_spp(surv = tail(param$survivals,1), afr = min(which(param$fecundities != 0)))
  }) # end observeEvent
  #####

  #############################################
  ## Calibration of survivals & fecundities
  ##-------------------------------------------
  observeEvent({
    input$run
  },{
    param$vr_calibrated <- calibrate_params(
      inits = init_calib(s = param$survivals, f = param$fecundities, lam0 = param$pop_growth_mean),
      f = param$fecundities, s = param$survivals, lam0 = param$pop_growth_mean
    )
    param$s_calibrated <- head(param$vr_calibrated, length(param$survivals))
    param$f_calibrated <- tail(param$vr_calibrated, length(param$fecundities))
  })
  #####

  ############################################################
  ## Observe parameter values to be used in simulations run
  ##----------------------------------------------------------
  observe({
    param # required to ensure up-to-date values are run

    # simple inputs
    param$nsim <- input$nsim
    param$fatal_constant <- input$fatal_constant

    # fixed in global environment (for now)
    param$theta = theta
    param$time_horzion = time_horzion
    param$coeff_var_environ = coeff_var_environ

  }) # end observe
  #####


  #####
  ##--------------------------------------------
  ##  Display parameter distribution
  ##--------------------------------------------

  # Function to plot a gamma distribution
  plot_gamma <- function(mu, se, show_mean = TRUE, show_se = TRUE, ...){

    ## Define shape and scale parameter of gamma distribution
    shape = (mu/se)^2
    scale = se^2/mu

    ## Plot the curve
    curve(dgamma(x, shape=shape, scale=scale), from = max(0,mu-3*se), to = mu+4*se, lwd = 3, col = "darkblue", yaxt = "n",
          ylab = "", xlab = "Valeur du paramètre", cex.lab = 1.2)
    mtext(text = "Densité de probabilité", side = 2, line = 2, cex = 1.2)

    y2 <- dgamma(x = mu, shape = shape, scale = scale)
    xx <- qgamma(p = c(0.01,0.99), shape = shape, scale = scale)
    clip(xx[1], xx[2], -100, y2)
    abline(v = mu, lwd = 3, col = "darkblue")

    if(show_mean) mtext(text = paste("Moyenne = ", round(mu, 2)), side = 3, line = 2.5, cex = 1.2, adj = 0)
    if(show_se) mtext(text = paste("Erreur-type = ", round(se, 2)), side = 3, line = 1, cex = 1.2, adj = 0)
  }

  ########################
  ## Fatalities
  ##----------------------
  observeEvent({
    input$analysis_choice
    input$button_fatalities
    input$fatalities_input_type
    input$fatalities_run_expert

    input$farm_number_cumulated
    input$fatalities_mat_cumulated
  },{
    if(input$analysis_choice != "cumulated"){

      # Show from input values: if button is ON and input_type is set on "value"
      if(input$button_fatalities%%2 == 1 & input$fatalities_input_type == "val"){
        output$title_distri_plot <- renderText({ "Mortalités annuelles" })
        output$distri_plot <- renderPlot({ plot_gamma(mu = input$fatalities_mean, se = input$fatalities_se) })
      } else {
        # Show from elicitation expert: if button is ON and input_type is set on "expert elicitation"
        if(input$button_fatalities%%2 == 1 & input$fatalities_input_type == "eli_exp"){
          if(!is.null(param$fatalities_eli_result)){
            output$title_distri_plot <- renderText({ "Mortalités annuelles" })
            output$distri_plot <- renderPlot({ plot_expert(param$fatalities_eli_result$out) })
          } else {
            output$title_distri_plot <- NULL
            output$distri_plot <- NULL
          }
          # Hide otherwise (when button is OFF)
        }else{
          output$title_distri_plot <- NULL
          output$distri_plot <- NULL
        }
      }

      # Hide otherwise (when analysis = cumulated impacts)
    }else{
      output$title_distri_plot <- renderText({ "Mortalités annuelles par parc (impacts cumulés)" })

      # output$distri_plot <- NULL
      output$distri_plot <- renderPlot({
        par(mfrow = c(1,input$farm_number_cumulated), mar = c(5, 4, 7, 2) + 0.1, oma = c(0,0,0,0))
        for(j in 1:input$farm_number_cumulated){
          plot_gamma(mu = input$fatalities_mat_cumulated[j,1], se = input$fatalities_mat_cumulated[j,2])
          title(paste("Parc", j), line = 5, outer = FALSE, cex.main = 1.8)
        }
      })

    }
  }, ignoreInit = FALSE)


  ########################
  ## Population size
  ##----------------------
  observeEvent({
    input$pop_size_input_type
    input$button_pop_size
  },{
    # Show from input values: if button is ON and input_type is set on "value"
    if(input$button_pop_size%%2 == 1 & input$pop_size_input_type == "val"){
      output$title_distri_plot <- renderText({ "Taille initiale de la population" })
      output$distri_plot <- renderPlot({ plot_gamma(mu = input$pop_size_mean, se = input$pop_size_se) })
    } else {
      # Show from elicitation expert: if button is ON and input_type is set on "expert elicitation"
      if(input$button_pop_size%%2 == 1 & input$pop_size_input_type == "eli_exp"){
        if(!is.null(param$pop_size_eli_result)){
          output$title_distri_plot <- renderText({ "Taille initiale de la population" })
          output$distri_plot <- renderPlot({ plot_expert(param$pop_size_eli_result$out) })
        } else {
          output$title_distri_plot <- NULL
          output$distri_plot <- NULL
        }
        # Hide otherwise (when button is OFF)
      }else{
        output$title_distri_plot <- NULL
        output$distri_plot <- NULL
      }
    }
  }, ignoreInit = FALSE)


  ########################
  ## Population growth
  ##----------------------
  observeEvent({
    input$pop_growth_input_type
    input$button_pop_growth
  },{
    # Show from input values: if button is ON and input_type is set on "value"
    if(input$button_pop_growth%%2 == 1 & input$pop_growth_input_type == "val"){
      output$title_distri_plot <- renderText({ "Taux de croissance de la population" })
      output$distri_plot <- renderPlot({ plot_gamma(mu = input$pop_growth_mean, se = input$pop_growth_se) })
    } else {
      # Show from elicitation expert: if button is ON and input_type is set on "expert elicitation"
      if(input$button_pop_growth%%2 == 1 & input$pop_growth_input_type == "eli_exp"){
        if(!is.null(param$pop_growth_eli_result)){
          output$title_distri_plot <- renderText({ "Taux de croissance de la population" })
          output$distri_plot <- renderPlot({ plot_expert(param$pop_growth_eli_result$out) })
        } else {
          output$title_distri_plot <- NULL
          output$distri_plot <- NULL
        }
        # Hide otherwise (when button is OFF)
      }else{
        output$title_distri_plot <- NULL
        output$distri_plot <- NULL
      }
    }
  }, ignoreInit = FALSE)

  ########################
  ## Carrying capacity
  ##----------------------
  observeEvent({
    input$carrying_cap_input_type
    input$button_carrying_cap
  },{
    # Show from elicitation expert: if button is ON and input_type is set on "expert elicitation"
    if(input$button_carrying_cap%%2 == 1 & input$carrying_cap_input_type == "eli_exp"){
      if(!is.null(param$carrying_cap_eli_result)){
        output$title_distri_plot <- renderText({ "Capacité de charge" })
        output$distri_plot <- renderPlot({ plot_expert(param$carrying_cap_eli_result$out) })
      } else {
        output$title_distri_plot <- NULL
        output$distri_plot <- NULL
      }
      # Hide otherwise (when button is OFF)
    }else{
      output$title_distri_plot <- NULL
      output$distri_plot <- NULL
    }
  }, ignoreInit = FALSE)
  #####


  #####
  ##-------------------------------------------------
  ##  Display parameter values (on the side panel)
  ##-------------------------------------------------
  #################################
  ## Fatalities
  ##-------------------------------
  output$fatalities_mean_info <- renderText({  paste0("Moyenne : ", tail(param$fatalities_mean, 1)) })
  output$fatalities_se_info <- renderText({  paste0("Erreur-type : ", tail(param$fatalities_se, 1)) })


  #################################
  ## Poplutation size
  ##-------------------------------
  ## UNIT
  output$pop_size_unit_info <- renderText({
    if(!is.null(param$pop_size_unit)){
      if(param$pop_size_unit == "Npair"){
        paste0("Nombre de couple")
      } else {
        paste0("Effectif total")
      }
    }
  })

  ## VALUES
  output$pop_size_mean_info <- renderText({  paste0("Moyenne : ", param$pop_size_mean) })
  output$pop_size_se_info <- renderText({  paste0("Erreur-type : ", param$pop_size_se) })


  #################################
  ## Population growth
  ##-------------------------------
  output$pop_growth_mean_info <- renderText({  paste0("Moyenne : ", param$pop_growth_mean) })
  output$pop_growth_se_info <- renderText({  paste0("Erreur-type : ", param$pop_growth_se) })


  #################################
  ## Carrying capacity
  ##-------------------------------
  # UNIT (like pop size)
  output$carrying_capacity_info <- renderText({

    # Source info "unit"
    if(is.null(param$pop_size_unit)){
      unit1 <- input$pop_size_unit
    }else{
      unit1 <- param$pop_size_unit
    }

    # UNIT information
    if(unit1 == "Npair"){
      info1 <- paste0("Nombre de couple")
    } else {
      info1 <- paste0("Effectif total")
    }

    # paste for printing
    paste0(info1, " : ", param$carrying_capacity)
  })


  #################################
  ## Vital rates
  ##-------------------------------
  output$vital_rates_info <- renderTable({
    input$mat_fill_vr
  }, rownames = TRUE)
  #####



  #####
  ##-----------------------------------------------------------------------------------
  ##                                RUN SIMULATIONS
  ##-----------------------------------------------------------------------------------
  observeEvent({
    input$run
  }, {

    if(ready$fatalities & ready$pop_size & ready$pop_growth & ready$carrying_capacity){
      withProgress(message = 'Simulation progress', value = 0, {

        out$run <- run_simul_shiny(nsim = param$nsim,
                                   cumulated_impacts = param$cumulated_impacts,

                                   fatalities_mean = param$fatalities_mean,
                                   fatalities_se = param$fatalities_se,
                                   onset_time = param$onset_time,

                                   pop_size_mean = param$pop_size_mean,
                                   pop_size_se = param$pop_size_se,
                                   pop_size_type = param$pop_size_unit,

                                   pop_growth_mean = param$pop_growth_mean,
                                   pop_growth_se = param$pop_growth_se,

                                   survivals = param$s_calibrated,
                                   fecundities = param$f_calibrated,

                                   carrying_capacity = param$carrying_capacity,
                                   theta = param$theta,
                                   rMAX_species = param$rMAX_species,

                                   model_demo = NULL,
                                   time_horzion = param$time_horzion,
                                   coeff_var_environ = param$coeff_var_environ,
                                   fatal_constant = param$fatal_constant)
      }) # Close withProgress

    }else{
      out$run <- NULL
      out$msg <- "error_not_ready"
    }
  }) # Close observEvent
  #####



  #####
  ##-----------------------------------------------------------------------------------
  ##                                OUTPUTS
  ##-----------------------------------------------------------------------------------

  ##-------------------------------------------
  ## Impact text
  ##-------------------------------------------
  ## Two Functions to print the output
  print_it <- function(impact, lci, uci){
    paste0("Impact sur la taille de population : ", round(impact, 2)*100, "%",
           "[", round(lci, 2)*100, "% ; ", round(uci, 2)*100, "%]")
  }

  print_out <- function()
    if(!is.null(out$run)) {
      # Print the result
      print_it(impact = get_metrics(N = out$run$N)$scenario$impact[time_horzion, "avg",-1],
               lci = get_metrics(N = out$run$N)$scenario$impact[time_horzion, "lci",-1],
               uci = get_metrics(N = out$run$N)$scenario$impact[time_horzion, "uci",-1])
    } else {
      # When run is NULL

      if(!is.null(out$msg)){

        # Print the error msg, if there is one
        if(out$msg == "error_not_ready"){
          paste0("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
        }else{
          paste0("Some other error occurred")
        }

      }else{
        # When no error msg : nothing happens
      } # if "msg"
    } # if "run

  # Display result (text)
  output$impact_text <- renderText({
    if(!param$cumulated_impacts){
      print_out()
    } else{
      NULL
    }
  })

  ##-------------------------------------------
  ## Plot Impacts
  ##-------------------------------------------
  ## Function to plot the impact
  plot_out_impact <- function(){
    if(is.null(out$run)) {} else {plot_impact(N = out$run$N, xlab = "year", ylab = "pop size")}
  }

  output$title_impact_plot <- renderText({
    if(input$run > 0){
      "Résultat : Impact relatif au cours du temps"
    }
  })

  output$impact_plot <- renderPlot({
    plot_out_impact()
  })

  ##-------------------------------------------
  ## Plot Demographic Trajectories
  ##-------------------------------------------
  # Function to plot trajectories
  plot_out_traj <- function(){
    if(is.null(out$run)) {} else {plot_traj(N = out$run$N, xlab = "year", ylab = "pop size")}
  }

  output$graph_traj <- renderPlot({
    plot_out_traj()
  })
  #####


  ###################################################################################
} # End server

