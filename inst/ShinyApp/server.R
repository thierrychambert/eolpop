server <- function(input, output, session){


  ##--------------------------------------------
  ##  Hide all inputs excepted actionButtons  --
  ##--------------------------------------------
  observe({
    shinyjs::hide("fatal_constant")
    shinyjs::hide("fatalities_input_type")
    shinyjs::hide("fatalities_mean")
    shinyjs::hide("fatalities_se")
    shinyjs::hide("fatalities_mat_expert")
    shinyjs::hide("fatalities_run_expert")
    shinyjs::hide("farm_number_cumulated")
    shinyjs::hide("fatalities_mat_cumulated")
    shinyjs::hide("pop_size_type")
    shinyjs::hide("pop_size_input_type")
    shinyjs::hide("pop_size_mean")
    shinyjs::hide("pop_size_se")
    shinyjs::hide("pop_size_mat_expert")
    shinyjs::hide("pop_size_run_expert")
    shinyjs::hide("carrying_cap_input_type")
    shinyjs::hide("carrying_capacity")
    shinyjs::hide("carrying_cap_mat_expert")
    shinyjs::hide("carrying_cap_run_expert")
    shinyjs::hide("pop_growth_input_type")
    shinyjs::hide("pop_growth_mean")
    shinyjs::hide("pop_growth_se")
    shinyjs::hide("pop_growth_mat_expert")
    shinyjs::hide("pop_growth_run_expert")
    shinyjs::hide("pop_trend")
    shinyjs::hide("pop_trend_strength")
    shinyjs::hide("mat_fill_vr")

    # Show fatalities part

    if(input$button_fatalities%%2 == 1){
      shinyjs::show("fatal_constant")

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
        shinyjs::show("farm_number_cumulated")
        shinyjs::show("fatalities_mat_cumulated")
      }

    }

    # Show inputs for population size part

    if(input$button_pop_size%%2 == 1){
      shinyjs::show("pop_size_type")
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

    # Show inputs for population trend part

    if(input$button_pop_trend%%2 == 1){
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
        shinyjs::show("pop_trend_strength")
      }
    }

    # Show inputs vital rates part

    if(input$button_vital_rates%%2 == 1){
        shinyjs::show("mat_fill_vr")
    }

  }) # en observe show/hide
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  ##----------------------------------------------
  ##  Function to run the elicitation analysis  --
  ##----------------------------------------------
  # Function to extract value from elicitation matrix and run the elication analysis
  func_eli <- function(mat_expert){
    t_mat_expert <- t(mat_expert)
    vals <- t_mat_expert[2:4,]
    Cp <- t_mat_expert[5,]
    weights <- t_mat_expert[1,]

    out <- elicitation(vals, Cp, weights)
    return(list(out = out, mean = out$mean_smooth, SE = sqrt(out$var_smooth)))
  }
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  ##--------------------------------------------
  ##  Reactive value : simulation inputs      --
  ##--------------------------------------------
  param <- reactiveValues(N1 = NULL,
                          nsim = NULL,
                          cumulated_impacts = NULL,

                          fatalities_mean = NULL,
                          fatalities_se = NULL,
                          onset_time = NULL,
                          onset_year = NULL,

                          pop_size_mean = NULL,
                          pop_size_se = NULL,
                          pop_size_type = NULL,

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


  ##----------------------------------------------------------
  ## Observe parameter values to be used in simulations run --
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
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  ##--------------------------------------------
  ##  Display parameter distribution          --
  ##--------------------------------------------
  plot_distri <- function(mu, se) curve(dnorm(x, mu, se), from = mu-3*se, to = mu+3*se, lwd = 3, col = "darkblue",
                                        ylab = "Densité de probabilité", xlab = "Valeur du paramètre", cex.lab = 1.2)

  ## Fatalities ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$fatalities_input_type
  },{
  output$fatalities_distri_plot <- renderPlot({ plot_distri(mu = input$fatalities_mean, se = input$fatalities_se) })
  })

  ## Population size ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$pop_size_input_type
  },{
    output$pop_size_distri_plot <- renderPlot({ plot_distri(mu = input$pop_size_mean, se = input$pop_size_se) })
  })

  ## Population growth ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$pop_growth_input_type
  },{
    output$pop_growth_distri_plot <- renderPlot({ plot_distri(mu = input$pop_growth_mean, se = input$pop_growth_se) })
  })
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  ##--------------------------------------------
  ##  Run expert elicitation                  --
  ##--------------------------------------------
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

  ## Fatalities ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$fatalities_run_expert
  }, {
    if( all(!is.na(input$fatalities_mat_expert)) ) {

      ## run elicitation analysis
      param$fatalities_eli_result <- func_eli(input$fatalities_mat_expert)

      ## plot distribution
      output$fatalities_distri_plot <- renderPlot({ plot_expert(param$fatalities_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent


  ## Population size ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$pop_size_run_expert
  }, {
    if(all(!is.na(input$pop_size_mat_expert))) {

      ## run elicitation analysis
      param$pop_size_eli_result <- func_eli(input$pop_size_mat_expert)

      ## plot distribution
      output$pop_size_distri_plot <- renderPlot({ plot_expert(param$pop_size_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent


  ## Population growth ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$pop_growth_run_expert
  },{
    if(all(!is.na(input$pop_growth_mat_expert))) {

      ## run elicitation analysis
      param$pop_growth_eli_result <- func_eli(input$pop_growth_mat_expert)

      ## plot distribution
      output$pop_growth_distri_plot <- renderPlot({ plot_expert(param$pop_growth_eli_result$out) })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent


  ## Carrying capacity ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$carrying_cap_run_expert
  },{
    if(all(!is.na(input$carrying_cap_mat_expert))) {

      param$carrying_cap_eli_result <- func_eli(input$carrying_cap_mat_expert)

      ## run elicitation analysis
      output$carrying_cap_distri_plot <- renderPlot({
        plot_expert(param$carrying_cap_eli_result$out, show_se = FALSE)
        })

    } else {
      print("missing value")
    } # end if
  }) # end observeEvent
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  ##--------------------------------------------
  ## Select parameter values for simulations  --
  ##--------------------------------------------

  ## Cumulated impacts or not ? ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$run
  }, {
    if(input$analysis_choice == "scenario"){
      param$cumulated_impacts = FALSE
    } else {
      param$cumulated_impacts = TRUE
    } # end if
  }) # end observeEvent


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  ## Fatalities ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$run
  }, {
    # Case 1 : Not cumulated effects (if1)
    if(input$analysis_choice == "scenario"){

      # Case 1.1 : Values from expert elicitation (if2)
      if(input$fatalities_input_type == "eli_exp"){
        param$fatalities_mean <- c(0, round(param$fatalities_eli_result$mean))
        param$onset_time <- NULL
        param$fatalities_se <- c(0, round(param$fatalities_eli_result$SE))

      } else {

        # Case 1.2 : Values directly provided (i.e., not from expert elicitation)
        param$fatalities_mean <- c(0, input$fatalities_mean)
        param$onset_time = NULL
        param$fatalities_se <- c(0, input$fatalities_se)
      } # end (if2)

      # Case 2 : Cumulated effects (if-else 1)
    } else {
      param$fatalities_mean <- c(0, input$fatalities_mat_cumulated[,1])
      param$fatalities_se <- c(0, input$fatalities_mat_cumulated[,2])
      param$onset_year <- c(min(input$fatalities_mat_cumulated[,3]), input$fatalities_mat_cumulated[,3])
      param$onset_time <- param$onset_year - min(param$onset_year) + 1
    } # end (if1)

  }) # end observeEvent
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
  ## Population size ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$run
  },{

    # Case 1 : Values from expert elicitation
    if(input$pop_size_input_type == "eli_exp"){
      if(!(is.null(param$pop_size_eli_result))){
        param$pop_size_mean <- round(param$pop_size_eli_result$mean)
        param$pop_size_se <- round(param$pop_size_eli_result$SE)
      } else {
        print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
      }

    # Case 2 : Values directly provided (i.e., not from expert elicitation)
    } else {
      param$pop_size_mean <- input$pop_size_mean
      param$pop_size_se <- input$pop_size_se
    }
    param$pop_size_type <- input$pop_size_type
  })


  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## Survivals, fecundities and rMAX_species ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({input$run}, {
    param$survivals <- input$mat_fill_vr[,1]
    param$fecundities <- input$mat_fill_vr[,2]
    param$rMAX_species <- rMAX_spp(surv = tail(param$survivals,1), afr = min(which(param$fecundities != 0)))
  }) # end observeEvent




  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~##
  ## Population growth ###~~~~~~~~~~~~~~~~~~~~~~~~~~###
  observeEvent({
    input$run
  }, {

    # Case 1 : Values from expert elicitation
    if(input$pop_growth_input_type == "eli_exp"){
      if(!(is.null(param$pop_growth_eli_result))){
        param$pop_growth_mean <- round(min(1 + param$rMAX_species, round(param$pop_growth_eli_result$mean, 2)), 2)
        param$pop_growth_se <- round(param$pop_growth_eli_result$SE, 2)
      } else {
        print("Erreur: Vous n'avez pas lancer l'analyse 'valeurs experts'")
      }

    # Case 2 : Trend information
    } else if(input$pop_growth_input_type == "trend"){

      if(input$pop_trend == "Croissance") {
        if(input$pop_trend_strength == "Faible") {
          param$pop_growth_mean <- 1.01
        } else if(input$pop_trend_strength == "Moyen"){
          param$pop_growth_mean <- 1.03
        } else {
          param$pop_growth_mean <- 1.06
        }
      } else if(input$pop_trend == "Déclin"){
        if(input$pop_trend_strength == "Faible") {
          param$pop_growth_mean <- 0.99
        } else if(input$pop_trend_strength == "Moyen"){
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
      param$pop_growth_mean <- round(min(1 + param$rMAX_species, input$pop_growth_mean), 2)
      param$pop_growth_se <- input$pop_growth_se
    }
  })

  # Survival and fecundity calibration
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


  # Observe carrying capacity
  observeEvent({input$run}, {
    if(input$carrying_cap_input_type == "eli_exp"){
      if(!(is.null(param$carrying_cap_eli_result))){
        param$carrying_capacity <- round(param$carrying_cap_eli_result$mean)
      } else {
        print("#intégrer un message d'erreur")
      }
    } else {
      param$carrying_capacity <- input$carrying_capacity
    }
  })

  observeEvent({input$run}, {
    print(param$pop_growth_mean)
    print(param$pop_growth_se)
  })

  # End of reactive

  # Simulations

  observeEvent({
    input$run
  }, {

    withProgress(message = 'Simulation progress', value = 0, {

      param$N1 <- run_simul_shiny(nsim = param$nsim,
                                  cumulated_impacts = param$cumulated_impacts,

                                  fatalities_mean = param$fatalities_mean,
                                  fatalities_se = param$fatalities_se,
                                  onset_time = param$onset_time,

                                  pop_size_mean = param$pop_size_mean,
                                  pop_size_se = param$pop_size_se,
                                  pop_size_type = param$pop_size_type,

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
  }) # Close observEvent


  # Plot Impacts

  plot_out_impact <- function(){
    if(is.null(param$N1)) {} else {plot_impact(N = param$N1$N, xlab = "year", ylab = "pop size")}
  }

  output$graph_impact <- renderPlot({
    plot_out_impact()
  })

  # Plot trajectories

  plot_out_traj <- function(){
    if(is.null(param$N1)) {} else {plot_traj(N = param$N1$N, xlab = "year", ylab = "pop size")}
  }

  output$graph_traj <- renderPlot({
    plot_out_traj()
  })
  # End simulations


  ##--------------------------------------------
  ##  Display General information             --
  ##--------------------------------------------

  output$species_name <- renderText({ paste0("Espèce : ", as.character(input$species_choice)) })


  ## Fatalities
  output$fatalities_mean_info <- renderText({
    if(input$fatalities_input_type == "eli_exp"){
      if(!(is.null(param$fatalities_eli_result))){
        info <- round(param$fatalities_eli_result$mean, 2)
      } else {
        info <- NA
      }
    }
    else {
      info <- input$fatalities_mean
    }
    paste0("Moyenne : ", info)
  })

  output$fatalities_se_info <- renderText({
    if(input$fatalities_input_type == "eli_exp"){
      if(!(is.null(param$fatalities_eli_result))){
        info <- round(param$fatalities_eli_result$SE, 2)
      } else {
        info <- NA
      }
    }
    else {
      info <- input$fatalities_se
    }
    paste0("Erreur-type : ", info)
  })










  ## Poplutation size

  output$pop_size_type_info <- renderText({
    if(input$pop_size_type == "Npair"){
      paste0("Nombre de couple")
    } else {
      paste0("Effectif total")
    }
  })

  output$pop_size_mean_info <- renderText({
    if(input$pop_size_input_type == "eli_exp"){
      if(!(is.null(param$pop_size_eli_result))){
        info <- round(param$pop_size_eli_result$mean)
      } else {info <- NA}
    }
    else {
      info <- input$pop_size_mean
    }
    paste0("Moyenne : ", info)
  })

  output$pop_size_se_info <- renderText({
    if(input$pop_size_input_type == "eli_exp"){
      if(!(is.null(param$pop_size_eli_result))){
        info <- round(param$pop_size_eli_result$SE)
      } else {info <- NA}
    }
    else {
      info <- input$pop_size_se
    }
    paste0("Erreur-type : ", info)
  })

  ## Carrying capacity

  output$carrying_capacity_info <- renderText({
    if(input$carrying_cap_input_type == "eli_exp"){
      if(!(is.null(param$carrying_cap_eli_result))){
        info <- round(param$carrying_cap_eli_result$mean)
      } else {info <- NA}
    }
    else {
      info <- input$carrying_capacity
    }
    paste0("Valeur : ", info)
  })

  ## Population growth

  output$pop_trend_type_info <- renderText({paste0("Type de Tendance de pop : ", input$pop_growth_input_type)})

  output$pop_growth_mean_info <- renderText({
    if(input$pop_growth_input_type == "eli_exp"){
      if(!(is.null(param$pop_growth_eli_result))){
        info <- round(param$pop_growth_eli_result$mean, 2)
      } else {info <- NA}
    } else if(input$pop_growth_input_type == "trend"){
        if(input$pop_trend == "Croissance") {
          if(input$pop_trend_strength == "Faible") {
            info <- 1.01
          } else if(input$pop_trend_strength == "Moyen"){
            info <- 1.03
          } else {
            info <- 1.06
          }
        } else if(input$pop_trend == "Déclin"){
          if(input$pop_trend_strength == "Faible") {
            info <- 0.99
          } else if(input$pop_trend_strength == "Moyen"){
            info <- 0.97
          } else {
            info <- 0.94
          }
        } else {
          info <- 1.00
        }
    } else {
        info <- input$pop_growth_mean
    }
    paste0("Moyenne : ", info)
  })

  output$pop_growth_se_info <- renderText({
    if(input$pop_growth_input_type == "eli_exp"){
      if(!(is.null(param$pop_growth_eli_result))){
        info <- round(param$pop_growth_eli_result$SE, 2)
      } else {info <- NA}
    } else if (input$pop_growth_input_type == "trend") {
      info <- 0.03
    }
    else {
      info <- input$pop_growth_se
    }
    paste0("Erreur-type : ", info)
  })


  ## Vital rates

  output$vital_rates_info <- renderTable({
    input$mat_fill_vr
    }, rownames = TRUE)

  # End genral informations output



  ## Update matrix cumulated impact

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

  # Survivals and Fecundities

  create.matrice <- function(data_sf, species){
    out_mat <- data_sf %>%
      filter(species == data_sf$Nom_espece) %>%
      select(classes_age, survie, fecondite)
    return(out_mat)
  }


## Update the vital rate matrix when changing species in the list
  observeEvent({input$species_choice}, {

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


###################################################################################
} # End server

