server <- function(input, output){

  # Hide all inputs excepted actionButtons

  observe({
    shinyjs::hide("fatal_constant")
    shinyjs::hide("fatalities_input_type")
    shinyjs::hide("fatalities_mean")
    shinyjs::hide("fatalities_se")
    shinyjs::hide("fatalities_mat_expert")
    shinyjs::hide("farm_number_cumulated")
    shinyjs::hide("fatalities_mat_cumulated")
    shinyjs::hide("pop_size_type")
    shinyjs::hide("pop_size_input_type")
    shinyjs::hide("pop_size_mean")
    shinyjs::hide("pop_size_se")
    shinyjs::hide("pop_size_mat_expert")
    shinyjs::hide("carrying_cap_input_type")
    shinyjs::hide("carrying_capacity")
    shinyjs::hide("carrying_cap_mat_expert")
    shinyjs::hide("lambda_input_type")
    shinyjs::hide("pop_growth_mean")
    shinyjs::hide("pop_growth_se")
    shinyjs::hide("pop_growth_mat_expert")
    shinyjs::hide("pop_trend")
    shinyjs::hide("pop_trend_strength")
    shinyjs::hide("fill_type_vr")
    shinyjs::hide("mat_display_vr")
    shinyjs::hide("mat_fill_vr")

    # Show fatalities part

    if(input$button_fatalities%%2 == 1){
      shinyjs::show("fatal_constant")

      # Show inputs for none cumulated impacts scenario

      if(input$analysis_choice == "scenario"){
        shinyjs::show("fatalities_input_type")
        if(input$fatalities_input_type == "Valeurs"){
          shinyjs::show("fatalities_mean")
          shinyjs::show("fatalities_se")
        }
        if(input$fatalities_input_type == "Elicitation d'expert"){
          shinyjs::show("fatalities_mat_expert")
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
      if(input$pop_size_input_type == "Valeurs"){
        shinyjs::show("pop_size_mean")
        shinyjs::show("pop_size_se")
      }
      if(input$pop_size_input_type == "Elicitation d'expert"){
        shinyjs::show("pop_size_mat_expert")
      }
    }

    # Show inputs for carrying capacity part

    if(input$button_carrying_cap%%2 == 1){
      shinyjs::show("carrying_cap_input_type")
      if(input$carrying_cap_input_type == "Valeurs"){
        shinyjs::show("carrying_capacity")
      }
      if(input$carrying_cap_input_type == "Elicitation d'expert"){
        shinyjs::show("carrying_cap_mat_expert")
      }
    }

    # Show inputs for population trend part

    if(input$button_pop_trend%%2 == 1){
      shinyjs::show("lambda_input_type")
      if(input$lambda_input_type == "Taux de croissance"){
        shinyjs::show("pop_growth_mean")
        shinyjs::show("pop_growth_se")
      }
      if(input$lambda_input_type == "Elicitation d'expert"){
        shinyjs::show("pop_growth_mat_expert")
      }
      if(input$lambda_input_type == "Tendance locale ou régionale"){
        shinyjs::show("pop_trend")
        shinyjs::show("pop_trend_strength")
      }
    }

    # Show inputs vital rates part

    if(input$button_vital_rates%%2 == 1){
      shinyjs::show("fill_type_vr")
      if(input$fill_type_vr == "Automatique"){
        shinyjs::show("mat_display_vr")
      }
      if(input$fill_type_vr == "Manuelle"){
        shinyjs::show("mat_fill_vr")
      }
    }
  })

  ## Output

  param <- reactiveValues(N1 = NULL,
                          fatalities_mean = NULL,
                          fecundities = NULL,
                          survivals = NULL,
                          s_calibrated = NULL,
                          f_calibrated = NULL,
                          vr_calibrated = NULL,
                          cumulated_impacts = NULL,
                          onset_time = NULL,
                          onset_year = NULL,
                          carrying_capacity = NULL,
                          rMAX_species = rMAX_species,
                          theta = theta)

  # Reactive values (cumulated impacts, fatalities mean, fatalities se, onset_time, survivals mean, fecundities mean)

  observeEvent({input$run}, {
    if(input$analysis_choice == "scenario"){
      param$cumulated_impacts = FALSE
    } else {
      param$cumulated_impacts = TRUE
    }
  })

  # fatalities mean and onset_time

  observeEvent({input$run}, {
    if(input$analysis_choice == "scenario"){
      param$fatalities_mean <- c(0, input$fatalities_mean)
      param$onset_time = NULL
    } else {
      param$fatalities_mean <- c(0, input$fatalities_mat_cumulated[,1])
      param$onset_year <- c(min(input$fatalities_mat_cumulated[,3]), input$fatalities_mat_cumulated[,3])
      param$onset_time <- param$onset_year - min(param$onset_year) + 1
    }
  })

  # fatalities se

  observeEvent({input$run}, {
    if(input$analysis_choice == "scenario"){
      param$fatalities_se <- c(0, input$fatalities_se)
    } else {
      param$fatalities_se <- c(0, input$fatalities_mat_cumulated[,2])
    }
  })

  # Survivals and fecundities

  observeEvent({input$run}, {
    if(input$fill_type_vr == "Manuelle"){
      param$survivals <- input$mat_fill_vr[,1]
      param$fecundities <- input$mat_fill_vr[,2]
    } else {
      param$survivals <- survivals
      param$fecundities <- fecundities
    }
  })


  # Observe pop growth value
  ##  Avoid unrealistic scenarios
  observe({
    param$pop_growth_mean <- round(min(1 + param$rMAX_species, input$pop_growth_mean), 2)
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
  observeEvent({
    input$run
  },{
    param$carrying_capacity = input$carrying_capacity
  })

  # End of reactive

  # Simulations

  observeEvent({
    input$run
  }, {

    withProgress(message = 'Simulation progress', value = 0, {

    param$N1 <- run_simul_shiny(nsim = input$nsim,
                        cumuated_impacts = param$cumulated_impacts,

                        fatalities_mean = param$fatalities_mean,
                        fatalities_se = param$fatalities_se,
                        onset_time = param$onset_time,

                        pop_size_mean = input$pop_size_mean,
                        pop_size_se = input$pop_size_se,
                        pop_size_type = input$pop_size_type,

                        pop_growth_mean = param$pop_growth_mean,
                        pop_growth_se = input$pop_growth_se,

                        survivals = param$s_calibrated,
                        fecundities = param$f_calibrated,

                        carrying_capacity = param$carrying_capacity,
                        theta = param$theta,
                        rMAX_species = param$rMAX_species,

                        model_demo = NULL,
                        time_horzion = time_horzion,
                        coeff_var_environ = coeff_var_environ,
                        fatal_constant = input$fatal_constant)
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

  # Elicitation experts part

  func_eli <- function(mat_expert){
    t_mat_expert <- t(mat_expert)
    vals = t_mat_expert[3:5,]
    Cp = t_mat_expert[6,]
    weights = t_mat_expert[2,]

    out <- elicitation(vals, Cp, weights)
    return(list(out = out, mean = param$mean_smooth, SE = sqrt(param$var_smooth)))
  }

  func_eli_plot <- function(out){
    plot_elicitation(out)
  }

  observeEvent({input$run_expert}, {
    if(all(is.na(input$fatalities_mat_expert))) {} else {
      fatalities_result_eli <- func_eli(input$fatalities_mat_expert)
      output$fatalities_expert_mean <- renderText({paste0("Moyenne : ", fatalities_result_eli$mean)})
      output$fatalities_expert_sqrt_var <- renderText({paste0("Ecart-type : ", fatalities_result_eli$SE)})
      output$fatalities_expert_plot <- renderPlot({func_eli_plot(fatalities_result_eli$out)})
    }
    if(all(is.na(input$pop_size_mat_expert))) {} else {
      pop_size_result_eli <- func_eli(input$pop_size_mat_expert)
      output$pop_size_expert_mean <- renderText({paste0("Moyenne : ", pop_size_result_eli$mean)})
      output$pop_size_expert_sqrt_var <- renderText({paste0("Ecart-type : ", pop_size_result_eli$SE)})
      output$pop_size_expert_plot <- renderPlot({func_eli_plot(pop_size_result_eli$out)})
    }
    if(all(is.na(input$carrying_cap_mat_expert))) {} else {
      carrying_cap_result_eli <- func_eli(input$carrying_cap_mat_expert)
      output$carrying_cap_expert_mean <- renderText({paste0("Moyenne : ", carrying_cap_result_eli$mean)})
      output$carrying_cap_expert_sqrt_var <- renderText({paste0("Ecart-type : ", carrying_cap_result_eli$SE)})
      output$carrying_cap_expert_plot <- renderPlot({func_eli_plot(carrying_cap_result_eli$out)})
    }
    if(all(is.na(input$pop_growth_mat_expert))) {} else {
      pop_growth_result_eli <- func_eli(input$pop_growth_mat_expert)
      output$pop_growth_expert_mean <- renderText({paste0("Moyenne : ", pop_growth_result_eli$mean)})
      output$pop_growth_expert_sqrt_var <- renderText({paste0("Ecart-type : ", pop_growth_result_eli$SE)})
      output$pop_growth_expert_plot <- renderPlot({func_eli_plot(pop_growth_result_eli$out)})
    }
  })
  # End of elicitation part

  # Info outputs

  output$fatalities_mean_info <- renderText({paste0("Moyenne des mortalités : ", input$fatalities_mean)})
  output$fatalities_se_info <- renderText({paste0("Ecart-type des mortalités : ", input$fatalities_se)})

  output$pop_size_mean_info <- renderText({paste0("Moyenne Taille de pop : ", input$pop_size_mean)})
  output$pop_size_se_info <- renderText({paste0("Ecart-type Taille de pop : ", input$pop_size_se)})

  output$carrying_capacity_info <- renderText({paste0("Moyenne Capacité de charge : ", input$carrying_capacity)})

  output$pop_trend_type_info <- renderText({paste0("Type de Tendance de pop : ", input$lambda_input_type)})
  output$pop_trend_mean_info <- renderText({paste0("Moyenne Tendance de pop : ", param$pop_growth_mean)})
  output$pop_trend_se_info <- renderText({paste0("Ecart-type Tendance de pop : ", input$pop_growth_se)})
}
# End server



