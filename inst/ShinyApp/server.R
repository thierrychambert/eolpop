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
    shinyjs::hide("fatalities_mean")
    shinyjs::hide("fatalities_se")
    shinyjs::hide("fatalities_lower")
    shinyjs::hide("fatalities_upper")
    shinyjs::hide("fatalities_number_expert")
    shinyjs::hide("fatalities_mat_expert")
    shinyjs::hide("fatalities_run_expert")
    shinyjs::hide("farm_number_cumulated")
    shinyjs::hide("fatalities_mat_cumulated")
    shinyjs::hide("fatalities_vec_scenario")

    shinyjs::hide("pop_size_lower")
    shinyjs::hide("pop_size_upper")
    shinyjs::hide("pop_size_mean")
    shinyjs::hide("pop_size_se")
    shinyjs::hide("pop_size_number_expert")
    shinyjs::hide("pop_size_mat_expert")
    shinyjs::hide("pop_size_run_expert")

    shinyjs::hide("pop_growth_lower")
    shinyjs::hide("pop_growth_upper")
    shinyjs::hide("pop_growth_mean")
    shinyjs::hide("pop_growth_se")
    shinyjs::hide("pop_growth_number_expert")
    shinyjs::hide("pop_growth_mat_expert")
    shinyjs::hide("pop_growth_run_expert")
    shinyjs::hide("pop_trend")
    shinyjs::hide("pop_trend_strength")

    shinyjs::hide("carrying_capacity_lower")
    shinyjs::hide("carrying_capacity_upper")
    shinyjs::hide("carrying_capacity_mean")
    shinyjs::hide("carrying_capacity_se")
    shinyjs::hide("carrying_cap_number_expert")
    shinyjs::hide("carrying_cap_mat_expert")
    shinyjs::hide("carrying_cap_run_expert")

    shinyjs::hide("mat_fill_vr")
    shinyjs::hide("vr_mat_number_age_classes")

    #------------
    # Show some
    #------------
    # Show inputs for fatalities part
    if(input$button_fatalities%%2 == 1){
      #shinyjs::show("fatal_constant")

      # Show inputs for single farm option (non-cumulated impacts)
      if(input$analysis_choice == "single_farm"){
        shinyjs::show("fatalities_input_type")

        if(input$fatalities_input_type == "itvl"){
          shinyjs::show("fatalities_lower")
          shinyjs::show("fatalities_upper")
        }
        if(input$fatalities_input_type == "val"){
          shinyjs::show("fatalities_mean")
          shinyjs::show("fatalities_se")
        }
        if(input$fatalities_input_type == "eli_exp"){
          shinyjs::show("fatalities_number_expert")
          shinyjs::show("fatalities_mat_expert")
          shinyjs::show("fatalities_run_expert")
        }
      }

      # Show inputs for cumulated impacts option
      if(input$analysis_choice == "cumulated"){
        shinyjs::hide("fatalities_input_type")
        shinyjs::show("farm_number_cumulated")
        shinyjs::show("fatalities_mat_cumulated")
      }

      # Show inputs for multiple scenario
      if(input$analysis_choice == "multi_scenario"){
        shinyjs::hide("fatalities_input_type")
        shinyjs::show("fatalities_vec_scenario")
      }

    }

    # Show inputs for population size part
    if(input$button_pop_size%%2 == 1){
      shinyjs::show("pop_size_input_type")
      if(input$pop_size_input_type == "itvl"){
        shinyjs::show("pop_size_lower")
        shinyjs::show("pop_size_upper")
      }
      if(input$pop_size_input_type == "val"){
        shinyjs::show("pop_size_mean")
        shinyjs::show("pop_size_se")
      }
      if(input$pop_size_input_type == "eli_exp"){
        shinyjs::show("pop_size_number_expert")
        shinyjs::show("pop_size_mat_expert")
        shinyjs::show("pop_size_run_expert")
      }
    }

    # Show inputs for population trend/growth part
    if(input$button_pop_growth%%2 == 1){
      shinyjs::show("pop_growth_input_type")

      if(input$pop_growth_input_type == "itvl"){
        shinyjs::show("pop_growth_lower")
        shinyjs::show("pop_growth_upper")
      }
      if(input$pop_growth_input_type == "val"){
        shinyjs::show("pop_growth_mean")
        shinyjs::show("pop_growth_se")
      }
      if(input$pop_growth_input_type == "eli_exp"){
        shinyjs::show("pop_growth_number_expert")
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
      if(input$carrying_cap_input_type == "itvl"){
        shinyjs::show("carrying_capacity_lower")
        shinyjs::show("carrying_capacity_upper")
      }
      if(input$carrying_cap_input_type == "val"){
        shinyjs::show("carrying_capacity_mean")
        shinyjs::show("carrying_capacity_se")
      }
      if(input$carrying_cap_input_type == "eli_exp"){
        shinyjs::show("carrying_cap_number_expert")
        shinyjs::show("carrying_cap_mat_expert")
        shinyjs::show("carrying_cap_run_expert")
      }
    }

    # Show inputs vital rates part
    if(input$button_vital_rates%%2 == 1){
      shinyjs::show("mat_fill_vr")
      shinyjs::show("vr_mat_number_age_classes")
    }

  }) # en observe show/hide
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  #####

  ##############################################
  ##  Reactive values
  ##--------------------------------------------
  out <- reactiveValues(run = NULL, msg = NULL, analysis_choice = NULL)

  rv <- reactiveValues(distAVG = NULL, dist = NULL)

  ready <- reactiveValues(fatalities = TRUE, pop_size = TRUE, pop_growth = TRUE, carrying_capacity = TRUE)

  param <- reactiveValues(N1 = NULL,
                          nsim = NULL,
                          cumulated_impacts = FALSE,

                          fatalities_mean = NULL,
                          fatalities_mean_nb = NULL,
                          fatalities_se = NULL,
                          fatalities_se_nb = NULL,
                          onset_time = NULL,
                          onset_year = NULL,
                          out_fatal = NULL,

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

                          carrying_capacity_mean = NULL,
                          carrying_capacity_se = NULL,


                          theta = NULL,
                          rMAX_species = NULL,

                          model_demo = NULL,
                          time_horizon = NULL,
                          coeff_var_environ = NULL,
                          fatal_constant = NULL,

                          fatalities_eli_result = NULL,
                          pop_size_eli_result = NULL,
                          pop_growth_eli_result = NULL,
                          carrying_cap_eli_result = NULL
  )
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###



  #####

  ##############################################
  ## Define some functions
  ##-------------------------------------------
  # Get lambda from +/-X% growth rate
  make_lambda <- function(pop_growth)  1 + (pop_growth/100)
  #####

  #####
  ##------------------------------------------
  ## Update elicitation matrices
  ##------------------------------------------

  ###############################
  ## Cumulated Impacts Matrix
  ##-----------------------------
  observeEvent({
    input$farm_number_cumulated
  }, {
    req(input$farm_number_cumulated > 0)
    current_mat <- input$fatalities_mat_cumulated
    n_farm <- input$farm_number_cumulated
    if(n_farm > nrow(current_mat)){
      fill_mat <- c(as.vector(t(current_mat)), rep(NA,(3*(n_farm-nrow(current_mat)))))
    }else{
      fill_mat <- as.vector(t(current_mat[1:n_farm,]))
    }
    updateMatrixInput(session, inputId = "fatalities_mat_cumulated",
                      value =  matrix(fill_mat, nrow = n_farm, ncol = 3, byrow = TRUE,
                                      dimnames = list(paste("Parc", c(1:n_farm)),
                                                      c("Moyenne",
                                                        "Erreur-type",
                                                        "Année (début)"))))
  })
  #####

  ########################
  ## Fatalities Matrix
  ##----------------------
  observeEvent({
    input$fatalities_number_expert
  }, {
    req(input$fatalities_number_expert > 0)
    current_mat <- input$fatalities_mat_expert
    n_experts <- input$fatalities_number_expert
    if(n_experts > nrow(current_mat)){
      fill_mat <- c(as.vector(t(current_mat)), rep(NA,(5*(n_experts-nrow(current_mat)))))
    }else{
      fill_mat <- as.vector(t(current_mat[1:n_experts,]))
    }
    updateMatrixInput(session, inputId = "fatalities_mat_expert",
                      value = matrix(fill_mat, nrow = n_experts, ncol = 5, byrow = TRUE,
                                     dimnames = list(paste0("#", 1:n_experts),
                                                     c("Poids", "Min", "Best", "Max", "% IC" ))
                                     )
                      )
  })
  #####

  ########################
  ## Pop Size Matrix
  ##----------------------
  observeEvent({
    input$pop_size_number_expert
  }, {
    req(input$pop_size_number_expert > 0)
    current_mat <- input$pop_size_mat_expert
    n_experts <- input$pop_size_number_expert
    if(n_experts > nrow(current_mat)){
      fill_mat <- c(as.vector(t(current_mat)), rep(NA,(5*(n_experts-nrow(current_mat)))))
    }else{
      fill_mat <- as.vector(t(current_mat[1:n_experts,]))
    }
    updateMatrixInput(session, inputId = "pop_size_mat_expert",
                      value = matrix(fill_mat, nrow = n_experts, ncol = 5, byrow = TRUE,
                                     dimnames = list(paste0("#", 1:n_experts),
                                                     c("Poids", "Min", "Best", "Max", "% IC" ))
                      )
    )
  })
  #####

  ########################
  ## Pop Growth Matrix
  ##----------------------
  observeEvent({
    input$pop_growth_number_expert
  }, {
    req(input$pop_growth_number_expert > 0)
    current_mat <- input$pop_growth_mat_expert
    n_experts <- input$pop_growth_number_expert
    if(n_experts > nrow(current_mat)){
      fill_mat <- c(as.vector(t(current_mat)), rep(NA,(5*(n_experts-nrow(current_mat)))))
    }else{
      fill_mat <- as.vector(t(current_mat[1:n_experts,]))
    }
    updateMatrixInput(session, inputId = "pop_growth_mat_expert",
                      value = matrix(fill_mat, nrow = n_experts, ncol = 5, byrow = TRUE,
                                     dimnames = list(paste0("#", 1:n_experts),
                                                     c("Poids", "Min", "Best", "Max", "% IC" ))
                      )
    )
  })
  #####

  ############################
  ## Carrying Capacity Matrix
  ##--------------------------
  observeEvent({
    input$carrying_cap_number_expert
  }, {
    req(input$carrying_cap_number_expert > 0)
    current_mat <- input$carrying_cap_mat_expert
    n_experts <- input$carrying_cap_number_expert
    if(n_experts > nrow(current_mat)){
      fill_mat <- c(as.vector(t(current_mat)), rep(NA,(5*(n_experts-nrow(current_mat)))))
    }else{
      fill_mat <- as.vector(t(current_mat[1:n_experts,]))
    }
    updateMatrixInput(session, inputId = "carrying_cap_mat_expert",
                      value = matrix(fill_mat, nrow = n_experts, ncol = 5, byrow = TRUE,
                                     dimnames = list(paste0("#", 1:n_experts),
                                                     c("Poids", "Min", "Best", "Max", "% IC" ))
                      )
    )
  })
  #####



  #####
  ##--------------------------------------------
  ##  Run expert elicitation
  ##--------------------------------------------
  # Function to run the elication analysis
  func_eli <- function(mat_expert){
    t_mat_expert <- t(mat_expert)
    vals <- t_mat_expert[2:4,] %>% apply(., 2, sort)
    Cp <- t_mat_expert[5,] %>% sapply(., min, 0.99) %>% sapply(., max, 0.2)
    weights <- t_mat_expert[1,]

    out <- tryCatch(
      elicitation(vals, Cp, weights), error = function(e)
        return(NULL)
        #message("Erreur : certaines valeurs dans la matrice d'experts n'ont pas de sens")
      )

    if(!is.null(out)){
      OUT <- list(out = out, mean = out$mean_smooth, SE = sqrt(out$var_smooth))
    }else{
      OUT <- list(out = NA, mean = NA, SE = NA)
    }
    return(OUT)
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
    if(all(!is.na(input$pop_growth_mat_expert))){

      lambda_mat_expert <- input$pop_growth_mat_expert
      lambda_mat_expert[,2:4] <- make_lambda(lambda_mat_expert[,2:4])

      ## run elicitation analysis
      param$pop_growth_eli_result <- func_eli(lambda_mat_expert)

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

      ## show output
      if(!is.na(param$carrying_cap_eli_result$out)){
        output$title_distri_plot <- renderText({ "Capacité de charge" })
        output$distri_plot <- renderPlot({ plot_expert(param$carrying_cap_eli_result$out, show_se = FALSE) })
      }else {
        output$title_distri_plot <- renderText({ "Erreur : certaines valeurs dans la matrice d'experts n'ont pas de sens" })
      }

    } else {
      param$carrying_cap_eli_result <- NULL
      print("missing value")
      output$title_distri_plot <- renderText({ "Des valeurs sont manquantes dans la table 'experts'" })
    } # end if
  }) # end observeEvent

  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

  #####


  #####
  ##--------------------------------------------
  ##  Display parameter distribution
  ##--------------------------------------------

  # Function to plot a gamma distribution
  plot_gamma <- function(mu, se, show_mode = TRUE, show_mean = TRUE, show_se = TRUE, ...){

    ## Define shape and scale parameter of gamma distribution
    shape = (mu/se)^2
    scale = se^2/mu

    ## Plot the curve
    par(mar = c(5, 4, 6, 2))
    curve(dgamma(x, shape=shape, scale=scale), from = max(0,mu-3*se), to = mu+3*se, lwd = 3, col = "darkblue", yaxt = "n",
          ylab = "", xlab = "Valeur du paramètre", cex.lab = 1.2)
    mtext(text = "Densité de probabilité", side = 2, line = 2, cex = 1.2)

    # show mode
    MU <- (shape-1)*scale
    y_MU <- dgamma(x = MU, shape = shape, scale = scale)
    xx <- qgamma(p = c(0.01,0.99), shape = shape, scale = scale)
    clip(xx[1], xx[2], -100, y_MU)
    abline(v = MU, lwd = 3, col = "darkblue")

    # show mean
    y_mu <- dgamma(x = mu, shape = shape, scale = scale)
    clip(xx[1], xx[2], -100, y_mu)
    abline(v = mu, lwd = 2, col = "darkblue", lty = 2)

    if(show_mode) mtext(text = paste("Mode = ", round(MU, 2)), side = 3, line = 4, cex = 1.2, adj = 0)
    if(show_mean) mtext(text = paste("Moyenne = ", round(mu, 2)), side = 3, line = 2.5, cex = 1.2, adj = 0)
    if(show_se) mtext(text = paste("Erreur-type = ", round(se, 3)), side = 3, line = 1, cex = 1.2, adj = 0)
  } # end function plot_gamma

  plot_gamma_cumulated_impacts <- function(mu, se, nparc, ...){
    ## Define shape and scale parameter of gamma distribution
    shape = (mu/se)^2
    scale = se^2/mu

    ## Define x and y lim
    xx = yy = list()
    for(j in 1:nparc){
      xx[[j]] = seq(from = max(0,mu[j]-4*se[j]), to = mu[j]+4*se[j], length.out = 1e3)
      yy[[j]] = dgamma(xx[[j]], shape=shape[j], scale=scale[j])
    }

    ylim = c(min(unlist(yy)), max(unlist(yy))*1.4)
    xlim = c(min(unlist(xx)), max(unlist(xx)))

    ## Plot
    j=1
    curve(dgamma(x, shape=shape[j], scale=scale[j]),
          from = max(0,mu[j]-4*se[j]), to = mu[j]+4*se[j], n = 1e4,
          xlim = xlim, ylim = ylim,
          lwd = 3, col = j, yaxt = "n", xaxt = "n",
          #xaxp = c(round(xlim[1]), round(xlim[2]), n = 10),
          ylab = "", xlab = "Valeur du paramètre", cex.lab = 1.2)
    axis(side = 1, at = seq(round(xlim[1]), round(xlim[2]),
                            by = max(round((round(xlim[2])-round(xlim[1]))/10),1) ))
    mtext(text = "Densité de probabilité", side = 2, line = 2, cex = 1.2)

    y1 <- dgamma(x = mu[j], shape = shape[j], scale = scale[j])
    segments(x0 = mu[j], y0 = 0, y1 = y1, lty = 2, lwd = 3, col = j)
    points(x = mu[j], y = y1, pch = 19, cex = 1.5, col = j)

    for(j in 2:nparc){
      curve(dgamma(x, shape=shape[j], scale=scale[j]),
            from = max(0,mu[j]-4*se[j]), to = mu[j]+4*se[j], n = 1e4,
            lwd = 3, col = j, yaxt = "n",
            ylab = "", xlab = "Valeur du paramètre", cex.lab = 1.2, add = TRUE)

      y1 <- dgamma(x = mu[j], shape = shape[j], scale = scale[j])
      segments(x0 = mu[j], y0 = 0, y1 = y1, lty = 2, lwd = 3, col = j)
      points(x = mu[j], y = y1, pch = 19, cex = 1.5, col = j)
    }

    legend(x = xlim[1], y = ylim[2], legend = paste("Parc", 1:nparc),
           lwd = 3, col = 1:nparc, text.col = 1:nparc, cex = 1.5,
           bty = "n", horiz = TRUE)
  } # end function plot_gamma_cumulated_impacts

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

    ## 1. When analysis = single farm
    if(input$analysis_choice == "single_farm"){

      # Show from input values: if button is ON and input_type is set on "value" or "itvl" (thus not "eli_exp")
      if(input$button_fatalities%%2 == 1 & input$fatalities_input_type != "eli_exp"){
        output$title_distri_plot <- renderText({ "Mortalités annuelles" })

        output$distri_plot <- renderPlot({
          req(param$fatalities_mean, param$fatalities_se > 0)
          if(input$fatalities_input_type == "itvl"){
            req(input$fatalities_lower, input$fatalities_upper)
            plot_gamma(mu = tail(param$fatalities_mean, -1), se = tail(param$fatalities_se, -1))
          }else{
            req(input$fatalities_mean, input$fatalities_se)
            plot_gamma(mu = tail(param$fatalities_mean, -1), se = tail(param$fatalities_se, -1))
          }
        })

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

    ## 2. When analysis = cumulated impacts
    }else{
      if(input$analysis_choice == "cumulated"){
        output$title_distri_plot <- renderText({ "Mortalités annuelles par parc (impacts cumulés)" })
        # Plot: note we use the "NULL + delay" sequence only to avoid error message in R console
        output$distri_plot <- NULL
        delay(5,
              output$distri_plot <- renderPlot({
                req(all(!is.na(input$fatalities_mat_cumulated[,1])), all(input$fatalities_mat_cumulated[,2] > 0))
                plot_gamma_cumulated_impacts(mu = input$fatalities_mat_cumulated[,1],
                                             se = input$fatalities_mat_cumulated[,2],
                                             nparc = input$farm_number_cumulated)
              })
        )
      }else{
        ## 3. When analysis = multi_scenarios
        output$title_distri_plot <- renderText({ "Pas de graphe (pas d'incertitude dans le cas 'mulitple scénarios')" })
        output$distri_plot <- NULL
      } # end "else"

    } # end "if"
  }, ignoreInit = FALSE)


  ########################
  ## Population size
  ##----------------------
  observeEvent({
    input$button_pop_size
    input$pop_size_input_type
  },{
    # Show from input values: if button is ON and input_type is set on "value"
    if(input$button_pop_size%%2 == 1 & input$pop_size_input_type != "eli_exp"){
      output$title_distri_plot <- renderText({ "Taille initiale de la population" })

      output$distri_plot <- renderPlot({
        req(param$pop_size_mean, param$pop_size_se > 0)
        plot_gamma(mu = param$pop_size_mean, se = param$pop_size_se)
      })

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

    # Show from input values: if button is ON and input_type is set on "value" or "interval"
    if(input$button_pop_growth%%2 == 1 & input$pop_growth_input_type != "eli_exp" & input$pop_growth_input_type != "trend"){
      output$title_distri_plot <- renderText({ "Taux de croissance de la population" })

      output$distri_plot <- renderPlot({
        req(param$pop_growth_mean, param$pop_growth_se > 0)
        plot_gamma(mu = param$pop_growth_mean, se = param$pop_growth_se)
      })

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
    # Show from input values: if button is ON and input_type is set on "value"
    if(input$button_carrying_cap%%2 == 1 & input$carrying_cap_input_type != "eli_exp"){
      output$title_distri_plot <- renderText({ "Capacité de charge" })

      output$distri_plot <- renderPlot({
        req(param$carrying_capacity_mean, param$carrying_capacity_se > 0)
        plot_gamma(mu = param$carrying_capacity_mean, se = param$carrying_capacity_se)
      })

    } else {
      # Show from elicitation expert: if button is ON and input_type is set on "expert elicitation"
      if(input$button_carrying_cap%%2 == 1 & input$carrying_cap_input_type == "eli_exp"){
        if(!is.null(param$carrying_cap_eli_result)){
          if(!is.na(param$carrying_cap_eli_result$out)){
            output$title_distri_plot <- renderText({ "Capacité de charge" })
            output$distri_plot <- renderPlot({ plot_expert(param$carrying_cap_eli_result$out) })
          }else{
            output$title_distri_plot <- renderText({ "Erreur" })
            output$distri_plot <- NULL
          }
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
  #####


  #####
  ##-------------------------------------------------
  ##  Display parameter values (on the side panel)
  ##-------------------------------------------------
  #################################
  ## Fatalities
  ##-------------------------------
  ## UNIT
  output$fatalities_unit_info <- renderText({
    if(!is.null(input$fatalities_unit)){
      if(input$fatalities_unit == "h"){
        paste0("Taux de mortalité")
      } else {
        paste0("Nombre de mortalités")
      }
    }
  })

  ## Values
  output$fatalities_mean_info <- renderText({
    if(input$fatalities_unit == "h") add_perc <- "%" else add_perc <- ""
    paste0(c("Moyenne : ",
             paste0(tail(param$fatalities_mean, -1), add_perc, collapse = ", ")
    ), collapse = "")
  })


  output$fatalities_se_info <- renderText({
    if(input$fatalities_unit == "h") add_perc <- "%" else add_perc <- ""
    paste0(c("Erreur-type : ",
             paste0(tail(param$fatalities_se, -1), add_perc, collapse = ", ")
    ), collapse = "")
  })


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

  ## Show Popsize by age (table)
  # Function to create the table
  make_mat_popsizes <- function(data_sf, species, pop_size, pop_size_unit, survivals, fecundities){
    nam <- data_sf %>%
      filter(NomEspece == species) %>%
      select(classes_age) %>%
      unlist %>%
      as.vector

    matrix(round(pop_vector(pop_size = pop_size, pop_size_type = pop_size_unit, s = survivals, f = fecundities)),
           nrow = 1,
           dimnames = list("Effectifs", nam)
    )
  }

  # Display the table       (Note the delay : piece is just there to avoid an error message - time for parameters to be "loaded in")
  delay(ms = 200,
        output$pop_size_by_age <- renderTable({
          if(any(is.na(param$survivals)) | any(is.na(param$fecundities))){
            matrix("Valeurs de survies et/ ou de fécondités manquantes",
                   nrow = 1, dimnames = list(NULL, "Erreur"))
          }else{
            make_mat_popsizes(data_sf = data_sf, species = input$species_choice, pop_size = param$pop_size_mean,
                              pop_size_unit = input$pop_size_unit, s = param$survivals, f = param$fecundities)
          } # end if
        },
        width = "500px",
        rownames = FALSE,
        digits = 0)
    )


  #################################
  ## Population growth
  ##-------------------------------
  output$pop_growth_mean_info <- renderText({  paste0("Moyenne : ", param$pop_growth_mean) })
  output$pop_growth_se_info <- renderText({  paste0("Erreur-type : ", param$pop_growth_se) })

  #################################
  ## Carrying capacity
  ##-------------------------------
  # UNIT (like pop size)
  ## UNIT
  output$carrying_capacity_unit_info <- renderText({
    if(!is.null(param$pop_size_unit)){
      if(input$carrying_cap_input_type == "no_K"){
        "Pas de capacité de charge (K = infini)"
      }else{
        if(param$pop_size_unit == "Npair"){
          paste0("Nombre de couple")
        } else {
          paste0("Effectif total")
        }
      }
    }
  })

  ## VALUES
  output$carrying_capacity_mean_info <- renderText({
    if(input$carrying_cap_input_type == "no_K"){
      NULL
    }else{
      paste0("Moyenne : ", param$carrying_capacity_mean)
    }
  })

  output$carrying_capacity_se_info <- renderText({
    if(input$carrying_cap_input_type == "no_K"){
      NULL
    }else{
      paste0("Erreur-type : ", param$carrying_capacity_se)
    }
  })



  #################################
  ## Vital rates
  ##-------------------------------
  # Function to create the matrix
  make_mat_vr <- function(data_sf, species){
    out_mat <- data_sf %>%
      filter(NomEspece == species) %>%
      select(classes_age, survie, fecondite)
    return(out_mat)
  }

  # Update the vital rate matrix (mat_fill_vr) when changing the number of age classes
  observeEvent({
    input$vr_mat_number_age_classes
  }, {
    req(input$vr_mat_number_age_classes)
    number_age_class <- input$vr_mat_number_age_classes
    updateMatrixInput(session, inputId = "mat_fill_vr",
                        value = matrix(data = NA,
                                       nrow = number_age_class,
                                       ncol = 2,
                                       dimnames = list(c(paste("Age", (1:number_age_class)-1)), c("Survie", "Fécondité"))))
  }) # end observeEvent



  # Update the vital rate matrix (mat_fill_vr) when changing species in the list
  observeEvent({
    input$species_choice
  }, {

    if(input$species_choice == "Espèce générique") {

      number_age_class <- input$vr_mat_number_age_classes
      updateMatrixInput(session, inputId = "mat_fill_vr",
                        value = matrix(data = NA,
                                       nrow = number_age_class,
                                       ncol = 2,
                                       dimnames = list(c(paste("Age", (1:number_age_class)-1)), c("Survie", "Fécondité"))))
    } else {

      tab_species <- make_mat_vr(data_sf = data_sf, species = input$species_choice)

      if(all(is.na(tab_species))) {
        number_age_class <- input$vr_mat_number_age_classes
        updateMatrixInput(session, inputId = "mat_fill_vr",
                          value = matrix(data = NA,
                                         nrow = number_age_class,
                                         ncol = 2,
                                         dimnames = list(c(paste("Age", (1:number_age_class)-1)), c("Survie", "Fécondité"))))

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




  # Display vital rates output table
  output$vital_rates_info <- renderTable({
    input$mat_fill_vr
  }, rownames = TRUE)

  # Display intrinsic lambda (based solely on Leslie matrix)
  delay(ms = 300,
        output$lambda0_info <- renderText({
          req(all(!is.na(input$mat_fill_vr)))
          lam <- lambda(build_Leslie(s = input$mat_fill_vr[,1], f = input$mat_fill_vr[,2]))
          taux <- round(lam-1,2)*100
          if(taux < 0) Text <- "Déclin : " else Text <- "Croissance : "
          if(taux == 0) Text <- "Population stable : "
          paste0(Text, taux, "% par an")
        })
  )


  #####

  #################################
  ## Dispersal
  ##-------------------------------
  observeEvent({
    input$species_choice
  }, {
    distAVG <- species_data %>%
      filter(NomEspece == input$species_choice) %>%
      select(DistDispMoyKM)

    rv$distAVG <- round(distAVG, 1)

    rv$dist <- c(round(-log(0.03)*distAVG, 1),
                 round(-log(0.05)*distAVG, 1),
                 round(-log(0.10)*distAVG, 1))
  })

  output$dispersal_mean_info <- renderText({
    paste0("Distance moyenne de dispersion : ", rv$distAVG, " km")
    })

  output$dispersal_d03p_info <- renderText({
    paste0("Seuil de distance équiv. 3% de dispersion : ", rv$dist[1], " km")
  })

  output$dispersal_d05p_info <- renderText({
    paste0("Seuil de distance équiv. 5% de dispersion : ", rv$dist[2], " km")
  })

  output$dispersal_d10p_info <- renderText({
    paste0("Seuil de distance équiv. 10% de dispersion : ", rv$dist[3], " km")
  })

  #####



  #####
  ##--------------------------------------------
  ## Select parameter values for simulations
  ##--------------------------------------------
  # Functions to calculate mean and SD from lower & upper values
  get_mu <- function(lower, upper) (lower + upper)/2
  get_sd <- function(lower, upper, coverage) ((abs(upper - lower)/2))/qnorm(1-((1-coverage)/2))

  #################################
  ## Cumulated impacts or not ?
  ##-------------------------------
  observeEvent({
    input$run
  }, {
    if(input$analysis_choice == "cumulated"){
      param$cumulated_impacts = TRUE
    } else {
      param$cumulated_impacts = FALSE
    } # end if
  }) # end observeEvent

  #################################
  ## Fatalities
  ##-------------------------------
  observe({
    # Case 1 : Not cumulated effects (if1)
    if(input$analysis_choice == "single_farm"){

      # Case 1.1 : Values from expert elicitation (if2)
      if(input$fatalities_input_type == "eli_exp"){
        if(!(is.null(param$fatalities_eli_result))){
          param$fatalities_mean <- c(0, round(param$fatalities_eli_result$mean, 2))
          param$onset_time <- NULL
          param$fatalities_se <- c(0, round(param$fatalities_eli_result$SE, 3))
          ready$fatalities <- TRUE
        } else {
          ready$fatalities <- FALSE
        }

      } else {

        if(input$fatalities_input_type == "val"){
          # Case 1.2 : Values directly provided as mean & SE
          param$fatalities_mean <- c(0, input$fatalities_mean)
          param$onset_time <- NULL
          param$fatalities_se <- c(0, input$fatalities_se)
          ready$fatalities <- TRUE

        }else{
          # Case 1.3 : Values directly provided as lower/upper interval
          param$fatalities_mean <- c(0, round(get_mu(lower = input$fatalities_lower, upper = input$fatalities_upper), 2))
          param$onset_time <- NULL
          param$fatalities_se <- c(0, round(get_sd(lower = input$fatalities_lower, upper = input$fatalities_upper, coverage = CP), 3))
          ready$fatalities <- TRUE
        } # end (if3)

      } # end (if2)

      # Case 2 : Cumulated effects
    } else {
      if(input$analysis_choice == "cumulated"){
        ready$fatalities <- TRUE
        param$fatalities_mean <- c(0, input$fatalities_mat_cumulated[,1])
        param$fatalities_se <- c(0, input$fatalities_mat_cumulated[,2])
        param$onset_year <- c(min(input$fatalities_mat_cumulated[,3]), input$fatalities_mat_cumulated[,3])
        param$onset_time <- param$onset_year - min(param$onset_year) + 1

        # Case 3 : Scenarios
      }else{
        req(input$fatalities_vec_scenario)
        vec01 <- as.numeric(unlist(strsplit(input$fatalities_vec_scenario, " ")))
        param$fatalities_mean <- c(0, vec01)
        param$fatalities_se <- rep(0, length(vec01)+1)
        param$onset_time <- NULL
        ready$fatalities <- TRUE
      }





    } # end (if1)

  }) # end observe
  ###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###


  # Make sure fatalities are expressed as "number" (not rate) for the run_simul function
  se_prod2 <- function(mu1, se1, mu2, se2) sqrt((se1^2 * se2^2) + (se1^2 * mu2^2) + (mu1^2 * se2^2))

  observeEvent({
    input$run
  },{
    if(input$fatalities_unit == "h"){
      pop_size_tot <- sum(pop_vector(pop_size = param$pop_size_mean, pop_size_type = param$pop_size_type, s = param$survivals, f = param$fecundities)[-1])
      param$fatalities_mean_nb <- (param$fatalities_mean/100) * pop_size_tot
      param$fatalities_se_nb <- se_prod2(mu1 = param$fatalities_mean/100,
                                         se1 = param$fatalities_se/100,
                                         mu2 = pop_size_tot,
                                         se2 = (pop_size_tot/param$pop_size_mean) * param$pop_size_se)
    }else{
      param$fatalities_mean_nb <- param$fatalities_mean
      param$fatalities_se_nb <- param$fatalities_se
    }
  })


  #################################
  ## Population size
  ##-------------------------------
  observe({
    # Case 1 : Values from expert elicitation
    if(input$pop_size_input_type == "eli_exp"){
      if(!(is.null(param$pop_size_eli_result))){
        param$pop_size_mean <- round(param$pop_size_eli_result$mean, 1)
        param$pop_size_se <- round(param$pop_size_eli_result$SE, 1)
        ready$pop_size <- TRUE
      } else {
        ready$pop_size <- FALSE
      }

    } else {

      if(input$pop_size_input_type == "val"){
        # Case 2 : Values directly provided as mean & SE
        ready$pop_size <- TRUE
        param$pop_size_mean <- round(input$pop_size_mean, 1)
        param$pop_size_se <- round(input$pop_size_se, 1)

      }else{
        # Case 3 : Values directly provided as lower/upper interval
        ready$pop_size <- TRUE
        param$pop_size_mean <- round(get_mu(lower = input$pop_size_lower, upper = input$pop_size_upper), 1)
        param$pop_size_se <- round(get_sd(lower = input$pop_size_lower, upper = input$pop_size_upper, coverage = CP), 1)
      } # end (if3)

    }
    param$pop_size_unit <- input$pop_size_unit
  })


  #################################
  ## Population growth
  ##-------------------------------
  observe({
    # Case 1 : Values from expert elicitation
    if(input$pop_growth_input_type == "eli_exp"){
      if(!(is.null(param$pop_growth_eli_result))){
        param$pop_growth_mean <- round(min(1 + param$rMAX_species, round(param$pop_growth_eli_result$mean, 2)), 4)
        param$pop_growth_se <- round(param$pop_growth_eli_result$SE, 5)
        ready$pop_growth <- TRUE
      } else {
        ready$pop_growth <- FALSE
      }

    } else {

      # Case 2 : Trend information
      if(input$pop_growth_input_type == "trend"){
        ready$pop_growth <- TRUE

        if(input$pop_trend == "growth") {
          if(input$pop_trend_strength == "weak") {
            param$pop_growth_mean <- growth_weak
          } else if(input$pop_trend_strength == "average"){
            param$pop_growth_mean <- growth_average
          } else {
            param$pop_growth_mean <- growth_strong
          }
        } else if(input$pop_trend == "decline"){
          if(input$pop_trend_strength == "weak") {
            param$pop_growth_mean <- decline_weak
          } else if(input$pop_trend_strength == "average"){
            param$pop_growth_mean <- decline_average
          } else {
            param$pop_growth_mean <- decline_strong
          }
        } else {
          param$pop_growth_mean <- pop_stable
        }
        param$pop_growth_se <- trend_se


        # Case 3 : Values directly provided (i.e., not from expert elicitation)
      } else {

        if(input$pop_growth_input_type == "val"){
          # Case 2 : Values directly provided as mean & SE
          ready$pop_growth <- TRUE
          param$pop_growth_mean <- round(min(1 + param$rMAX_species, make_lambda(input$pop_growth_mean)), 4)
          param$pop_growth_se <- round(input$pop_growth_se/100, 5)

        }else{
          # Case 3 : Values directly provided as lower/upper interval
          ready$pop_growth <- TRUE
          param$pop_growth_mean <- round(min(1 + param$rMAX_species,
                                             get_mu(lower = make_lambda(input$pop_growth_lower),
                                                          upper = make_lambda(input$pop_growth_upper)))
                                             , 4)
          param$pop_growth_se <- round(get_sd(lower = make_lambda(input$pop_growth_lower),
                                              upper = make_lambda(input$pop_growth_upper), coverage = CP), 5)
        } # end (if3)

      }
    }
  })



  #################################
  ## Carrying capacity
  ##------------------------------
  observe({
    if(input$carrying_cap_input_type == "eli_exp"){
      if(!is.null(param$carrying_cap_eli_result)){
        param$carrying_capacity_mean <- round(param$carrying_cap_eli_result$mean)
        param$carrying_capacity_se <- round(param$carrying_cap_eli_result$SE, 1)
        ready$carrying_capacity <- TRUE
      } else {
        ready$carrying_capacity <- FALSE
      }

    } else {

      if(input$carrying_cap_input_type == "no_K"){
        ready$carrying_capacity <- TRUE
        param$carrying_capacity_mean <- max(param$pop_size_mean*100, 1e30) # use a very large K
        param$carrying_capacity_se <- 0

      }else{
        # values: mean and se
        if(input$carrying_cap_input_type == "val"){
          ready$carrying_capacity <- TRUE
          param$carrying_capacity_mean <- input$carrying_capacity_mean
          param$carrying_capacity_se <- input$carrying_capacity_se

        }else{
          # lower/upper interval
          ready$carrying_capacity <- TRUE
          param$carrying_capacity_mean <- round(get_mu(lower = input$carrying_capacity_lower, upper = input$carrying_capacity_upper), 0)
          param$carrying_capacity_se <- round(get_sd(lower = input$carrying_capacity_lower, upper = input$carrying_capacity_upper, coverage = CP), 1)
        } # end (if3)

      }
    }
  })
  #############################################
  ## Survivals, fecundities
  ##-------------------------------------------
  observe({
    param$survivals <- input$mat_fill_vr[,1]
    param$fecundities <- input$mat_fill_vr[,2]
  }) # end observeEvent
  #####

  #############################################
  ## Calibration of survivals & fecundities
  ##-------------------------------------------
  observeEvent({
    input$run
  },{

    # We also define rMAX and theta here
    rMAX_species <- rMAX_spp(surv = tail(param$survivals,1), afr = min(which(param$fecundities != 0)))
    param$rMAX_species <- rMAX_species

    param$theta <- fixed_theta
    #param$theta <- theta_spp(rMAX_species)

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
  observeEvent({
    input$run
  }, {
    param$nsim <- input$nsim
    param$fatal_constant <- input$fatalities_unit
    param$time_horizon <- input$time_horizon

    # This condition is used to avoid wild population swings in fast-paced species
    if(max(param$fecundities) < 1.5){
      param$coeff_var_environ <- coeff_var_environ
    }else{
      param$coeff_var_environ <- 0
    }

  }) # Close observEvent


  observe ({
    param # to ensure up-to-date values are run
  }) # end observe
  #####

  #####
  ##-----------------------------------------------------------------------------------
  ##                                RUN SIMULATIONS
  ##-----------------------------------------------------------------------------------
  observeEvent({
    input$run
  }, {

    if(ready$fatalities & ready$pop_size & ready$pop_growth & ready$carrying_capacity){
      out$analysis_choice <- input$analysis_choice
      withProgress(message = 'Simulation progress', value = 0, {

        out$run <- run_simul_shiny(nsim = param$nsim,
                                   cumulated_impacts = param$cumulated_impacts,

                                   fatalities_mean = param$fatalities_mean_nb,
                                   fatalities_se = param$fatalities_se_nb,
                                   onset_time = param$onset_time,

                                   pop_size_mean = param$pop_size_mean,
                                   pop_size_se = param$pop_size_se,
                                   pop_size_type = param$pop_size_unit,

                                   pop_growth_mean = param$pop_growth_mean,
                                   pop_growth_se = param$pop_growth_se,

                                   survivals = param$s_calibrated,
                                   fecundities = param$f_calibrated,

                                   carrying_capacity_mean = param$carrying_capacity_mean,
                                   carrying_capacity_se = param$carrying_capacity_se,

                                   theta = param$theta,
                                   rMAX_species = param$rMAX_species,

                                   model_demo = NULL,
                                   time_horizon = param$time_horizon,
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

  #######################################################################
  ## Impact : individual farms (for "cumulated impact" analysis only)
  ##---------------------------------------------------------------------
  print_indiv_impact <- function(){
    req(out$run)
    res <- get_metrics(N = out$run$N, cumulated_impacts = TRUE)
    n_farm <- (dim(res$indiv_farm$impact)[3]-1)
    fil <- paste0(round(t(res$indiv_farm$impact[param$time_horizon, -2, -1]),2)*100, "%")
    matrix(fil,
           nrow = n_farm,
           dimnames = list(paste("Parc",1:n_farm), c("Impact", "IC (min)", "IC (max)"))
    )
  } # end function print_impact

  # Display title
  output$title_indiv_impact_result <- renderText({
    req(input$run > 0, out$analysis_choice == "cumulated")
    paste("Résultat : Impact de chaque parc éolien, estimé au bout de" , param$time_horizon, "ans")
  })

  # Display impact result (table)
  output$indiv_impact_table <- renderTable({
    req(input$run & out$analysis_choice == "cumulated")
    print_indiv_impact()
  }, rownames = TRUE)


  ##################################################
  ## Impact : GLOBAL (for all types of analysis)
  ##------------------------------------------------
  print_impact <- function(){
    req(out$run)
    res <- get_metrics(N = out$run$N, cumulated_impacts = FALSE)
    n_scen <- (dim(res$scenario$impact)[3]-1)

    RowNam <- NULL
    if(out$analysis_choice == "single_farm") RowNam <- c("Parc 1")
    if(out$analysis_choice == "cumulated") RowNam <- c("Parc 1", paste("... + Parc", (2:n_scen)))
    if(out$analysis_choice == "multi_scenario") RowNam <- paste("Scenario", (1:n_scen))

    fil <- paste0(round(t(res$scenario$impact[param$time_horizon, -2, -1]),2)*100, "%")
    matrix(fil,
           nrow = n_scen,
           dimnames = list(RowNam, c("Impact", "IC (min)", "IC (max)"))
    )
  } # end function print_impact

  # Display title
  output$title_impact_result <- renderText({
    req(input$run)
    paste("Résultat : Impact global estimé au bout de" , param$time_horizon, "ans")
  })

  # Display impact result (table)
  output$impact_table <- renderTable({
    req(input$run)
    print_impact()
  }, rownames = TRUE)


  #############################################
  ## Probability of extinction
  ##-------------------------------------------
  print_PrExt <- function(){
    req(out$run)
    res <- get_metrics(N = out$run$N, cumulated_impacts = FALSE)
    n_scen <- dim(res$scenario$impact)[3]

    RowNam <- NULL
    if(out$analysis_choice == "single_farm") RowNam <- c("Sans parc", "Avec parc")
    if(out$analysis_choice == "cumulated") RowNam <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
    if(out$analysis_choice == "multi_scenario") RowNam <- paste("Scenario", (1:n_scen)-1)

    fil <- paste0(round(t(res$scenario$Pext),2)*100, "%")
    matrix(fil,
           nrow = n_scen,
           dimnames = list(RowNam, c("Probabilité d'extinction"))
    )
  } # end function print_PrExt

  # Display title
  output$title_PrExt_result <- renderText({
    req(input$run)
    paste("Résultat : Probabilité d'extinction à", param$time_horizon, "ans")
  })

  # Display impact result (table)
  output$PrExt_table <- renderTable({
    req(input$run)
    print_PrExt()
  }, rownames = TRUE)


  #############################################
  ## Plot Impacts
  ##-------------------------------------------
  ## Function to plot the impact
  plot_out_impact <- function(){
    if(is.null(out$run)) {} else {

      n_scen <- dim(out$run$N)[3]
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Sans parc", "Avec parc")
      if(out$analysis_choice == "cumulated") Legend <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", (1:n_scen)-1)

      plot_impact(N = out$run$N, onset_year = param$onset_year, percent = TRUE,
                  xlab = "\nAnnée", ylab = "Impact relatif (%)\n", Legend = Legend)
      }
  }

  output$title_impact_plot <- renderText({
    if(input$run > 0){
      "Résultat : Impact relatif au cours du temps"
    }
  })

  output$impact_plot <- renderPlot({
    plot_out_impact()
  })

  #############################################
  ## Plot Demographic Trajectories
  ##-------------------------------------------
  # Function to plot trajectories
  plot_out_traj <- function(){
    if(is.null(out$run)) {
    } else {

      n_scen <- dim(out$run$N)[3]
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Sans parc", "Avec parc")
      if(out$analysis_choice == "cumulated") Legend <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", (1:n_scen)-1)

      plot_traj(N = out$run$N, onset_year = param$onset_year,
                xlab = "\nAnnée", ylab = "Taille de population\n", Legend = Legend, ylim = c(0, NA))}
  }

  output$title_traj_plot <- renderText({
    if(input$run > 0){
      "Graphique : Trajectoires démographiques"
    }
  })

  output$traj_plot <- renderPlot({
    plot_out_traj()
  })
  #####


  ###################################################################################
} # End server

