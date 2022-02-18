server <- function(input, output, session){


  ###################################################
  ##  Fixed parameters in the server environment
  ##-------------------------------------------------
  ## Load species list
  species_data <- read.csv("species_list.csv", sep = ";")

  ## Load survival and fecundities data
  data_sf_with_stars <- read.csv("data_sf.csv", sep = ";")
  data_sf <- data_sf_with_stars
  data_sf$survie <- as.numeric(gsub("p", "", data_sf$survie))
  data_sf$fecondite <- as.numeric(gsub("p", "", data_sf$fecondite))

  ## Replace p with stars
  data_sf_with_stars$survie <- gsub("p", "*", data_sf_with_stars$survie)
  data_sf_with_stars$fecondite <- gsub("p", "*", data_sf_with_stars$fecondite)

  # We define theta = 1 (same as in PBR) - for simplicity, given large uncertainty of real shape of density-dependence in nature
  fixed_theta = 1

  # Coefficient of environmental variation (SD)
  ## Environnmental variance set at 8%, based on values found for birds in the literature:
  ## (Saeher & Engen 2002) : between 7% et 14 ==> average : 10%
  ## (Sæther et al. 2005) : between 2.5% et 10% ==> average : 6%
  coeff_var_environ = sqrt(0.08) # SD ~28%

  # Coverage probability used for lower/upper interval input values
  CP = 0.99

  # Values of pop_growth (assumed), when the "trend" option is chosen
  growth_weak <- 1.04
  growth_average <- 1.08
  growth_strong <- 1.12

  decline_weak <- 0.97
  decline_average <- 0.94
  decline_strong <- 0.91

  pop_stable <- 1
  trend_se <- 0.05 # SE to use for pop_growth, when the "trend" option is chosen


  ##############################################
  ##  Reactive values
  ##--------------------------------------------
  out <- reactiveValues(run_time = NULL, msg = NULL, show_scen_options = FALSE)

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
                          pop_growth_mean_use = NULL,
                          pop_growth_se = NULL,

                          fecundities = NULL,
                          survivals = NULL,
                          s_calib0 = NULL,
                          f_calib0 = NULL,
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
      paste0("Nombre de couples")
    } else {
      paste0("Effectif total")
    }
  })


  ## SHOW Result TITLE
  output$hide_RES_TITLE <- eventReactive({
    input$run
  },{
    if(input$run > 0) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_RES_TITLE", suspendWhenHidden = FALSE)

  ## Outputs / show CI
  output$hide_show_CI <- eventReactive({
    input$run
  },{
    if(input$run > 0) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_show_CI", suspendWhenHidden = FALSE)


  ## Outputs / text results
  output$hide_results <- eventReactive({
    input$run
  },{
    if(input$run > 0) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_results", suspendWhenHidden = FALSE)

  ## Graph choices
  output$hide_graph_choice <- eventReactive({
    input$run
  },{
    if(input$run > 0) TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_graph_choice", suspendWhenHidden = FALSE)

  ## Graph result : PDF
  output$hide_graph_PDF <- eventReactive({
    input$run
    input$choose_graph
  },{
    if(input$run > 0 & input$choose_graph == "show_PDF") TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_graph_PDF", suspendWhenHidden = FALSE)


  ## Graph result : ECDF
  output$hide_graph_ECDF <- eventReactive({
    input$run
    input$choose_graph
  },{
    if(input$run > 0 & input$choose_graph == "show_ECDF") TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_graph_ECDF", suspendWhenHidden = FALSE)

  ## Side bar : risk A / quantile of CDF
  output$hide_risk_A <- eventReactive({
    input$run
    input$choose_graph
  },{
    if(input$run > 0 & input$choose_graph == "show_ECDF") TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_risk_A", suspendWhenHidden = FALSE)


  ## Graph result : impact_time
  output$hide_graph_impact_time <- eventReactive({
    input$run
    input$choose_graph
  },{
    if(input$run > 0 & input$choose_graph == "show_impact_time") TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_graph_impact_time", suspendWhenHidden = FALSE)


  ## Graph result : demog_proj
  output$hide_graph_demog_proj <- eventReactive({
    input$run
    input$choose_graph
  },{
    if(input$run > 0 & input$choose_graph == "show_demog_proj") TRUE else FALSE
  }, ignoreInit = TRUE)

  outputOptions(output, "hide_graph_demog_proj", suspendWhenHidden = FALSE)


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
    shinyjs::hide("button_use_custom_vr")

    shinyjs::hide("age_class_show")

    shinyjs::hide("show_scenario")
    if(out$show_scen_options){
      shinyjs::show("show_scenario")
    }

    #------------
    # Show some
    #------------
    # Show inputs for fatalities part
    if(input$button_fatalities%%2 == 1){

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
      shinyjs::show("vr_mat_number_age_classes")
      shinyjs::show("mat_fill_vr")
      shinyjs::show("button_use_custom_vr")
    }


    # Show radiobutton (output) for plot_traj graph
    if(input$run > 0){
      shinyjs::show("age_class_show")
    }

  }) # en observe show/hide
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
                                                      c("Valeur centrale",
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
                                                     c("Poids", "Min", "Best", "Max", "%IC" ))
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
                                                     c("Poids", "Min", "Best", "Max", "%IC" ))
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
                                                     c("Poids", "Min", "Best", "Max", "%IC" ))
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
                                                     c("Poids", "Min", "Best", "Max", "%IC" ))
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
        output$title_distri_plot <- renderText({ "Pas de graphe (pas d'incertitude dans le cas 'scénarios hypothétiques')" })
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
        paste0("Nombre de couples")
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
      filter(Species_fr == species) %>%
      select(classes_age) %>%
      unlist %>%
      as.vector

    matrix(round(pop_vector(pop_size = pop_size, pop_size_type = pop_size_unit, s = survivals, f = fecundities)),
           nrow = 1,
           dimnames = list("Effectifs", nam)
    )
  }

  # Display the table       (Note : the "delay" piece is just there to avoid an error message - time for parameters to be "loaded in")
  delay(ms = 200,
        output$pop_size_by_age <- renderTable({
          if(any(is.na(param$survivals)) | any(is.na(param$fecundities))){
            matrix("Valeurs de survies et/ ou de fécondités manquantes",
                   nrow = 1, dimnames = list(NULL, "Erreur"))
          }else{
            if(is.numeric(param$s_calib0) & is.numeric(param$f_calib0)){
              s_use <- param$s_calib0
              f_use <- param$f_calib0
            } else {
              s_use <- input$mat_fill_vr[,1]
              f_use <- input$mat_fill_vr[,2]
            }
            make_mat_popsizes(data_sf = data_sf, species = input$species_choice, pop_size = param$pop_size_mean,
                              pop_size_unit = input$pop_size_unit, s = s_use, f = f_use)
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
          paste0("Nombre de couples")
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
      filter(Species_fr == species) %>%
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
  }) # end observeEvent species_choice


  # Display vital rates output table
  delay(ms = 300,
        output$vital_rates_info <- renderTable({

          #input$mat_fill_vr
          ages <- paste("Age", (1:length(param$s_calib0))-1)
          matrix(data = c(param$s_calib0, param$f_calib0),
                 nrow = length(param$s_calib0),
                 ncol = 2,
                 dimnames = list(ages, c("Survie", "Fécondité"))
          )

        }, rownames = TRUE, striped = TRUE, align = 'c')
  )




  # Display intrinsic lambda (based solely on Leslie matrix)
  delay(ms = 300,
        output$lambda0_info <- renderText({
          req(all(!is.na(input$mat_fill_vr)))

          if(is.numeric(param$s_calib0) & is.numeric(param$s_calib0)){
            s_use <- param$s_calib0
            f_use <- param$f_calib0
          } else {
            s_use <- input$mat_fill_vr[,1]
            f_use <- input$mat_fill_vr[,2]
          }

          lam <- lambda(build_Leslie(s = s_use, f = f_use))
          taux <- round((lam-1)*100,1)
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
      filter(Species_fr == input$species_choice) %>%
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

        # Case 3 : Hypothetical Scenarios
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
        param$pop_growth_mean <- round(param$pop_growth_eli_result$mean, 4)
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
          param$pop_growth_mean <- round(make_lambda(input$pop_growth_mean), 4)
          param$pop_growth_se <- round(input$pop_growth_se/100, 5)

        }else{
          # Case 3 : Values directly provided as lower/upper interval
          ready$pop_growth <- TRUE
          param$pop_growth_mean <- round(get_mu(lower = make_lambda(input$pop_growth_lower),
                                                upper = make_lambda(input$pop_growth_upper)), 4)
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

    # for now, until calibration is really done # parameters to display in the table
    tab_species <- make_mat_vr(data_sf = data_sf_with_stars, species = input$species_choice)
    param$s_calib0 <- tab_species$survie
    param$f_calib0 <- tab_species$fecondite

    #param$s_calib0 <- input$mat_fill_vr[,1]
    #param$f_calib0 <- input$mat_fill_vr[,2]

  }) # end observeEvent
  #####

  #############################################
  ## Calibration of survivals & fecundities
  ##-------------------------------------------
  ## Information display : update when hand filling values
  observeEvent({
    input$button_use_custom_vr
  },{
    param$s_calib0 <- input$mat_fill_vr[,1]
    param$f_calib0 <- input$mat_fill_vr[,2]
  })

  ## Calibration 1 : just for information display
  observeEvent({
    input$button_calibrate_vr
  },{

    vr_calib0 <- calibrate_params(
      inits = init_calib(s = param$survivals, f = param$fecundities, lam0 = param$pop_growth_mean),
      f = param$fecundities, s = param$survivals, lam0 = param$pop_growth_mean
    )
    param$s_calib0 <- head(vr_calib0, length(param$survivals))
    param$f_calib0 <- tail(vr_calib0, length(param$fecundities))
  })

  ## Calibration 2 : for simulation run
  observeEvent({
    input$run
  },{

    # Define a limit for rMAX (theoretical formula from Niel & Lebreton 2005) or No limit (Inf)
    rMAX_species <- rMAX_spp(surv = tail(param$survivals,1), afr = min(which(param$fecundities != 0)))
     rMAX_species <- rMAX_species*1.5  # avoid too much constraint for the linear DD relationship
    param$rMAX_species <- rMAX_species

    # Apply rmax limit on population growth rate
    param$pop_growth_mean_use <- round(min(1 + rMAX_species, param$pop_growth_mean), 4)

    # Define theta (shape of DD)
    param$theta <- fixed_theta                 # linear DD
    #param$theta <- theta_spp(rMAX_species)    # non-linear DD, based on theoretical formula

    param$vr_calibrated <- calibrate_params(
      inits = init_calib(s = param$survivals, f = param$fecundities, lam0 = param$pop_growth_mean_use),
      f = param$fecundities, s = param$survivals, lam0 = param$pop_growth_mean_use
    )
    param$s_calibrated <- head(param$vr_calibrated, length(param$survivals))
    param$f_calibrated <- tail(param$vr_calibrated, length(param$fecundities))


    # Make matrix of vital rate values for the report
    if(is.numeric(param$s_calib0) & is.numeric(param$f_calib0)){
      s_use <- param$s_calib0
      f_use <- param$f_calib0
    } else {
      s_use <- input$mat_fill_vr[,1]
      f_use <- input$mat_fill_vr[,2]
    }

    ages <- paste("Age", (1:length(s_use))-1)
    out$vital_rates_mat <-
      matrix(data = round(c(s_use, f_use), 2),
             nrow = length(s_use),
             ncol = 2,
             dimnames = list(ages, c("Survie", "Fécondité"))
      )

  })
  #####

  ############################################################
  ## Convert Fatalities as numbers (not rates)
  ##----------------------------------------------------------
  # Make sure fatalities are expressed as "number" (not rate) for the run_simul function
  se_prod2 <- function(mu1, se1, mu2, se2) sqrt((se1^2 * se2^2) + (se1^2 * mu2^2) + (mu1^2 * se2^2))

  observeEvent({
    input$run
  },{
    if(input$fatalities_unit == "h"){
      pop_size_tot <- sum(pop_vector(pop_size = param$pop_size_mean, pop_size_type = param$pop_size_type, s = param$s_calibrated, f = param$f_calibrated)[-1])
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


  ## Function to translate time units in french
  units_time_french <- function(u){
    if(u == "secs")  u_fr <- "secondes"
    if(u == "mins")  u_fr <- "minutes"
    if(u == "hours") u_fr <- "heures"
    if(u == "days")  u_fr <- "jours"
    if(u == "weeks") u_fr <- "semaines"
    return(u_fr)
  }














  #####
  ##-----------------------------------------------------------------------------------
  ##                                RUN SIMULATIONS
  ##-----------------------------------------------------------------------------------
  # Status File
  status_file <- tempfile()

  get_status <- function(){
    scan(status_file, what = "character",sep="\n")
  }

  set_status <- function(msg){
    write(msg, status_file)
  }

  fire_interrupt <- function(){
    set_status("interrupt")
  }

  fire_ready <- function(){
    set_status("Ready")
  }

  fire_running <- function(perc_complete){
    if(missing(perc_complete))
      msg <- "Running..."
    else
      msg <- paste0("Running... ", perc_complete, "% Complete")
    set_status(msg)
  }

  interrupted <- function(){
    get_status() == "interrupt"
  }

  # Delete file at end of session
  onStop(function(){
    print(status_file)
    if(file.exists(status_file))
      unlink(status_file)
  })

  # Create Status File
  fire_ready()

  nclicks <- reactiveVal(0)
  result_val <- reactiveVal()










  #######################################################################################
  observeEvent({
    input$run
  }, {

    if(ready$fatalities & ready$pop_size & ready$pop_growth & ready$carrying_capacity){

      out$analysis_choice <- input$analysis_choice
      out$species_choice <- input$species_choice
      if(out$analysis_choice != "single_farm") out$show_scen_options <- TRUE

      ## Define parameters
      nsim = param$nsim
      cumulated_impacts = param$cumulated_impacts

      fatalities_mean = param$fatalities_mean_nb
      fatalities_se = param$fatalities_se_nb
      onset_time = param$onset_time

      pop_size_mean = param$pop_size_mean
      pop_size_se = param$pop_size_se
      pop_size_type = param$pop_size_unit

      pop_growth_mean = param$pop_growth_mean_use
      pop_growth_se = param$pop_growth_se

      survivals = param$s_calibrated
      fecundities = param$f_calibrated

      carrying_capacity_mean = param$carrying_capacity_mean
      carrying_capacity_se = param$carrying_capacity_se

      theta = param$theta
      rMAX_species = param$rMAX_species

      model_demo = NULL
      time_horizon = param$time_horizon
      coeff_var_environ = param$coeff_var_environ
      fatal_constant = param$fatal_constant

      # Time
      start_time <- Sys.time()



      ################################
      ##   run_simul starts here    ##
      ################################

      # Create object to store DD parameters
      DD_params <- list()

      # Fill the list of DD parameters
      DD_params$K <- NULL
      DD_params$theta <- theta
      DD_params$rMAX <- rMAX_species

      # Number of years
      nyr <- time_horizon

      # Number of age classes
      nac <- length(survivals)

      # Number of fatalities scenario (+1 because we include a base scenario of NO fatality)
      nsc <- length(fatalities_mean)

      # Initiate Pop Size (output) Array
      out$N <- array(NA, dim = c(nac, nyr, nsc, nsim), dimnames = list(paste0("age", 1:nac),
                                                                   paste0("year", 1:nyr),
                                                                   paste0("sc", (1:nsc)-1)
      ))
      # Object to store values of population growth drawn at each iteration
      lam_it <- rep(NA, nsim)




      ##--------------------------------------------
      # Prepare for Loops                         --
      ##--------------------------------------------
      # Don't do anything if analysis is already being run
      if(nclicks() != 0){
        showNotification("Already running analysis")
        return(NULL)
      }

      # Increment clicks and prevent concurrent analyses
      nclicks(nclicks() + 1)
      result_val("Running...")
      fire_running()

      ##--------------------------------------------
      # Start Loops over simulations              --
      ##--------------------------------------------
      result <- future(seed = NULL, {
        print("Running...")

        ## Loops now ##
        for(sim in 1:nsim){

          # Check for user interrupts
          if(interrupted()){
            print("Stopping...")
            stop("User Interrupt")
          }

          # Notify status file of progress
          fire_running(100*sim/nsim)


          ## PARAMETER UNCERTAINTY : draw values for each input
          # 1. Nomber of fatalities
          M <- NA
          for(j in 1:nsc){
            M[j] <- sample_gamma(1, mu = fatalities_mean[j], sd = fatalities_se[j])
          }

          # 2. Population size : draw and distribute by age class
          N0 <- sample_gamma(1, mu = pop_size_mean, sd =  pop_size_se) %>%
            round %>%
            pop_vector(pop_size_type = pop_size_type, s = survivals, f = fecundities)

          # Draw carrying capacity
          carrying_capacity <- sample_gamma(1, mu = carrying_capacity_mean, sd = carrying_capacity_se) %>%
            round

          # Define K
          K <- sum(pop_vector(pop_size = carrying_capacity, pop_size_type = pop_size_type, s = survivals, f = fecundities))
          if(K < sum(N0)) K <- round(sum(N0)*1.05)
          DD_params$K <- K


          if(pop_growth_se > 0){

            # 3. Population Growth Rate
            lam0 <- sample_gamma(1, mu = pop_growth_mean, sd = pop_growth_se)

            # 4. Calibrate vital rates to match the the desired lambda
            inits <- init_calib(s = survivals, f = fecundities, lam0 = lam0)

            vr <- calibrate_params(inits = inits, f = fecundities, s = survivals, lam0 = lam0)
            s <- head(vr, length(survivals))
            f <- tail(vr, length(fecundities))
            lam_it[sim] <- round(lambda(build_Leslie(s,f)),2)

          }else{

            # No parameter uncertainty on population growth
            s <- survivals
            f <- fecundities
            lam_it[sim] <- round(lambda(build_Leslie(s,f)),2)

          } # End if/else

          model_demo = NULL

          # Choose the model demographique to use (if choice was not forced)
          if(is.null(model_demo)){

            ## Define the complete model by default
            model_demo <- M1_noDD_noDemoStoch # M4_WithDD_WithDemoStoch

            # DECLINING (or stable), but initially LARGE population
            if(lam_it[sim] <= 1 & sum(N0) > 3000) model_demo <- M1_noDD_noDemoStoch

            # DECLINING  (or stable), and initially SMALL population
            if(lam_it[sim] <= 1 & sum(N0) <= 3000) model_demo <- M2_noDD_WithDemoStoch


            # GROWING population...
            if(lam_it[sim] > 1){

              # Calculate rMAX (and theta if needed)
              new_rMAX <- infer_rMAX(K = K, theta = theta,
                                     pop_size_current = sum(N0), pop_growth_current = lam_it[sim],
                                     rMAX_theoretical = Inf)

              if(new_rMAX <= rMAX_species){
                DD_params$rMAX <- new_rMAX
              }else{

                # Infer theta
                DD_params$rMAX <- rMAX_species
                DD_params$theta <- infer_theta(K = K,
                                               pop_size_current = sum(N0), pop_growth_current = lam_it[sim],
                                               rMAX = rMAX_species)

              }

              print(DD_params)


              # ... and initially LARGE population
              if(sum(N0) > 500) model_demo <- M3_WithDD_noDemoStoch


              # ... but initially SMALL population
              if(sum(N0) <= 500) model_demo <- M4_WithDD_WithDemoStoch

            } # if lam > 1


          } # end if "is.null"


          #
          if(cumulated_impacts){
            fun_project <- pop_project_cumulated_impacts
          }else{
            fun_project <- pop_project
            onset_time = NULL
          } # end if

          # Project population trajectory
          out$N[,,,sim] <- fun_project(fatalities = M, onset_time = onset_time, intial_pop_vector = N0,
                                   s = s, f = f, DD_params = DD_params,
                                   model_demo = model_demo, time_horizon = time_horizon,
                                   coeff_var_environ = coeff_var_environ, fatal_constant = fatal_constant)

        } # sim ##-----------------------------------------------------------------------------------------

        # As result
        "C'est fini !"

      }) %...>% result_val()
      ###################################################

      ## Ouput of the run
      #out$N <- list(N = N) # , lambdas = lam_it)
      print(out$N)

      ################################
      ##    run_simul ends here     ##
      ################################

      end_time <- Sys.time()
      duration <- end_time - start_time
      out$N_time <- paste(round(as.numeric(duration), 2), units_time_french(units(duration)))
      print(out$N_time)


      # Catch inturrupt (or any other error) and notify user
      result <- catch(result,
                      function(e){
                        result_val(NULL)
                        print(e$message)
                        showNotification(e$message)
                      })

      # After the promise has been evaluated set nclicks to 0 to allow for anlother Run
      result <- finally(result,
                        function(){
                          fire_ready()
                          nclicks(0)
                        })


      # Return something other than the promise so shiny remains responsive
      NULL

    }else{
      out$N <- NULL
      out$msg <- "error_not_ready"
    }
  }) # Close observEvent
  #####


  ### Buttons : CANCEL and STATUS
  # Register user interrupt
  observeEvent(input$cancel,{
    print("Cancel")
    fire_interrupt()
  })


  # Let user get analysis progress
  observeEvent(input$status,{
    print("Status")
    showNotification(get_status())
  })


  #####
  ##-----------------------------------------------------------------------------------
  ##                                OUTPUTS
  ##-----------------------------------------------------------------------------------

  ### Run message
  output$msg_run <- renderText({
    req(result_val())
  })


  ### Run time
  output$run_time <- renderText({
    req(input$run > 0)
    paste("Temps de calcul (simulations) :", out$N_time)
  })

  ##################################################
  ## Functions for OUTPUT
  ##------------------------------------------------
  ## Function to print individual farm impacts
  print_indiv_impact <- function(){
    req(out$N)
    res <- get_metrics(N = out$N, cumulated_impacts = TRUE)
    n_farm <- (dim(res$indiv_farm$impact)[3]-1)

    fil <- paste0(round(t(quantiles_impact(res$indiv_farm$DR_N, show_quantile = NULL, show_CI = input$show_CI/100)$CI)[-1,]), "%")
    matrix(fil,
           nrow = n_farm,
           dimnames = list(paste("Parc",1:n_farm), c("Impact", "IC (min)", "IC (max)"))
    )
  }

  ## Function to print the global impacts
  print_impact <- function(show_CI){
    req(out$N)
    res <- get_metrics(N = out$N, cumulated_impacts = FALSE)
    n_scen <- (dim(res$scenario$impact)[3]-1)

    RowNam <- NULL
    if(out$analysis_choice == "single_farm") RowNam <- c("Parc 1")
    if(out$analysis_choice == "cumulated") RowNam <- c("Parc 1", paste("... + Parc", (2:n_scen)))
    if(out$analysis_choice == "multi_scenario") RowNam <- paste("Scenario", (1:n_scen))

    fil <- paste0(round(t(quantiles_impact(res$scenario$DR_N, show_quantile = NULL, show_CI = show_CI)$CI)[-1,]), "%")
    matrix(fil,
           nrow = n_scen,
           dimnames = list(RowNam, c("Impact", "IC (min)", "IC (max)"))
    )
  }

  ## Function to make a table of the impacts at given quantile
  table_impact_QT <- function(show_quantile){
    req(out$N)
    res <- get_metrics(N = out$N, cumulated_impacts = FALSE)
    n_scen <- (dim(res$scenario$impact)[3]-1)

    RowNam <- NULL
    if(out$analysis_choice == "single_farm") RowNam <- c("Parc 1")
    if(out$analysis_choice == "cumulated") RowNam <- c("Parc 1", paste("... + Parc", (2:n_scen)))
    if(out$analysis_choice == "multi_scenario") RowNam <- paste("Scenario", (1:n_scen))

    dr_N <- get_metrics(N = out$N, cumulated_impacts = param$cumulated_impacts)$scenario$DR_N
    fil <- paste0(round(
      quantiles_impact(dr_N, show_quantile = 1-(input$risk_A/100), show_CI = NULL, percent = TRUE)$QT[-1]
      , 1), "%")

    matrix(fil,
           nrow = n_scen,
           dimnames = list(RowNam, c("Impact au quantile"))
    )
  }

  ## Function to print the Probability of Extinction
  print_PrExt <- function(){
    req(out$N)
    res <- get_metrics(N = out$N, cumulated_impacts = FALSE)
    n_scen <- dim(res$scenario$impact)[3]

    RowNam <- NULL
    if(out$analysis_choice == "single_farm") RowNam <- c("Sans parc", "Avec parc")
    if(out$analysis_choice == "cumulated") RowNam <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
    if(out$analysis_choice == "multi_scenario") RowNam <- paste("Scenario", (1:n_scen)-1)

    fil <- paste0(round(t(res$scenario$Pext),2)*100, "%")
    matrix(fil,
           nrow = n_scen,
           dimnames = list(RowNam, c("Prob. extinction"))
    )
  }

  ## Function to plot the probability density of the impact
  plot_out_PDF <- function(legend_position, text_size, show_scenario, show_CI){
    if(is.null(out$N)) {} else {

      n_scen <- dim(out$N)[3]
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Parc 1")
      if(out$analysis_choice == "cumulated") Legend <- c("Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", 1:(n_scen-1))

      density_impact(N = out$N, show_CI = show_CI, center = "median",
                     sel_sc = show_scenario, xlims = c(0,100),
                     percent = TRUE, xlab = "\nImpact relatif (%)", ylab = "Densité de probabilité\n",
                     Legend = Legend, legend_position = legend_position, text_size = text_size)
    }
  }


  ## Function to plot the cumulative probability density of the impact
  plot_out_ECDF <- function(legend_position, text_size, show_scenario, show_quantile){
    if(is.null(out$N)) {} else {

      n_scen <- dim(out$N)[3]
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Parc 1")
      if(out$analysis_choice == "cumulated") Legend <- c("Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", 1:(n_scen-1))

      ECDF_impact(N = out$N, show_quantile = show_quantile, sel_sc = show_scenario,
                  xlims = c(0,100),
                  percent = TRUE, xlab = "\nImpact relatif (%)", ylab = "Densité de probabilité cumulée\n",
                  Legend = Legend, legend_position = legend_position, text_size = text_size)
    }
  }


  ## Function to plot the relative impact over time
  plot_out_impact <- function(legend_position, text_size, show_scenario, show_CI){
    if(is.null(out$N)) {} else {

      n_scen <- dim(out$N)[3]
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Sans parc", "Avec parc")
      if(out$analysis_choice == "cumulated") Legend <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", (1:n_scen)-1)

      plot_impact(N = out$N, onset_year = param$onset_year, sel_sc = show_scenario,
                  percent = TRUE, show_CI = show_CI,
                  xlab = "\nAnnée", ylab = "Impact relatif (%)\n", Legend = Legend,
                  legend_position = legend_position, text_size = text_size)
    }
  }


  # Function to plot trajectories
  plot_out_traj <- function(show_scenario){
    if(is.null(out$N)) {
    } else {

      n_scen <- dim(out$N)[3]

      # Define Legend
      Legend <- NULL
      if(out$analysis_choice == "single_farm") Legend <- c("Sans parc", "Avec parc")
      if(out$analysis_choice == "cumulated") Legend <- c("Sans parc", "+ Parc 1", paste("... + Parc", (3:n_scen)-1))
      if(out$analysis_choice == "multi_scenario") Legend <- paste("Scenario", (1:n_scen)-1)

      # Plot population trajectories
      plot_traj(N = out$N, age_class_use = input$age_class_show, fecundities = param$f_calibrated,
                onset_year = param$onset_year, sel_sc = show_scenario,
                xlab = "\nAnnée", ylab = "Taille de population\n", Legend = Legend, ylim = c(0, NA))}
  }


  ##################################################
  ## Impact (text) : GLOBAL (for all types of analysis)
  ##------------------------------------------------
  # Display title
  output$title_impact_result <- renderText({
    req(input$run)
    paste("Impact global estimé au bout de" , param$time_horizon, "ans")
  })

  # Display impact result (table)
  output$impact_table <- renderTable({
    req(input$run)
    out$impact_table <- print_impact(show_CI = input$show_CI/100)
    out$impact_table
  }, rownames = TRUE, align = "lccc", width = "auto")


  #############################################
  ## Text : Probability of extinction
  ##-------------------------------------------
  # Display title
  output$title_PrExt_result <- renderText({
    req(input$run)
    paste("Probabilité d'extinction à", param$time_horizon, "ans")
  })

  # Display impact result (table)
  output$PrExt_table <- renderTable({
    req(input$run)
    out$PrExt_table <- print_PrExt()
    out$PrExt_table
  }, rownames = TRUE, align = "c", width = "auto")




  #####

  #############################################
  ## Graphs (PDF & ECDF) : choose scenario
  ##-------------------------------------------
  # Choose which scenario(s) to show
  observe({
    if(!is.null(out$N)){
      n_scen <- dim(out$N)[3] - 1

      choices <- c("all", paste(1:n_scen))
      names(choices) <- c("Tous", paste("Scenario", 1:n_scen))

      updateRadioButtons(session, "show_scenario",
                         label = "Choix du scénario",
                         choices = choices, # c("all", paste("scénario", 1:n_scen)),
                         selected = "all"
      )
    }
  })

  #############################################
  ## Plot : Probability Density of Impact
  ##-------------------------------------------
  output$title_PDF_plot <- renderText({
    if(input$run > 0){
      paste("Résultat : Densité de probabilité de l'impact relatif à", param$time_horizon, "ans")
    }
  })

  output$PDF_plot <- renderPlot({
    plot_out_PDF(legend_position = "right", text_size = "large",
                 show_scenario = input$show_scenario, show_CI = input$show_CI/100)
  })

  #############################################
  ## Plot : Cumulative distribution of Impact
  ##-------------------------------------------
  output$title_ECDF_plot <- renderText({
    if(input$run > 0){
      paste("Résultat : Distribution cumulative de l'impact relatif à", param$time_horizon, "ans")
    }
  })

  output$ECDF_plot <- renderPlot({
    plot_out_ECDF(legend_position = "right", text_size = "large",
                  show_scenario = input$show_scenario, show_quantile = 1-(input$risk_A/100))
  })


  output$quantile_impact_result <- renderText({
    dr_N <- get_metrics(N = out$N, cumulated_impacts = param$cumulated_impacts)$scenario$DR_N
    impact_QT <- quantiles_impact(dr_N, show_quantile = 1-(input$risk_A/100), show_CI = NULL, percent = TRUE)$QT[-1]
    paste0("Scénario ", 1:length(impact_QT), " : ", round(impact_QT,1), "%", collapse = "\n")
  })


  #############################################
  ## Plot Impacts over time
  ##-------------------------------------------
  output$title_impact_plot <- renderText({
    if(input$run > 0){
      "Résultat : Impact relatif au cours du temps"
    }
  })

  output$impact_plot <- renderPlot({
    plot_out_impact(legend_position = "right", text_size = "large",
                    show_scenario = input$show_scenario, show_CI = input$show_CI/100)
  })


  #############################################
  ## Plot Demographic Trajectories
  ##-------------------------------------------
  output$title_traj_plot <- renderText({
    if(input$run > 0){
      "Graphique : Trajectoires démographiques"
    }
  })

  output$warning_traj_plot <- renderText({
    if(input$run > 0){
      "Attention : Il s'agit de prédictions en l'état actuel des connaissances.
      Personne ne peut prédire comment les facteurs d'influence démographique (environnement, etc.)
      vont évoluer dans le futur. Donc personne ne peut prédire de façon exacte ce que sera la taille
      de population dans plusieurs années.\n
      Ce graphe est simplementfourni à titre informatif. Attention à ne pas le sur-interpréter.\n
      L'impact des collisions doit être évalué à partir des valeurs et du graphe ci-dessus, qui fournissent
      une estimation plus robuste (c-à-d. moins sensibles aux postulats et incertitudes) de cet impact."
    }
  })


  output$traj_plot <- renderPlot({
    plot_out_traj(show_scenario = input$show_scenario)
  })
  #####

  ###################################################################################

  #############################################
  ## Save outputs for report
  ##-------------------------------------------
  # Type of analysis & time horizon ####
  observeEvent({
    input$run
  }, {
    out$time_horizon <- param$time_horizon
    if(out$analysis_choice == "single_farm") out$analysis_choice_report <- "Impacts non cumulés"
    if(out$analysis_choice == "cumulated") out$analysis_choice_report <- "Impacts cumulés"
    if(out$analysis_choice == "multi_scenario") out$analysis_choice_report <- "Scénarios hypothétiques"
  })

  # Fatalities ####
  observeEvent({
    input$run
  }, {

    ## Unit
    if(input$fatalities_unit == "M"){
      out$fatalities_unit <- paste0("Unité : nombre de mortalités annuelles")
      unit <- " mortalité(s)"
    }
    if(input$fatalities_unit == "h"){
      out$fatalities_unit <- paste0("Unité : taux de mortalité (%) annuel")
      unit <- " %"
    }

    ## Analysis : hypothetical scenarios
    if(input$analysis_choice == "multi_scenario"){
      req(input$fatalities_vec_scenario)
      out$fatalities_input_type <- "Saisie : Scénarios hypothétiques"
      vec01 <- as.numeric(unlist(strsplit(input$fatalities_vec_scenario, " ")))
      out$fatalities_val1 <- paste0("Scénario(s) de mortalité: ",
                                    vec01, unit, ".")
      out$fatalities_val2 <- NULL
    }else{

      ## Analysis : cumulated impacts
      if(input$analysis_choice == "cumulated"){
        out$fatalities_input_type <- "Saisie : impacts cumulés"
        out$fatalities_val1 <- paste0("Valeur estimée : ", input$fatalities_mat_cumulated[,1], unit, " ; ")
        out$fatalities_val2 <- paste0("Erreur-type : ", input$fatalities_mat_cumulated[,2], unit)
      }else{
        ## Analysis : single farm
        if(input$fatalities_input_type == "itvl"){
          out$fatalities_input_type <- "Saisie : intervalle"
          out$fatalities_val1 <- paste0("Min : ", input$fatalities_lower, unit, " ; ")
          out$fatalities_val2 <- paste0("Max : ", input$fatalities_upper, unit)
        }
        if(input$fatalities_input_type == "val"){
          out$fatalities_input_type <- "Saisie : estimation et erreur-type"
          out$fatalities_val1 <- paste0("Valeur estimée : ", input$fatalities_mean, unit, " ; ")
          out$fatalities_val2 <- paste0("Erreur-type : ", input$fatalities_se, unit)
        }
        if(input$fatalities_input_type == "eli_exp"){
          out$fatalities_input_type <- "Saisie : élicitation d'experts"
          out$fatalities_val1 <- paste0("Moyenne estimée : ", round(param$fatalities_eli_result$mean, 2), unit, " ; ")
          out$fatalities_val2 <- paste0("Erreur_type : ", round(param$fatalities_eli_result$SE, 2), unit)
        }
        ##### END single fram ###
      } # if 2: "cumulated"
    } # if 1 : multi_scenario

  }) # observeEvent


  # Population Size ####
  observeEvent({
    input$run
  }, {
    if(input$pop_size_unit == "Npair"){
      out$pop_size_unit <- paste0("Unité : nombre de couples")
      unit <- " couples"
    }
    if(input$pop_size_unit == "Ntotal"){
      out$pop_size_unit <- paste0("Unité : effectif total")
      unit <- " individus"
    }

    if(input$pop_size_input_type == "itvl"){
      out$pop_size_input_type <- "Saisie : intervalle"
      out$pop_size_val1 <- paste0("Min : ", input$pop_size_lower, unit, " ; ")
      out$pop_size_val2 <- paste0("Max : ", input$pop_size_upper, unit)
    }
    if(input$pop_size_input_type == "val"){
      out$pop_size_input_type <- "Saisie : estimation et erreur-type"
      out$pop_size_val1 <- paste0("Valeur estimée : ", input$pop_size_mean, unit, " ; ")
      out$pop_size_val2 <- paste0("Erreur-type : ", input$pop_size_se, unit)
    }
    if(input$pop_size_input_type == "eli_exp"){
      out$pop_size_input_type <- "Saisie : élicitation d'experts"
      out$pop_size_val1 <- paste0("Moyenne estimée : ", round(param$pop_size_eli_result$mean, 2), unit, " ; ")
      out$pop_size_val2 <- paste0("Erreur_type : ", round(param$pop_size_eli_result$SE, 2), unit)
    }
  })


  # Population Growth rate ####
  observeEvent({
    input$run
  }, {
    unit <- "%"

    if(input$pop_growth_input_type == "itvl"){
      out$pop_growth_input_type <- "Saisie : intervalle"
      out$pop_growth_val1 <- paste0("Min : ", input$pop_growth_lower, unit, " ; ")
      out$pop_growth_val2 <- paste0("Max : ", input$pop_growth_upper, unit)
    }
    if(input$pop_growth_input_type == "val"){
      out$pop_growth_input_type <- "Saisie : estimation et erreur-type"
      out$pop_growth_val1 <- paste0("Valeur estimée : ", input$pop_growth_mean, unit, " ; ")
      out$pop_growth_val2 <- paste0("Erreur-type : ", input$pop_growth_se, unit)
    }
    if(input$pop_growth_input_type == "eli_exp"){
      out$pop_growth_input_type <- "Saisie : élicitation d'experts"
      out$pop_growth_val1 <- paste0("Moyenne estimée : ", round(param$pop_growth_eli_result$mean, 2), unit, " ; ")
      out$pop_growth_val2 <- paste0("Erreur_type : ", round(param$pop_growth_eli_result$SE, 2), unit)
    }

    ## TREND ####
    if(input$pop_growth_input_type == "trend"){
      out$pop_growth_input_type <- "Saisie : tendance"

      if(input$pop_trend == "stable"){
        V1 <- "Stable"
        V2 <- NULL
      }

      if(input$pop_trend == "growth"){
        V1 <- "En croissance"
        if(input$pop_trend_strength == "weak") V2 <- "faible"
        if(input$pop_trend_strength == "average") V2 <- "modérée"
        if(input$pop_trend_strength == "strong") V2 <- "forte"
      }

      if(input$pop_trend == "decline"){
        V1 <- "En déclin"
        if(input$pop_trend_strength == "weak") V2 <- "faible"
        if(input$pop_trend_strength == "average") V2 <- "modéré"
        if(input$pop_trend_strength == "strong") V2 <- "fort"
      }
      out$pop_growth_val1 <- V1
      out$pop_growth_val2 <- V2
    }
  })

  # Carrying capacity ####
  observeEvent({
    input$run
  }, {
    if(input$pop_size_unit == "Npair"){
      out$carrying_cap_unit <- paste0("Unité : nombre de couples")
      unit <- " couples"
    }
    if(input$pop_size_unit == "Ntotal"){
      out$carrying_cap_unit <- paste0("Unité : effectif total")
      unit <- " individus"
    }

    if(input$carrying_cap_input_type == "itvl"){
      out$carrying_cap_input_type <- "Saisie : intervalle"
      out$carrying_cap_val1 <- paste0("Min : ", input$carrying_capacity_lower, unit, " ; ")
      out$carrying_cap_val2 <- paste0("Max : ", input$carrying_capacity_upper, unit)
    }
    if(input$carrying_cap_input_type == "val"){
      out$carrying_cap_input_type <- "Saisie : estimation et erreur-type"
      out$carrying_cap_val1 <- paste0("Valeur estimée : ", input$carrying_capacity_mean, unit, " ; ")
      out$carrying_cap_val2 <- paste0("Erreur-type : ", input$carrying_capacity_se, unit)
    }
    if(input$carrying_cap_input_type == "eli_exp"){
      out$carrying_cap_input_type <- "Saisie : élicitation d'experts"
      out$carrying_cap_val1 <- paste0("Moyenne estimée : ", round(param$carrying_cap_eli_result$mean, 2), unit, " ; ")
      out$carrying_cap_val2 <- paste0("Erreur_type : ", round(param$carrying_cap_eli_result$SE, 2), unit)
    }

    if(input$carrying_cap_input_type == "no_K"){
      out$carrying_cap_input_type <- NULL
      out$carrying_cap_val1 <- paste0("Absence de capacité de charge")
      out$carrying_cap_val2 <- NULL
    }

  })



  #####

  ## Results #####
  # Graphs ####
  observeEvent({
    input$run
    input$show_CI
    input$risk_A
  }, {
    req(input$run > 0)
    out$PDF_plot <- plot_out_PDF(legend_position = "bottom", text_size = "small",
                                 show_scenario = "all", show_CI = input$show_CI/100)
    out$ECDF_plot <- plot_out_ECDF(legend_position = "bottom", text_size = "small",
                                   show_scenario = "all", show_quantile = 1-(input$risk_A/100))
    out$impact_plot <- plot_out_impact(legend_position = "bottom", text_size = "small",
                                       show_scenario = "all", show_CI = input$show_CI/100)

    out$CI <- input$show_CI
    out$QT <- 100-(input$risk_A)
    out$risk_A <- input$risk_A

    out$impact_QT_table <- table_impact_QT(show_quantile = 1-(input$risk_A/100))

    print(out$impact_QT_table)
  })



  #####

  ###################################################################################

  ##-----------------------------------------------------------------------------------
  ##                                REPORT
  ##-----------------------------------------------------------------------------------
  output$report <- downloadHandler(

    filename = "Rapport_Eolpop.pdf",

    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")

      file.copy("report.Rmd", tempdir(), overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      paramsRMD <- list(
        intro = input$intro_report,
        analysis = out$analysis_choice_report,
        species = out$species_choice,
        def_pop_text = input$def_pop_text,
        vital_rates_mat = out$vital_rates_mat,
        #vital_rates = out$vital_rates,

        fatalities_unit = out$fatalities_unit,
        fatalities_input_type = out$fatalities_input_type,
        fatalities_val1 = out$fatalities_val1,
        fatalities_val2 = out$fatalities_val2,

        pop_size_unit = out$pop_size_unit,
        pop_size_input_type = out$pop_size_input_type,
        pop_size_val1 = out$pop_size_val1,
        pop_size_val2 = out$pop_size_val2,

        pop_growth_input_type = out$pop_growth_input_type,
        pop_growth_val1 = out$pop_growth_val1,
        pop_growth_val2 = out$pop_growth_val2,

        carrying_cap_unit = out$carrying_cap_unit,
        carrying_cap_input_type = out$carrying_cap_input_type,
        carrying_cap_val1 = out$carrying_cap_val1,
        carrying_cap_val2 = out$carrying_cap_val2,

        PDF_plot = out$PDF_plot,
        ECDF_plot = out$ECDF_plot,
        impact_plot = out$impact_plot,
        #trajectory_plot = out$trajectory_plot

        time_horizon = out$time_horizon,
        impact_table = out$impact_table,
        PrExt_table = out$PrExt_table,

        CI = out$CI,
        QT = out$QT,
        risk_A = out$risk_A,
        impact_QT_table = out$impact_QT_table
      )


      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = paramsRMD,
                        envir = new.env(parent = globalenv())
      )
    }
  ) # close downloadHandler




  ###################################################################################
} # End server

