server <- function(output, input){
  observe({
    shinyjs::hide("morta_type")
    shinyjs::hide("nber_park")
    shinyjs::hide("nber_wind_turbine")
    shinyjs::hide("temporality")
    shinyjs::hide("data")
    shinyjs::hide("expert")
    shinyjs::hide("run_expert")
    shinyjs::hide("M1")
    shinyjs::hide("M1_se")
    shinyjs::hide("M1_ic")
    shinyjs::hide("mort_cons")
    shinyjs::hide("N_type")
    shinyjs::hide("expert_2")
    shinyjs::hide("N00_mu")
    shinyjs::hide("N00_se")
    shinyjs::hide("IC_2")
    shinyjs::hide("lambda_type")
    shinyjs::hide("lam0_mu")
    shinyjs::hide("lam0_se")
    shinyjs::hide("IC_3")
    shinyjs::hide("trend")
    shinyjs::hide("trend_2")
    shinyjs::hide("auto")
    shinyjs::hide("mat_params_demog")

    if(input$Mortality%%2 == 1){
      shinyjs::show("morta_type")
      shinyjs::show("nber_wind_turbine")
      shinyjs::show("temporality")
      shinyjs::show("data")
      shinyjs::show("mort_cons")
      if(input$morta_type == "cumulees"){
        shinyjs::show("nber_park")
        shinyjs::show("nber_wind_turbine")}
      if(input$data == "Suivi (observations terrains + EolApp)" | input$data == "Modele predictif (type Band)"){
        shinyjs::show("M1")
        shinyjs::show("M1_se")
        shinyjs::show("M1_ic")
      }
      if(input$data == "Dire d'expert"){
        shinyjs::show("expert")
        shinyjs::show("run_expert")
      }}

    if(input$pop_size%%2 == 1){
      shinyjs::show("N_type")
      #if(input$N_type == "Npair"){
       shinyjs::show("N00_mu")
       shinyjs::show("N00_se")
       shinyjs::show("IC_2")
      #}
      #if(input$N_type == "Effectif total"){
        #shinyjs::show("expert_2")
      #}
    }

    if(input$pop_trend%%2 == 1){
      shinyjs::show("lambda_type")
      shinyjs::show("lam0_mu")
      shinyjs::show("lam0_se")
      shinyjs::show("IC_3")
      shinyjs::show("trend")
      shinyjs::show("trend_2")
    }

    if(input$params_demog%%2 == 1){
      shinyjs::show("auto")
      shinyjs::show("mat_params_demog")
    }

  })

  out <- reactiveValues(N = NULL)


  observeEvent({
    input$run
  }, {

    run0 <- run_simul(nsim = nsim,
                     fatalities_mean = c(M0, input$M1),
                     fatalities_se = c(M0_se, input$M1_se),
                     pop_size_mean = input$N00_mu,
                     pop_size_se = input$N00_se,
                     pop_size_type = input$N_type,
                     pop_growth_mean = input$lam0_mu,
                     pop_growth_se = input$lam0_se,
                     survivals_mean = s_input,
                     fecundities_mean = f_input,
                     model_demo = model_demo,
                     time_horzion = TH,
                     coeff_var_environ = cv_env,
                     fatal_constant = input$mort_cons)

    out$N <- run0$N

    print(input$M1)
    print(dim(out$N))
    print(get_metrics(N = out$N)[30,"avg","sc1"])
    print(get_metrics(N = out$N)[30,"lci","sc1"])
    print(get_metrics(N = out$N)[30,"uci","sc1"])
    print(is.null(out$N))

    print(
      print_it(impact = get_metrics(N = out$N)[30,"avg","sc1"],
               lci = get_metrics(N = out$N)[30,"lci","sc1"],
               uci = get_metrics(N = out$N)[30,"uci","sc1"])
    )

      })

  ## Output
  print_it <- function(impact, lci, uci){
    paste0("Impact sur la taille de population : ", round(impact, 2)*100, "%",
           " [", round(lci, 2)*100, "% ; ", round(uci, 2)*100, "%]")
  } # End function

  print_out <- reactive({
    if(is.null(out$N)){
      "Pas encore de resultat"
    } else {
      print_it(impact = get_metrics(N = out$N)[30,"avg","sc1"],
               lci = get_metrics(N = out$N)[30,"lci","sc1"],
               uci = get_metrics(N = out$N)[30,"uci","sc1"])
    }
  }) # end reactive

  ## Text : impact
    output$message <- renderText({ print_out() })
  #output$message <- renderText({ "Test" })



  # Plot trajectories
  plot_out <- function() if(is.null(out$N)) {} else {plot_impact(N = out$N)}
  output$graph <- renderPlot({
  plot_out()
  })

} # end server



