

# Expert stuff

values <- reactiveValues(mat = NULL, t_mat = NULL, vals = NULL, Cp = NULL, weights = NULL, out = NULL)
observe({
  values$t_mat <- t(input$expert)
})

observe({
  values$vals = values$t_mat[3:5,]
  values$Cp = values$t_mat[6,]
  values$weights = values$t_mat[2,]
})

func_out <- eventReactive({input$run_expert}, {if(all(is.na(values$t_mat))){print(values$vals)}
  else {values$mat <- exp_estim_gamma(values$vals, values$Cp, values$weights)
  SE <- sqrt(values$mat$var_smooth)
  return(list(values$mat$mean_smooth, SE))}})

output$Mean <- renderPrint({
  func_out()
})

func_plot <- function(){if(is.null(values$mat)) {} else {plot_exp_estim(values$mat)}}
output$plot <- renderPlot({
  func_plot()
})


