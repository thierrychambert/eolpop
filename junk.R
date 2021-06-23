source("C:/rdev/eolpop/inst/ShinyApp/ui.R")

input <- list(M1 = 5, M1_se = 0, N00_mu = 200, N_type = "Npair", lam0 = 0.95, mort_cons = "h")

run0 <- run_simul(nsim = nsim,
                  fatalities_mean = c(M0, input$M1),
                  fatalities_se = c(M0_se, input$M1_se),
                  pop_size_mean = input$N00_mu,
                  pop_size_se = N00_se,
                  N_type = input$N_type,
                  pop_growth_mean = input$lam0,
                  pop_growth_se = lam0_se,
                  survivals_mean = s_input,
                  fecundities_mean = f_input,
                  model_demo = model_demo,
                  time_horzion = TH,
                  coeff_var_environ = cv_env,
                  fatal_constant = input$mort_cons)

out <- list(N = NULL)
out$N <- run0$N

N <- out$N

names(out)
dim(out$N)

get_metrics(out$N)

get_metrics(N = out$N)[dim(out$N)[2],"avg","sc1"]


# Impact
print_it <- function(impact, lci, uci){
  paste0("Impact sur la taille de population : ", round(impact, 2)*100, "%",
         " [", round(lci, 2)*100, "% ; ", round(uci, 2)*100, "%]")
}

print_out <- function() if(is.null(out$N)) {} else {
  print_it(impact = get_metrics(N = out$N)[30,"avg","sc1"],
           lci = get_metrics(N = out$N)[30,"lci","sc1"],
           uci = get_metrics(N = out$N)[30,"uci","sc1"])
}

print_out()












ui <- fluidPage(
  checkboxInput("error", "error?"),
  textOutput("result")
)
server <- function(input, output, session) {
  a <- reactive({
    if (input$error) {
      stop("Error!")
    } else {
      1
    }
  })
  b <- reactive(a() + 1)
  c <- reactive(b() + 1)
  output$result <- renderText(c())
}

shinyApp(ui = ui, server = server)









rm(list = ls(all.names = TRUE))
graphics.off()

library(eolpop)

#load(file = "./data/run0.rda")
names(run0)

out <- run0
dim(out$N)
get_metrics(out$N)


get_metrics(N = out$N)[dim(out$N)[2],"avg","sc1"]

get_metrics(N = out$N)[dim(out$N)[2],"lci","sc1"]

get_metrics(N = out$N)[dim(out$N)[2],"uci","sc1"]



