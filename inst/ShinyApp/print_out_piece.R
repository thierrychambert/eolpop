print_out <- reactive({
  if(is.null(out$N)){
    "Waiting"
  } else {
    print_it(impact = get_metrics(N = out$N)[30,"avg","sc1"],
             lci = get_metrics(N = out$N)[30,"lci","sc1"],
             uci = get_metrics(N = out$N)[30,"uci","sc1"])
  }
}) # end reactive
