init_fit <- function(x,spec_rm = TRUE) {
  if (!require(rugarch)) {
    stop("You need to install the rugarch package to use this function")

  }
  if (!require(stringr)) {
    stop("You need to install the stringr package to use this function")

  }
  if (!require(dplyr)) {
    stop("You need to install the dplyr package to use this function")
  }
  if (class(spec_rm)!= "logical") {
    stop("Argument spec_rm should have a logical value")
  }
  lst <- ls(pattern = ".spec",envir = .GlobalEnv)
  variables <- lst[sapply(lst,function(var) any(class(get(var)) == "uGARCHspec"))]

  num_models <- length(variables)

  counter <- 0

  if (spec_rm == TRUE) {

    for (var in variables) {

      nam <- paste(str_remove(var,pattern = ".spec"),"fit",sep = ".")

      print(noquote(paste0("Estimation of ", nam, " initiated")))

      cat("\n")

      assign(nam,ugarchfit(spec = get(var),data = x),envir = .GlobalEnv)

      counter <- counter + 1

      print(noquote(paste("Estimation complete", as.character(counter), "out of", as.character(num_models))))

      cat("\n")

    }

    rm(list = variables,envir = .GlobalEnv)
  }
  if (spec_rm == FALSE) {
    for (var in variables) {
      nam <- paste(str_remove(var,pattern = ".spec"),"fit",sep = ".")

      print(noquote(paste0("Estimation of ", nam, " initiated")))

      cat("\n")

      assign(nam,ugarchfit(spec = get(var),data = x),envir = .GlobalEnv)

      counter <- counter + 1

      print(noquote(paste("Estimation complete", as.character(counter), "out of", as.character(num_models))))

      cat("\n")

    }
  }
}
