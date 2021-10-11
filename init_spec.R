init_spec <- function(model,all = FALSE) {
  if (!require(rugarch)) {
    stop("You need to install the dplyr package to use this function")

  }
  if (class(all) != "logical") {
    stop("Argument all should contain a logical value")

  }
  distributions <- c("norm","snorm","std","sstd","ged","sged","nig","ghyp","jsu")
  if (all == FALSE) {
    if (class(model) != "character") {
      model <- as.character(model)

    }
    if (length(model) > 1) {
      options(warn = -1)
      concat <- as.character(outer(model,distributions,paste,sep = "."))
      for (i in concat) {
        nam <- paste(i,"spec",sep = ".")
        assign(nam,ugarchspec(variance.model = list(model = strsplit(nam,split = ".",fixed = TRUE)[[1]][1]),distribution.model = strsplit(nam,split = ".",fixed = TRUE)[[1]][2]),envir = .GlobalEnv)
      }

    }
    for (dist in distributions) {
      nam <- paste(model,dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list( model = model),distribution.model = dist),envir = .GlobalEnv)

    }
  }
  if(all == TRUE){
    for (dist in distributions) {
      nam <- paste("sGARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "sGARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
    for (dist in distributions) {
      nam <- paste("eGARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "eGARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
    for (dist in distributions) {
      nam <- paste("gjrGARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "gjrGARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
    for (dist in distributions) {
      nam <- paste("apARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "apARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
    for (dist in distributions) {
      nam <- paste("iGARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "iGARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
    for (dist in distributions) {
      nam <- paste("csGARCH",dist,"spec",sep = ".")
      assign(nam,ugarchspec(variance.model = list(model = "csGARCH"),distribution.model = dist),envir = .GlobalEnv)
    }
  }}
