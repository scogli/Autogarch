agg_models <- function(models,all,view = TRUE) {
  if (!require(dplyr)) {
    stop("You need to install the dplyr package to use this function")

  }
  if (view == TRUE) {
    if (all == TRUE) {
      lst <- ls(pattern = ".fit",envir = .GlobalEnv)
      variables <- lst[sapply(lst, function(var) any(class(get(var)) == "uGARCHfit"))]
      bin <- list()
      for (var in variables) {
        bin[[var]] <- c(infocriteria(get(var))[1:4],likelihood(get(var))[1])
      }

      info_criteria <<- t(as.data.frame(bin,row.names = c(row.names(infocriteria(get(variables[1]))),"LogLik")))
      View(info_criteria)

    }
    if (all == FALSE) {
      variables <- models
      bin <- list()
      for (var in variables) {
        bin[[var]] <- c(infocriteria(get(var))[1:4],likelihood(get(var))[1])
      }

      info_criteria <<- t(as.data.frame(bin,row.names = c(row.names(infocriteria(get(variables[1]))),"LogLik")))
      View(info_criteria)

    }


  }
  if (view == FALSE) {
    if (all == TRUE) {
      lst <- ls(pattern = ".fit",envir = .GlobalEnv)
      variables <- lst[sapply(lst, function(var) any(class(get(var)) == "uGARCHfit"))]
      bin <- list()
      for (var in variables) {
        bin[[var]] <- c(infocriteria(get(var))[1:4],likelihood(get(var))[1])
      }

      info_criteria <<- t(as.data.frame(bin,row.names = c(row.names(infocriteria(get(variables[1]))),"LogLik")))


    }
    if (all == FALSE) {
      variables <- models
      bin <- list()
      for (var in variables) {
        bin[[var]] <- c(infocriteria(get(var))[1:4],likelihood(get(var))[1])
      }

      info_criteria <<- t(as.data.frame(bin,row.names = c(row.names(infocriteria(get(variables[1]))),"LogLik")))


    }


  }


}
