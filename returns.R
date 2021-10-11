returns <- function(x, simple = FALSE,view = TRUE){

  if (missing(x)) {
    stop("Argument x is missing")

  }
  #Need to insert argument checking for the class of argument x
  if (class(simple) != "logical") {
    stop("Argument simple should contain a logical value")
  }
  bin <- list()
  x <- as.data.frame(x)
  if (view == TRUE) {
    if (simple == FALSE) {
      for (ticker in colnames(x)) {
        if (ticker == "Date") {
          bin[[ticker]] <- x[[ticker]][2:length(x[[ticker]])]

        }
        else{
          bin[[ticker]] <- diff(log(x[[ticker]]))
        }
      }

      log_returns <<- as.data.frame(bin)


      View(log_returns)


    }
    if (simple == TRUE) {
      for (ticker in colnames(x)) {
        if (ticker == "Date") {
          bin[[ticker]] <- x[[ticker]][2:length(x[[ticker]])]
        }
        else{
          bin[[ticker]] <- ((x[[ticker]][2:length(x[[ticker]])] - x[[ticker]][1:length(x[[ticker]])-1])/x[[ticker]][1:length(x[[ticker]])-1])
        }
      }

      smpl_returns <<- as.data.frame(bin)

      View(smpl_returns)

    }

  }
  if (view == FALSE) {
    if (simple == FALSE) {
      for (ticker in colnames(x)) {
        if (ticker == "Date") {
          bin[[ticker]] <- x[[ticker]][2:length(x[[ticker]])]

        }
        else{
          bin[[ticker]] <- diff(log(x[[ticker]]))
        }
      }

      log_returns <<- as.data.frame(bin)





    }
    if (simple == TRUE) {
      for (ticker in colnames(x)) {
        if (ticker == "Date") {
          bin[[ticker]] <- x[[ticker]][2:length(x[[ticker]])]
        }
        else{
          bin[[ticker]] <- ((x[[ticker]][2:length(x[[ticker]])] - x[[ticker]][1:length(x[[ticker]])-1])/x[[ticker]][1:length(x[[ticker]])-1])
        }
      }

      smpl_returns <<- as.data.frame(bin)



    }

  }}
