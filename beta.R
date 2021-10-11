beta <- function(x, mkt_ticker,start_date,end_date) {
  if (missing(x)) {
    stop("Argument x is missing")

  }
  if (missing(mkt_ticker)) {
    stop("Argument mkt_ticker is missing")

  }
  if (missing(start_date)) {
    stop("Argument start_date is missing")

  }
  if (missing(end_date)) {
    stop("Argument end_date is missing")

  }
  if (class(mkt_ticker) != "character") {
    mkt_ticker <- as.character(mkt_ticker)
  }
  if (class(start_date) != "character") {
    start_date <- as.character(start_date)
  }
  if (class(end_date) != "character") {
    end_date <- as.character(end_date)
  }
  if (!require(dplyr)) {
    stop("You need to install the dplyr package to use this function")}
  else{
    bin <- list()
    df <- as.data.frame(x %>% filter(Date >= start_date & Date <= end_date))
    for (ticker in colnames(x)[2:length(colnames(x))] ) {
      if (mkt_ticker == ticker) {
        next
      }
      bin[[ticker]] <- cov(df[[ticker]],df[[mkt_ticker]])/var(df[[mkt_ticker]])
    }

    betas <<- as.data.frame(bin)

    View(betas)
  }

}
