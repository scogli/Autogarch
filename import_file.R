import_file <- function(variable_name, sheet_name = NULL) {

  filePth <- choose.files(caption = "Select a Financial Time Series")

  if (length(filePth) == 0) {

    stop("Please choose a valid path")

    break

  }

  if (!require(readxl)) {

    stop("You need to install the readxl package to use this function")

  }


  assign(variable_name,read_excel(filePth,sheet = sheet_name),envir = .GlobalEnv)

}


