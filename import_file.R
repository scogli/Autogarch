# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:

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


