rank_models <- function(info_criteria_data) {

  model_name <- row.names(which(info_criteria_data == min(info_criteria_data, na.rm = TRUE), arr.ind = TRUE))

  positions <- as.data.frame(which(info_criteria_data == min(info_criteria_data, na.rm = TRUE), arr.ind = TRUE))

  info_name <- names(info_criteria_data)[positions$col]

  info_value <- info_criteria_data[positions$row, positions$col]

  model <- strsplit(model_name,"[.]")[[1]][1]

  distribution <- strsplit(model_name,"[.]")[[1]][2]

  print(noquote("Autogarch Process Complete!"))

  cat("\n")

  print(noquote(paste("Information Criteria minimized: ", as.character(info_name))))

  print(noquote(paste("Value: ", as.character(info_value))))

  cat("\n")

  print(noquote(paste("Model: ", model)))

  print(noquote(paste("Distribution: ", distribution)))

}
