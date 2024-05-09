library(data.table)

create_tree <- function(data) {
  # Change modelsele(data), split(data), value_selection(data) into our own functions which returns model name, column name, split value selection
  current_model <- modelsele(data)
  split_variable <- split(data)
  selection <- value_selection(data)
  
  # Stop growing condition
  if (nrow(data) < 0.05 * nrow(fread("C:/Users/10068/Desktop/Poison/data_clean.csv")) ||
      length(unique(data[[current_model]])) == 1 ||
      all(sapply(data[, !names(data) %in% current_model, with = FALSE], function(x) length(unique(x)) <= 1))) {
    return(list("Terminal Node", model = current_model, data = data))
  }
  
  # Based on it's continuous or categorical, build leaves
  if (is.numeric(data[[split_variable]])) {
    left_data <- data[data[[split_variable]] < selection, , drop = FALSE]
    right_data <- data[data[[split_variable]] >= selection, , drop = FALSE]
  } else {
    left_data <- data[data[[split_variable]] %in% selection, , drop = FALSE]
    right_data <- data[!data[[split_variable]] %in% selection, , drop = FALSE]
  }
  
  # Recursively  call the function
  left_tree <- create_tree(left_data)
  right_tree <- create_tree(right_data)
  
  
  return(list("Node", split_variable = split_variable, selection = selection,
              left = left_tree, right = right_tree, model = current_model))
}

# how to use
data <- fread("C:/Users/10068/Desktop/Poison/data_clean.csv")
tree <- create_tree(data)
