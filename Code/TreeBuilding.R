library(rpart)


build_regression_tree <- function(csv_file_path) {
  data <- read.csv(csv_file_path)
  
  # Stop growing condition
  control <- rpart.control(minsplit = 10, minbucket = 5, maxdepth = 5)
  
  build_node <- function(data, maxdepth) {

    if (nrow(data) < control$minsplit || maxdepth <= 0) {
      return(NULL)
    }
    
    col <- column(data)
    split_value <- number(data, col)
    left_data <- subset(data, data[[col]] <= split_value)
    right_data <- subset(data, data[[col]] > split_value)
    
    if (nrow(left_data) < control$minbucket || nrow(right_data) < control$minbucket) {
      return(NULL)
    }
    
    left_child <- build_node(left_data, maxdepth - 1)
    right_child <- build_node(right_data, maxdepth - 1)
    
    node <- list(
      col = col,
      split_value = split_value,
      left_child = left_child,
      right_child = right_child
    )
    
    return(node)
    
  }
  

  regression_tree <- build_node(data, control$maxdepth)
  

  return(regression_tree)
}


regression_tree <- build_regression_tree("C:/Users/10068/Desktop/Poison/data_clean.csv")


print(regression_tree)