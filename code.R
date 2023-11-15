library(openxlsx)
library(caret)
library(pROC)

data <- read.csv("/Users/bbp29/OneDrive/Desktop/M2023/IDA/Project/mushrooms.csv")

string_columns <- names((sapply(data, is.character)))

for (column in string_columns) {
  data[[column]] <- as.numeric(factor(data[[column]],
                                      levels = unique(data[[column]])
  ))
}


cor_matrix <- cor(data)

print(ncol(data))


write.xlsx(
  as.table(cor_matrix),
  file = "cor.matrix.xlsx",
  rowNames = TRUE,
  colNames = TRUE
)

columns_to_remove <- integer(0)

for (i in seq(1, ncol(data))) {
  print(i)
  print(abs(cor_matrix[i]))
  if(!is.na(cor_matrix[i]) && abs(cor_matrix[i]) < .5) {
    columns_to_remove <- c(columns_to_remove, i)
  }
}

data <- data[, -columns_to_remove]
data <- data[, -which(names(data) == "veil.type")]

ncol(data)
summary(data)
print(data)

# b) Split the data randomly into 2:1 ratio
set.seed(123) # Set seed for reproducibility
train_indices <- createDataPartition(data$class, p = 2/3, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Actual labels of the test class
actual_labels <- as.factor(test_data$class)

# Function to calculate entropy
calculate_entropy <- function(labels) {
  probabilities <- table(labels) / length(labels)
  -sum(probabilities * log2(probabilities + 1e-10))
}

# Function to calculate information gain
calculate_information_gain <- function(data, feature, target) {
  total_entropy <- calculate_entropy(data[[target]])
  feature_levels <- unique(data[[feature]])
  
  weighted_entropy <- sum(
    sapply(feature_levels, function(level) {
      subset_data <- data[data[[feature]] == level, ]
      weight <- nrow(subset_data) / nrow(data)
      weight * calculate_entropy(subset_data[[target]])
    })
  )
  
  information_gain <- total_entropy - weighted_entropy
  information_gain
}

# Function to find the best split
find_best_split <- function(data, features, target) {
  information_gains <- sapply(features, function(feature) {
    calculate_information_gain(data, feature, target)
  })
  best_feature <- features[which.max(information_gains)]
  best_feature
}

# Function to build the decision tree
build_decision_tree <- function(data, features, target) {
  if (length(unique(data[[target]])) == 1) {
    # If all examples have the same label, create a leaf node
    return(data[[target]][1])
  }
  
  if (length(features) == 0) {
    # If there are no features left, create a leaf node with the majority label
    majority_label <- names(sort(table(data[[target]]), decreasing = TRUE)[1])
    return(majority_label)
  }
  
  best_feature <- find_best_split(data, features, target)
  
  tree <- list()
  tree$feature <- best_feature
  tree$children <- list()
  
  feature_levels <- unique(data[[best_feature]])
  for (level in feature_levels) {
    subset_data <- data[data[[best_feature]] == level, ]
    subset_data <- subset_data[, !names(subset_data) %in% best_feature]
    subset_features <- features[features != best_feature]
    subtree <- build_decision_tree(subset_data, subset_features, target)
    tree$children[[as.character(level)]] <- subtree
  }
  
  return(tree)
}

# Example usage
features <- names(data)[-1]
print(features)
target <- names(data)[1]
print(target)
tree <- build_decision_tree(train_data, features, target)

# Print the decision tree
print(tree)


# Function to make predictions on the test set
predict_tree <- function(tree, test_data) {
  predictions <- apply(test_data, 1, function(row) predict_decision_tree(tree, data.frame(t(row))))
  return(predictions)
}

# Predict Decision Tree Function
predict_decision_tree <- function(tree, new_data) {
  while (!is.character(tree) && !is.null(names(tree))) {
    feature <- tree$feature
    feature_value <- as.character(new_data[[feature]])
    
    # Convert feature_value to the same format as in the training data
    feature_value <- as.numeric(factor(feature_value, levels = levels(train_data[[feature]])))
    
    if (!feature_value %in% names(tree$children)) {
      # If feature value is not in the tree, return the majority class
      majority_class <- names(sort(table(train_data$class), decreasing = TRUE))[1]
      return(majority_class)
    }
    
    # Access the child node
    tree <- tree$children[[feature_value]]
  }
  
  return(tree)
}

calculate_precision <- function(predictions, actual, positive_class) {
  true_positive <- sum(predictions == positive_class & actual == positive_class)
  false_positive <- sum(predictions == positive_class & actual != positive_class)
  
  if (true_positive + false_positive == 0) {
    return(0)
  }
  
  precision <- true_positive / (true_positive + false_positive)
  return(precision)
}

calculate_recall <- function(predictions, actual, positive_class) {
  true_positive <- sum(predictions == positive_class & actual == positive_class)
  false_negative <- sum(predictions != positive_class & actual == positive_class)
  
  if (true_positive + false_negative == 0) {
    return(0)
  }
  
  recall <- true_positive / (true_positive + false_negative)
  return(recall)
}

calculate_f1_score <- function(predictions, actual, positive_class) {
  precision <- calculate_precision(predictions, actual, positive_class)
  recall <- calculate_recall(predictions, actual, positive_class)
  
  if (precision + recall == 0) {
    return(0)
  }
  
  f1_score <- 2 * precision * recall / (precision + recall)
  return(f1_score)
}

# Function to calculate accuracy
calculate_accuracy <- function(predictions, actual) {
  correct_predictions <- sum(predictions == actual)
  total_predictions <- length(predictions)
  accuracy <- correct_predictions / total_predictions
  return(accuracy)
}

# Function to calculate ROC curve
calculate_roc_curve <- function(predictions, actual, positive_class) {
  thresholds <- seq(0, 1, 0.01)
  roc_data <- data.frame(TP = numeric(length(thresholds)), FP = numeric(length(thresholds)))
  
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    binary_predictions <- as.integer(predictions == positive_class & actual == positive_class)
    
    # Handle cases where there are no positive predictions
    if (sum(binary_predictions) == 0) {
      roc_data[i, "TP"] <- 0
      roc_data[i, "FP"] <- 0
    } else {
      conf_matrix <- table(binary_predictions, as.integer(actual == positive_class))
      
      # Check if the matrix has the required dimensions
      if (nrow(conf_matrix) >= 2 && ncol(conf_matrix) >= 2) {
        roc_data[i, "TP"] <- conf_matrix[2, 2] / sum(conf_matrix[2, ])
        roc_data[i, "FP"] <- conf_matrix[1, 2] / sum(conf_matrix[1, ])
      } else {
        roc_data[i, "TP"] <- 0
        roc_data[i, "FP"] <- 0
      }
    }
  }
  
  return(roc_data)
}

# Function for k-fold cross-validation
k_fold_cross_validation <- function(data, features, target, k) {
  set.seed(123)
  folds <- sample(1:k, nrow(data), replace = TRUE)
  metrics <- data.frame(
    Precision = numeric(k),
    Recall = numeric(k),
    Accuracy = numeric(k),
    F1_Score = numeric(k)
  )
  
  for (i in 1:k) {
    train_data <- data[folds != i, ]
    test_data <- data[folds == i, ]
    
    tree <- build_decision_tree(train_data, features, target)
    
    predictions <- predict_tree(tree, test_data)
    
    metrics[i, "Precision"] <- calculate_precision(predictions, test_data$class, positive_class = 2)
    metrics[i, "Recall"] <- calculate_recall(predictions, test_data$class, positive_class = 2)
    metrics[i, "Accuracy"] <- calculate_accuracy(predictions, test_data$class)
    metrics[i, "F1_Score"] <- calculate_f1_score(predictions, test_data$class, positive_class = 2)
  }
  
  return(metrics)
}

# Example usage
features <- names(data)[-1]
target <- names(data)[1]
k_fold_metrics <- k_fold_cross_validation(data, features, target = "class", k = 5)

# Print k-fold cross-validation metrics
print(k_fold_metrics)
