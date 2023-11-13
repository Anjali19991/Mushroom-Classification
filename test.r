# library(openxlsx)

data <- read.csv("mushrooms.csv")

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

for (i in seq(1, ncol(data))) {
  print(i)
  print(abs(cor_matrix[i]))
  if(abs(cor_matrix[i]) > .5) {
    data <- data[, -i]
  }
}

ncol(data)
summary(data)



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
tree <- build_decision_tree(data, features, target)

# Print the decision tree
print(tree)
