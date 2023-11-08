# options(expressions = 500000)

library(data.tree)
data <- read.csv("mushrooms.csv")

# Target - class: e = edible; p = poisonous

# Function to calculate entropy
calculate_entropy <- function(target) {
  # Non-zero probability that an arbitrary tuple in D belongs to a particular class
  probability <- table(target) / length(target)

  entropy <- -sum(probability * log2(probability))
  return(entropy)
}

# Function to calculate information gain
# Gain = Entropy - info_still_need
calculate_information_gain <- function(data, feature, target) {
  total_entropy <- calculate_entropy(data[[target]])
  unique_values <- unique(data[[feature]])
  weighted_entropy <- 0

  # Calculate weighted average of entropies for different values of the feature
  for (value in unique_values) {
    subset_data <- data[data[[feature]] == value, ]
    weighted_entropy <- weighted_entropy + (nrow(subset_data) / nrow(data))
  }

  information_gain <- total_entropy - weighted_entropy
  return(information_gain)
}

# Initialize the root of the decision tree
# root <- list()
# root[["Feature"]] <- NULL  # Initially, the root has no feature
# root[["Children"]] <- list()


# Function to create a new tree node
newTreeNode <- function(feature = character(0)) {
  return(Node$new(feature))
}



# Function to build decision tree
build_decision_tree <- function(node, data, target) {
  # current_target <- target[nrow(target)]
  # Stopping condition: All the data points have the same class
  if (length(unique(data[[current_target]])) == 1) {
    return(unique(data[[current_target]]))
  }

  # Stopping condition: No features left to split on
  if (ncol(data) == 1) {
    return(colnames(data))
  }

  # Find the feature with the highest information gain
  columns <- colnames(data)
  features <- columns[-which(columns == target)]
  information_gains <- sapply(
    features,
    calculate_information_gain,
    data = data,
    target = target
  )
  best_feature <- features[which.max(information_gains)]

  # Set the feature of the current node
  node@Feature <- best_feature

  # Split based on the best feature
  unique_values <- unique(data[[best_feature]])
  print(unique_values)
  for (value in unique_values) {
    subset_data <- data[data[[best_feature]] == value, ]
    if (nrow(subset_data) == 0) {
      # If there are no data points, choose the majority class from the parent
      majority_class <- colnames(sort(table(data[[target]], decreasing = TRUE)))[1]# nolint
      node$AddChild(majority_class) # nolint
    } else {
      # Recursively build the tree
      child <- node$AddChild(value) # Initialize child node # nolint
      build_decision_tree(child, subset_data, target)
    }
  }
}

# Update the root initialization using the newTreeNode function
root <- newTreeNode(NULL)


# Build decision tree
decision_tree <- build_decision_tree(
  root,
  data,
  target = data$class
)

# Function to make prediction on new data
make_prediciton <- function(tree, new_data) {
  while (!is.character(tree@Feature)) {
    feature <- tree@Feature
    feature_value <- as.character(new_data[[feature]])

    if (!feature_value %in% colnames(tree@Children)) {
      # If feature value is not in the tree, return the majority class
      majority_class <- colnames(sort(table(data$class, decreasing = TRUE))[1])
      return(majority_class)
    }
    tree <- tree@Children[[feature_value]]
  }
}

# Example predictions
new_data_point <- data.frame(
  cap.shape = "x",
  cap.surface = "y",
  cap.color = "w",
  bruises = "f",
  odor = "a",
  gill.attachment = "f",
  gill.spacing = "w",
  gill.size = "b",
  gill.color = "g",
  stalk.shape = "e",
  stalk.root = "c",
  stalk.surface.above.ring = "s",
  stalk.surface.below.ring = "s",
  stalk.color.above.ring = "w",
  stalk.color.below.ring = "w",
  viel.type = "p",
  viel.color = "w",
  ring.number = "o",
  ring.type = "e", 
  spore.print.color = "k",
  population = "n",
  habitat = "g"
)
prediction <- make_prediciton(decision_tree, new_data_point)
cat("Predicted Class: ", prediction)
