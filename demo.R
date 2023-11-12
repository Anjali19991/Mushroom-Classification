options(expressions = 500000)

# GROUP - 15
# IDA PROJECT
# MUSHROOM CLASSIFICATION


# ================================== Here is a brief description about our data-set =================================#


# For the mushroom data-set, it's essential to understand the meaning and context of each column.
# Given the potential risk associated with mushroom consumption (poisonous vs. edible), understanding the features is crucial for analysis.
# Here's a generalized description of what each column might represent based on typical mushroom data-sets:

#  The "Mushroom Classification" data-set usually includes the following columns:

# 0. A target column which tell whether the mushroom is edible or not.
# 1. Cap Shape: Describes the shape of the mushroom's cap (e.g., bell, conical, convex, flat, knobbed, sunken).
# 2. Cap Surface: Indicates the texture of the mushroom's cap (e.g., fibrous, grooves, scaly, smooth).
# 3. Cap Color: Represents the color of the mushroom's cap (e.g., brown, yellow, white, gray, red, etc.).
# 4. Bruises: Specifies if the mushroom bruises easily (bruises, no bruises).
# 5. Odor: Describes the odor of the mushroom (e.g., almond, anise, none, foul, etc.).
# 6. Gill Attachment: Indicates how the gills are attached to the stem (e.g., free, attached).
# 7. Gill Spacing: Represents the spacing between gills (e.g., close, crowded).
# 8. Gill Size: Specifies the size of the gills (e.g., broad, narrow).
# 9. Gill Color: Represents the color of the gills (varies with different mushrooms).
# 10. Stalk Shape: Describes the shape of the mushroom's stalk (e.g., enlarging, tapering).
# 11. Stalk Root: Indicates the root of the mushroom's stalk (e.g., bulbous, club, cup, equal, rhizomorphs, rooted, missing).
# 12. Stalk Surface Above Ring: Describes the texture of the stalk surface above the ring (e.g., fibrous, scaly, smooth, silky).
# 13. Stalk Surface Below Ring: Represents the texture of the stalk surface below the ring (similar to above).
# 14. Stalk Color Above Ring: Indicates the color of the stalk above the ring.
# 15. Stalk Color Below Ring: Represents the color of the stalk below the ring.
# 16. Veil Type: Describes the veil type (partial, universal).
# 17. Veil Color: Represents the color of the veil.
# 18. Ring Number: Specifies the number of rings on the mushroom.
# 20. Ring Type: Describes the type of ring (cobwebby, evanescent, flaring, large, none, pendant, sheathing, zone).
# 21. Spore Print Color: Represents the color of the spore print.
# 22. Population: Describes the population of the mushrooms (abundant, clustered, numerous, scattered, several, solitary).
# 23. Habitat: Specifies the habitat where the mushrooms are found (e.g., grasses, leaves, meadows, paths, urban, waste, woods).
# Each column in the data-set provides specific attributes or characteristics of the mushrooms that aid in their classification as either edible or poisonous.
# Understanding these features helps in analyzing and building a model to predict the mushroom's edibility based on these attributes.

# ................................................................................................................
# Hence our goal is to build a model which determines whether a mushroom is poisonous or not based on its features
# ................................................................................................................



# ============================================================================================================================================================

#-------------------------------------------------------------------------------
# -----------------Loading required libraries-----------------------------------
#-------------------------------------------------------------------------------

# install.packages("ggplot2")
library(data.tree)
library(dplyr)
library(data.table)
library(ggplot2)
library(RWeka)
library(C50)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


#-------------------------------------------------------------------------------
#------------------ Loading the data set----------------------------------------
#-------------------------------------------------------------------------------

data <- read.csv("mushrooms.csv")


#-------------------------------------------------------------------------------
# ------------------ Understanding our data-set --------------------------------
#-------------------------------------------------------------------------------


# shows the first 5 rows of our data set
#---------------------------------------

head(data, 5)

# Finding the dimension of our data set
#--------------------------------------

print("Dimension of the dataset is :")
print(dim(data))

print(paste("Number of columns : ", ncol(data))) # from here we can conclude that there are in total 23 attributes.
print(paste("Number of rows : ", nrow(data))) # We have 8124 records

# Finding the structure of our data-set (column names, types, etc.):
#-----------------------------------------------------------------

str(data)
# From here we can conclude that all our attributes have character data .

#-------------------------------------------------------------------------------
#------------------------------ DATA CLEANING ----------------------------------
#-------------------------------------------------------------------------------


# Looking for null value
#-----------------------

# Check if data frame is NULL


print(paste("Null values in the data-set : ", is.null(data)))

# Since the result is false , we can say that there are no null row-values in the data-set.

# Check for missing values
#--------------------------

print(paste("Missind data : ", sum(is.na(data))))

# Since the count for NA's rows is 0 , we can conclude that our data-set has no row-missing values.

# Hence finally we can conclude here that our data set has no missing or null values.

# Looking for duplicate rows
#------------------------------

# count number of duplicate rows

print(paste("Count for duplicate rows : ", nrow(data[duplicated(data), ])))

# Since the outcome is zero we can conclude that there are no duplicate rows in our data-set

# checking for noise in our data-set and missing values for each column :
#------------------------------------------------------------------------

for (i in 1:23) {
  unique_values <- unique(data[[i]])
  print(paste("Unique values in column ", i, " are: "))
  print(unique_values)
}

# Looking at the result we found that there are unknown values in column 12 which is 'stalk-root' column of our data-set
# Which are identified by ? int the cells.
# Let us now try to find the count of such values.

count_question_mark <- sum(data$stalk.root == "?")
print(count_question_mark)

# We found that the count of ? in stalk-root column is 2480 which is pretty high compared to total number of rows in the data-set.
# This gives us a hint to investigate what all attributes are actually needed for our need for our model .
# Because attributes with only one kind of values and attribute with all rows having unique value do not contribute much towards our goal.
# Hence we try finding the cardiniality of each attribute.

#-------------------------------------------------------------------------------
#---------------------- Exploring Our Data-set ---------------------------------
#-------------------------------------------------------------------------------

# Finding number of unique values in each columnes
#------------------------------------------------

object_columns <- sapply(data, is.character)
result <- sapply(data[object_columns], function(x) length(unique(x)))
print(result)

# Dropping columns with very low/high cardinality
#----------------------------------------------------

dim(data)

columns_to_drop <- c(
  "bruises",
  "gill.attachment",
  "gill.spacing",
  "gill.size",
  "stalk.shape",
  "veil.type"
  # "cap.surface",
  # "stalk.surface.above.ring",
  # "stalk.surface.below.ring",
  # "veil.color",
  # "ring.number",
  # "stalk.root",
  # "ring.type"
)

data <- data[, !(names(data) %in% columns_to_drop)]
# data <- data %>% select(-one_of(columns_to_drop))

dim(data)


# Count for edible vs poisonous mushrooms in our data-set
#--------------------------------------------------------


A <- c(sum(data$class == "e"), sum(data$class == "p"))
B <- c("Edible", "Poisonous")

barplot(A,
  names.arg = B, xlab = "Type of Mushroom",
  ylab = "Count", main = "Count : Edible vs Poisonous Mushroom", col = "pink"
)

# Frequency of each attribute
#-----------------------------

plot_unique_frequency <- function(data) {
  for (col in names(data)) {
    if (
      class(data[[col]]) %in% c("character", "factor") 
      && length(unique(data[[col]])) > 0
    ) {
      unique_counts <- as.data.frame(table(data[[col]]))

      # Plotting bar plot for each attribute
      p <- ggplot(unique_counts, aes(x = Var1, y = Freq)) +
        geom_bar(stat = "identity", fill = "skyblue") +
        labs(
          title = paste("Frequency of Unique Values for", col),
          x = col,
          y = "Frequency"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))

      print(p)
    }
  }
}

plot_unique_frequency(data)


# Our data is moderately balanced

# Lets us try finding the major habitats of poisonous mushrooms and edible mushrooms
#-----------------------------------------------------------------------------------

df_grp_region <- data %>%
  group_by(habitat) %>%
  summarise(
    poisonous_frequency = sum(class == "p"),
    edible_frequency = sum(class == "e"),
    .groups = "drop"
  )

View(df_grp_region)
print(df_grp_region)

df_grp_region_long <- tidyr::pivot_longer(df_grp_region,
  cols = c(poisonous_frequency, edible_frequency),
  names_to = "Class", values_to = "Frequency"
)

df_grp_region_long


ggplot(df_grp_region_long, aes(x = habitat, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Habitat", y = "Frequency", fill = "Class") +
  scale_fill_manual(values = c("green", "red"), labels = c("Edible", "Poisonous")) +
  ggtitle("Habitat Distribution: Edible Vs Poisonous") +
  theme_minimal()




# With the help of graph we can see that most of the poisonous mushrooms grow across paths




# Lets try finding out the populations these mushrooms usually fall in
#----------------------------------------------------------------------


df_grp_population <- data %>%
  group_by(population) %>%
  summarise(
    poisonous_frequency = sum(class == "p"),
    edible_frequency = sum(class == "e"),
    .groups = "drop"
  )

View(df_grp_population)
print(df_grp_population)

df_grp_population_long <- tidyr::pivot_longer(df_grp_population,
  cols = c(poisonous_frequency, edible_frequency),
  names_to = "Class", values_to = "Frequency"
)


ggplot(df_grp_population_long, aes(x = population, y = Frequency, fill = Class)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "population", y = "Frequency", fill = "Class") +
  scale_fill_manual(values = c("green", "red"), labels = c("Edible", "Poisonous")) +
  ggtitle("Population Distribution: Edible Vs Poisonous") +
  theme_minimal()

# From graph we can see that most poisonous mushrooms follow several population

# Lets do the correlation analysis of the attributes in the data-set
#-------------------------------------------------------------------


# columns <- names(data)

# for (i in 1:length(columns)) {
#   for (j in 1:length(columns)) {
#     if (is.factor(data[[columns[i]]]) && is.factor(data[[columns[j]]])) {
#       if (i != j) {  # Exclude cases when i equals j
#         cross_table <- table(data[[columns[i]]], data[[columns[j]]])
#         if (sum(cross_table) > 0) {  # Ensure there is data in the contingency table
#           if (sum(dim(cross_table) > 1) == 2) {  # Check if the table has at least two dimensions
#             result <- chisq.test(cross_table)
#             print(paste("Chi-square test between", columns[i], "and", columns[j], ":", "p-value =", result$p.value))
#           } else {
#             print(paste("No variability in the contingency table:", columns[i], "and", columns[j]))
#           }
#         } else {
#           print(paste("No data in the contingency table:", columns[i], "and", columns[j]))
#         }
#       }
#     }
#   }
# }


# Target - class: e = edible; p = poisonous

# Function to calculate entropy
# calculate_entropy <- function(target) {
#   # Non-zero probability that an arbitrary tuple in D belongs to a particular class
#   probability <- table(target) / length(target)

#   entropy <- -sum(probability * log2(probability))
#   return(entropy)
# }

# # Function to calculate information gain
# # Gain = Entropy - info_still_need
# calculate_information_gain <- function(data, feature, target) {
#   total_entropy <- calculate_entropy(data[[target]])
#   unique_values <- unique(data[[feature]])
#   weighted_entropy <- 0

#   # Calculate weighted average of entropies for different values of the feature
#   for (value in unique_values) {
#     subset_data <- data[data[[feature]] == value, ]
#     weighted_entropy <- weighted_entropy + (nrow(subset_data) / nrow(data))
#   }

#   information_gain <- total_entropy - weighted_entropy
#   return(information_gain)
# }

# # Initialize the root of the decision tree
# # root <- list()
# # root[["Feature"]] <- NULL  # Initially, the root has no feature
# # root[["Children"]] <- list()


# # Function to create a new tree node
# newTreeNode <- function(feature = character(0)) {
#   return(Node$new(feature))
# }



# # Function to build decision tree
# build_decision_tree <- function(node, data, target) {
#   # current_target <- target[nrow(target)]
#   # Stopping condition: All the data points have the same class
#   if (length(unique(data[[target]])) == 1) {
#     return(unique(data[[target]]))
#   }

#   # Stopping condition: No features left to split on
#   if (ncol(data) == 1) {
#     return(colnames(data))
#   }

#   # Find the feature with the highest information gain
#   columns <- colnames(data)
#   features <- columns[-which(columns == target)]
#   information_gains <- sapply(
#     features,
#     calculate_information_gain,
#     data = data,
#     target = target
#   )
#   best_feature <- features[which.max(information_gains)]

#   # Split based on the best feature
#   unique_values <- unique(data[[best_feature]])
#   for (value in unique_values) {
#     subset_data <- data[data[[best_feature]] == value, ]
#     if (nrow(subset_data) == 0) {
#       # If there are no data points, choose the majority class from the parent
#       majority_class <- colnames(sort(table(data[[target]], decreasing = TRUE)))[1] # nolint
#       node$AddChild(majority_class) # nolint
#     } else {
#       # Recursively build the tree
#       child <- node$AddChild(value) # Initialize child node # nolint
#       build_decision_tree(child, subset_data, target)
#     }
#   }
# }

# # Update the root initialization using the newTreeNode function
# target <- colnames(data)[1]
# root <- newTreeNode(target)


# # Build decision tree
# decision_tree <- build_decision_tree(
#   root,
#   data,
#   target = target
# )

# # Function to make prediction on new data
# make_prediciton <- function(tree, new_data) {
#   while (!is.character(tree@Feature)) {
#     feature <- tree@Feature
#     feature_value <- as.character(new_data[[feature]])

#     if (!feature_value %in% colnames(tree@Children)) {
#       # If feature value is not in the tree, return the majority class
#       majority_class <- colnames(sort(table(data$class, decreasing = TRUE))[1])
#       return(majority_class)
#     }
#     tree <- tree@Children[[feature_value]]
#   }
# }

# # Example predictions
# new_data_point <- data.frame(
#   cap.shape = "x",
#   cap.surface = "y",
#   cap.color = "w",
#   bruises = "f",
#   odor = "a",
#   gill.attachment = "f",
#   gill.spacing = "w",
#   gill.size = "b",
#   gill.color = "g",
#   stalk.shape = "e",
#   stalk.root = "c",
#   stalk.surface.above.ring = "s",
#   stalk.surface.below.ring = "s",
#   stalk.color.above.ring = "w",
#   stalk.color.below.ring = "w",
#   viel.type = "p",
#   viel.color = "w",
#   ring.number = "o",
#   ring.type = "e",
#   spore.print.color = "k",
#   population = "n",
#   habitat = "g"
# )
# prediction <- make_prediciton(decision_tree, new_data_point)
# cat("Predicted Class: ", prediction)
string_columns <- sapply(data, is.character)
# data <- cbind(data[, !string_columns, drop = FALSE], model.matrix(~ . - 1, data = data[, string_columns]))
model <- C5.0(data[, -which(names(data) == "class")], as.factor(data$class))
summary(model)