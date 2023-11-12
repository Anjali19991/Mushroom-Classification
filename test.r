library(openxlsx)

data <- read.csv("mushrooms.csv")

string_columns <- names((sapply(data, is.character)))


for (column in string_columns) {
  data[[column]] <- as.numeric(factor(data[[column]],
    levels = unique(data[[column]])
  ))
}


cor_matrix <- cor(data)

# write.xlsx(
#   as.table(cor_matrix),
#   file = "cor.matrix.xlsx",
#   rowNames = TRUE,
#   colNames = TRUE
# )

"
    ring.type
    spore.print.color
    stalk.surface.below.ring
    stalk.surface.above.ring
    gill.size
    odor
    bruises
    
"