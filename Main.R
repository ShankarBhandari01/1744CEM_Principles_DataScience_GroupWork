# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster")
# Load all libraries
lapply(required_packages, library, character.only = TRUE)
# importing data
data = read_csv('data/index_of_economic_freedom_2024.csv')
print(data)

print(colnames(data))
