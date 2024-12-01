# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster")
# Load all libraries
lapply(required_packages, library, character.only = TRUE)
# importing data
data = read_csv('data/index_of_economic_freedom_2024.csv')

colnames(data)
glimpse(data)
# calulating na values
colSums(is.na(data))
# remove na values
cleaned_data = na.omit(data)
# sorting overall score from high to low
cleaned_data<- cleaned_data%>%
  arrange(desc(Overall_Score))

# data type
str(cleaned_data)

#visualize
ggplot(cleaned_data, aes(x = Region, y = Overall_Score, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Overall Score",
    x = "Region",
    y = "Overall_Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

