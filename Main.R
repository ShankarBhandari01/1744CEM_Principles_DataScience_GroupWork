# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster","ggpubr")
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

# Select only the 12 pillar variables for PCA (assuming columns for each pillar are named appropriately)
pillar_columns <- c("Property_Rights", "Government_Integrity", "Judicial_Effectiveness", 
                    "Tax_Burden", "Government_Spending", "Fiscal_Health", 
                    "Business_Freedom", "Labor_Freedom", "Monetary_Freedom", 
                    "Trade_Freedom", "Investment_Freedom", "Financial_Freedom")


# Prepare the data for PCA
data_pca <- cleaned_data[, pillar_columns]

# Perform PCA
pca <- prcomp(data_pca, center = TRUE, scale. = TRUE)
summary(pca)  # Summary to show the explained variance

variance_explained = pca$sdev/sum(pca$sdev^2)
variance_explained

#scree
ggplot(NULL,aes(x=1:12,y=100*variance_explained)) +
  geom_col()

#“biplot”
autoplot(pca,
         x=1,y=2,
         label=TRUE, label.size=3, shape=FALSE,
         loadings=TRUE, loadings.label=TRUE)


# Biplot using the first two principal components
biplot(pca, main = "PCA Biplot (PC1 vs PC2)", col = c("blue", "red"))



# PCA loadings plot
loadings = as.data.frame(pca$rotation[,1:5])
loadings$Symbol = row.names(loadings)
loadings = gather(loadings, key='Component', value='Weight', -Symbol)
ggplot(loadings, aes(x=Symbol,y=Weight)) +
  geom_bar(stat='identity') +
  facet_grid(Component~.)
# produce and interpret a biplot using PC2 and PC3 as the axes.

#“biplot”
autoplot(pca,
         x=2,y=3,
         label=TRUE, label.size=3, shape=FALSE,
         loadings=TRUE, loadings.label=TRUE)



# Scatter matrix including PCs
radio_plus = data_pca %>%
  mutate(PC1=pca$x[,1],
         PC2=pca$x[,2],
         PC3=pca$x[,3]) %>%
  relocate(PC1,PC2,PC3)
ggpairs(radio_plus)

