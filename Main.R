# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster","ggpubr","factoextra")
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
fviz_screeplot(pca, addlabels = TRUE, barfill = "blue", barcolor = "black") +
  ggtitle("Scree Plot")



# Biplot with highlighted countries
fviz_pca_biplot(
  pca,
  axes = c(1, 2),
  geom.ind = "point",        # Points for individuals  
  palette = c("grey"), # Colors for groups
  addEllipses = TRUE,        # Add confidence ellipses
  label = "var",             # Label variables only
  repel = TRUE              # Avoid text overlapping
) 



# PCA loadings plot
loadings = as.data.frame(pca$rotation[,1:4])
loadings$Symbol = row.names(loadings)
loadings = gather(loadings, key='Component', value='Weight', -Symbol)
ggplot(loadings, aes(x=Symbol,y=Weight)) +
  geom_bar(stat='identity') +
  facet_grid(Component~.)

#“biplot”
fviz_pca_biplot(
  pca,
  axes = c(2, 3),
  geom.ind = "point",        # Points for individuals
  palette = c("grey"), # Colors for groups
  addEllipses = TRUE,        # Add confidence ellipses
  label = "var",             # Label variables only
  repel = TRUE              # Avoid text overlapping
) +
  ggtitle("PCA Biplot with Highlighted Outliers")




# Scatter matrix including PCs
pca_plus = data_pca %>%
  mutate(PC1=pca$x[,1],
         PC2=pca$x[,2],
         PC3=pca$x[,3]) %>%
  relocate(PC1,PC2,PC3)
ggpairs(pca_plus)





