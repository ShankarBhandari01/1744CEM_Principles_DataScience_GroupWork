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
  ggtitle("PCA Biplot")




# Scatter matrix including PCs
pca_plus = data_pca %>%
  mutate(PC1=pca$x[,1],
         PC2=pca$x[,2],
         PC3=pca$x[,3]) %>%
  relocate(PC1,PC2,PC3)
ggpairs(pca_plus,)




#B

#Pillar #1 Rule of Law – property rights, government integrity, and judicial effectiveness.

# Subset the data to exclude Pillar #1 variables
data_without_pillar1 <- cleaned_data %>%
  select(Overall_Score, Tax_Burden, Government_Spending, Fiscal_Health, Business_Freedom, 
         Labor_Freedom, Monetary_Freedom, Trade_Freedom, Investment_Freedom, Financial_Freedom)

# Perform PCA without Pillar #1 variables
pca_without_pillar1 <- prcomp(data_without_pillar1[, -1], scale = TRUE)

# Variance Explained by Each Principal Component
summary(pca_without_pillar1)

# Scree Plot
fviz_screeplot(pca_without_pillar1, addlabels = TRUE,barfill = "blue", barcolor = "black")

# Biplot for first two PCs
fviz_pca_biplot(pca_without_pillar1, axes = c(1, 2), label = "var", addEllipses = TRUE)

# PCA loadings plot
loadings = as.data.frame(pca_without_pillar1$rotation[,1:4])
loadings$Symbol = row.names(loadings)
loadings = gather(loadings, key='Component', value='Weight', -Symbol)
ggplot(loadings, aes(x=Symbol,y=Weight)) +
  geom_bar(stat='identity') +
  facet_grid(Component~.)

# dissimilarity matrix Manhattan 
dissimilarity_manhattan <- dist(scale(data_pca), method = "manhattan")


# hierarchical clustering methods
cluster_results_manhattan_ward <- agnes(dissimilarity_manhattan, method = "ward")
cat("Agglomerative Coefficient manhattan_ward: ", cluster_results_manhattan_ward$ac, "\n")
cluster_results_manhattan_single <- agnes(dissimilarity_manhattan, method = "single")
cat("Agglomerative Coefficient manhattan_single: ", cluster_results_manhattan_single$ac, "\n")

# plot cluster using ward methods
plot(cluster_results_manhattan_ward, which.plots=2)
rect.hclust(cluster_results_manhattan_ward, k=4)
cutree(cluster_results_manhattan_ward, k=4)


# plot cluster results single linkage
plot(cluster_results_manhattan_single, which.plots=2)
rect.hclust(cluster_results_manhattan_single, k=4)
cutree(cluster_results_manhattan_single, k=4)

#dissimilarity matrix Euclidean distance
dissimilarity_euclidean <- dist(scale(data_pca), method = "euclidean")

# hierarchical clustering methods
cluster_results_euclidean_ward <- agnes(dissimilarity_euclidean, method = "ward")
cat("Agglomerative Coefficient euclidean_ward: ", cluster_results_euclidean_ward$ac, "\n")
cluster_results_euclidean_single <- agnes(dissimilarity_manhattan, method = "single")
cat("Agglomerative Coefficient euclidean_single: ", cluster_results_euclidean_single$ac, "\n")


# cluster with ward methods
plot(cluster_results_euclidean_ward, which.plots=2)
rect.hclust(cluster_results_euclidean_ward, k=4)
cutree(cluster_results_euclidean_ward, k=4)

#cluster with single linkage methods
plot(cluster_results_euclidean_single, which.plots=2)
rect.hclust(cluster_results_euclidean_single, k=4)
cutree(cluster_results_euclidean_single, k=4)

# Create a data frame to summarize the results
comparison_table_row <- data.frame(
  Distance = c("Euclidean", "Manhattan", "Euclidean", "Manhattan"),
  Method = c("Ward", "Ward", "Single", "Single"),
  Agglomerative_Coefficient = c(
    cluster_results_euclidean_ward$ac,
    cluster_results_manhattan_ward$ac,
    cluster_results_euclidean_single$ac,
    cluster_results_manhattan_single$ac
  )
)

# Print the comparison table
print(comparison_table_row)


# Cluster the variables using manhattan with ward method cluster methods
dissimilarity_manhattan_column_ward = dist(t(scale(data_pca)), method="manhattan") # note t() is transpose
cluster_results_manhattan_column_ward = agnes(dissimilarity_manhattan_column_ward, method="ward")
plot(cluster_results_manhattan_column_ward, which.plots=2)
rect.hclust(cluster_results_manhattan_column_ward, k=4)
cutree(cluster_results_manhattan_column_ward, k=4)

# Cluster the variables using manhattan with single linkage method cluster methods
dissimilarity_manhattan_column_single = dist(t(scale(data_pca)), method="manhattan") # note t() is transpose
cluster_results_manhattan_column_single = agnes(dissimilarity_manhattan_column_single, method="single")
plot(cluster_results_manhattan_column_single, which.plots=2)
rect.hclust(cluster_results_manhattan_column_single, k=4)
cutree(cluster_results_manhattan_column_single, k=4)


# Cluster the variables using euclidean with ward method cluster methods
dissimilarity_euclidean_column_ward = dist(t(scale(data_pca)), method="euclidean") # note t() is transpose
cluster_results_euclidean_column_ward = agnes(dissimilarity_euclidean_column_ward, method="ward")
plot(cluster_results_euclidean_column_ward, which.plots=2)
rect.hclust(cluster_results_euclidean_column_ward, k=4)
cutree(cluster_results_euclidean_column_ward, k=4)

# Cluster the variables using euclidean with single linkage method cluster methods
dissimilarity_euclidean_column_single = dist(t(scale(data_pca)), method="euclidean") # note t() is transpose
cluster_results_euclidean_column_single = agnes(dissimilarity_euclidean_column_single, method="single")
plot(cluster_results_euclidean_column_single, which.plots=2)
rect.hclust(cluster_results_euclidean_column_single, k=4)
cutree(cluster_results_euclidean_column_single, k=4)



# Create a data frame to summarize the results
comparison_table_column <- data.frame(
  Distance = c("Euclidean", "Manhattan", "Euclidean", "Manhattan"),
  Method = c("Ward", "Ward", "Single", "Single"),
  Agglomerative_Coefficient = c(
    cluster_results_euclidean_column_ward$ac,
    cluster_results_manhattan_column_ward$ac,
    cluster_results_euclidean_column_single$ac,
    cluster_results_manhattan_column_single$ac
  )
)

print(comparison_table_column)
