# List of required libraries
required_packages <- c("tidyverse", "GGally", "ggfortify", "cluster", "factoextra")

# Load all libraries
lapply(required_packages, library, character.only = TRUE)
# importing data
data = read_csv('data/index_of_economic_freedom_2024.csv')

colnames(data)
glimpse(data)

# Remove NA values from givend data sets 
cleaned_data = na.omit(data)

# Sorting Overall_Score from highest to lowest
cleaned_data<- cleaned_data %>%
              arrange(desc(Overall_Score))

# data type
str(cleaned_data)

#Visualization of data using Region wise along with out liar
ggplot(cleaned_data, aes(x = Region, y = Overall_Score, fill = Region)) +
  geom_boxplot() +
  labs(
    title = "Distribution of Overall Score by Region",
    x = "Region",
    y = "Overall_Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#----------> PCA Analysis of 12 pillar variables-------------------------
#==================== Part A ============================================

pillar_data <- cleaned_data %>%
               select(Property_Rights:Financial_Freedom)

glimpse(pillar_data)


#Standardize the data
pillar_data_scaled <- scale(pillar_data)
pillar_data_scaled

# Step 2: Performing PCA
pca_result <- prcomp(pillar_data_scaled, scale. = TRUE)
summary(pca_result)

# Calculation of each PCA variance 
variance = pca_result$sdev^2 / sum(pca_result$sdev^2)
variance

# Extract eigenvalues/variances
get_eig(pca_result)


#---> Step 3: Visualizations using Scree plot (X = 1 to 12 pillar) <-----
#=========================================================================
ggplot(NULL,aes(x=1:12,y=100*variance)) +
  geom_col()+
  ggtitle("Screeplot of 12 piller variance")


#---- PCA result visualization using Factroextra scree-plots
#========================================================================
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 60))



#----> PCA bi-plot-------------------------------------------------------
#========================================================================
autoplot(pca_result,
         label=TRUE, label.size=3, shape=FALSE,
         loadings=TRUE, loadings.label=TRUE)


#-------- PCA bi-plot of 12 pillar variables using factoextra library
#========================================================================
fviz_pca_biplot(pca_result, 
                repel = TRUE, 
                axex=c(1,2),
                geom.ind = "point", 
                label ="var", 
                addEllipses = TRUE, 
                palette = c("grey"))


#----->  PCA loading plot of 12 pillar variables  -----------------------
#========================================================================

loadings = as.data.frame(pca_result$rotation[,1:3])
loadings$Symbol = row.names(loadings)
loadings = gather(loadings, key='Component', value='Weight', -Symbol)

ggplot(loadings, aes(x=Symbol,y=Weight)) +
  geom_bar(stat='identity') + 
  facet_grid(Component~.)


#---------> part B ------------------------------------------------------
#========================================================================
#filtration and removing outlier from the cleaned data sets 
#----------- it is because 175 and 176 are far from the group of data 
#
#cleaned_data = cleaned_data %>%
#              filter(!Country %in% c("Cuba", "North Korea"))
#cleaned_data
#========================================================

# Extract the 9 pillar variables after omitting pillar 1 variable 
pillar_data <- cleaned_data %>%
  select(Tax_Burden:Financial_Freedom)
glimpse(pillar_data)


#Standardize the data
pillar_data_scaled <- scale(pillar_data)
pillar_data_scaled

#----------> Step 2: Performing PCA of 9 pillar variables -----------------
#==========================================================================
pca_result <- prcomp(pillar_data_scaled, scale. = TRUE)
summary(pca_result)

#--------> Calculating of each PCA variance using formula -----------------
variance = pca_result$sdev^2/sum(pca_result$sdev^2)
variance

# Extract eigenvalues/variances
get_eig(pca_result)

# Step 3: Visualizations using Scree plot PCA 9 pillar variables  (X = 1 to 9 pillar)
ggplot(NULL,aes(x=1:9,y=100*variance)) +
  geom_col()+
  ggtitle("Screeplot of 9 piller variance")


#---- using factoextra library to plot scree-plots------------------
#===================================================================
fviz_screeplot(pca_result, addlabels = TRUE, ylim = c(0, 50))


#--------> PCA bi plot of 9 pillar variables ----------------------- 
#====================================================================
autoplot(pca_result,
         label=TRUE, label.size=3, shape=FALSE,
         loadings=TRUE, loadings.label=TRUE)


#-----> PCA bi plot in factoextra of 9 pillar variables -----------
#===================================================================

fviz_pca_biplot(pca_result, 
                repel = TRUE, 
                axex=c(1,2),
                geom.ind = "point", 
                label ="var", 
                addEllipses = TRUE, 
                palette = c("grey"))


#------ PCA loading plot 9 pillar variables ---------------------------
#======================================================================
loadings = as.data.frame(pca_result$rotation[,1:4])
loadings$Symbol = row.names(loadings)
loadings = gather(loadings, key='Component', value='Weight', -Symbol)

ggplot(loadings, aes(x=Symbol,y=Weight)) +
  geom_bar(stat='identity') + 
  facet_grid(Component~.)





#============================================================
#------------------> Part 2 - A Row wise <-------------------
#--------- Clustering using Manhattan ward method------------
#============================================================
D = dist(scale(pillar_data), method="manhattan")
cluster_results = agnes(D, method="ward")
plot(cluster_results, which.plots=2)
rect.hclust(cluster_results, k=4)
cutree(cluster_results, k=4)
print(cluster_results$ac)




#============================================================
#------- Clustering using Manhattan single linkage method-----
#=============================================================
D = dist(scale(pillar_data), method="manhattan")
cluster_results = agnes(D, method="single")
plot(cluster_results, which.plots=2)
rect.hclust(cluster_results, k=4)
cutree(cluster_results, k=4)
print(cluster_results$ac)
####


#===============================================================
#--- Clustering using distance matrix Euclidean ward method ----
#===============================================================

D = dist(scale(pillar_data), method="euclidean")
cluster_results = agnes(D, method="ward")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=4) 
cutree(cluster_results, k=4)
print(cluster_results$ac)


#=========================================================================
#--- Clustering using distance matrix Euclidean single Linkage method ----
#=========================================================================
D = dist(scale(pillar_data), method="euclidean")
cluster_results = agnes(D, method="single")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=5) 
cutree(cluster_results, k=5)
print(cluster_results$ac)



#------> Table creation for algomerative coefficient -----
#=========================================================

#Table 



#

#============================================================
#------------------> Part 2 - B Column wise <-------------------
#--------- Clustering using Manhattan ward method------------
#============================================================

D = dist(t(scale(pillar_data)), method="manhattan")
cluster_results = agnes(D, method="ward")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=5) 
cutree(cluster_results, k=5)
print(cluster_results$ac)


#============================================================
#------- Clustering using Manhattan single linkage method-----
#=============================================================

D = dist(t(scale(pillar_data)), method="manhattan")
cluster_results = agnes(D, method="single")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=5) 
cutree(cluster_results, k=5)
print(cluster_results$ac)


#============================================================
#------- Clustering using Euclidean single ward method-----
#=============================================================

D = dist(t(scale(pillar_data)), method="euclidean")
cluster_results = agnes(D, method="ward")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=5) 
cutree(cluster_results, k=5)
print(cluster_results$ac)


#============================================================
#------- Clustering using Euclidean single linkage method-----
#=============================================================
D = dist(t(scale(pillar_data)), method="euclidean")
cluster_results = agnes(D, method="single")
plot(cluster_results, which.plots=2) 
rect.hclust(cluster_results, k=5) 
cutree(cluster_results, k=5)
print(cluster_results$ac)


























