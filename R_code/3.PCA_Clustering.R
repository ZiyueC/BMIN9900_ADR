######################## PCA part
library(tidyverse)
library(caret)
library(factoextra)

final_vars <- c("Medication_Dosage", "Age", "Gender", "Race", "Insurance_type", 
                "Marital_Status", "Admission_Length_of_Stay", "used_warfarin", 
                "used_ppi", "used_other_statins", "used_nsaid", "used_clopidogrel", 
                "used_anticoagulant", "used_ace_arb")

df_pca <- na.omit(cohort_data[, c("ADR", final_vars)])
# Modify or select variables
df_pca <- df_pca %>% mutate(ADR = ifelse(ADR == "1", 1, 0))

df_pca_dummy <- dummyVars("~ .", data = df_pca)
# Make predictions using model
df_pca_transformed <- as.data.frame(predict(df_pca_dummy, newdata = df_pca))


df_pca_scaled <- df_pca_transformed %>%
# Modify or select variables
  dplyr::select(-ADR) %>%
  scale()

pca_model <- prcomp(df_pca_scaled, center = TRUE, scale. = TRUE)
fviz_eig(pca_model)

pca_data <- data.frame(ADR = df_pca$ADR, pca_model$x)

# Create a plot
ggplot(pca_data, aes(PC1, PC2, color = as.factor(ADR))) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA: ADR vs. Non-ADR",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "ADR Status") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

# View model summary
summary(pca_model)

library(plotly)
pca_model <- prcomp(df_pca_scaled, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_model$x[, 1:3], ADR = df_pca$ADR)
pca_3d_plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(ADR), 
                       colors = c("blue", "red"), 
                       type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(title = "3D PCA: ADR vs. Non-ADR",
         scene = list(
           xaxis = list(title = 'Principal Component 1'),
           yaxis = list(title = 'Principal Component 2'),
           zaxis = list(title = 'Principal Component 3')
         ),
         margin = list(l = 0, r = 0, b = 0, t = 40))


pca_3d_plot
## new PCA with binning

# Load libraries
library(tidyverse)
library(caret)
library(factoextra)

final_vars <- c("Medication_Dosage", "Age", "Gender", "Race", "Insurance_type", 
                "Marital_Status", "Admission_Length_of_Stay", "used_warfarin", 
                "used_ppi", "used_other_statins", "used_nsaid", "used_clopidogrel", 
                "used_anticoagulant", "used_ace_arb")


df_pca <- na.omit(cohort_data[, c("ADR", final_vars)])


# Modify or select variables
df_pca <- df_pca %>% mutate(ADR = ifelse(ADR == "1", 1, 0))


df_pca <- df_pca %>%
# Modify or select variables
  mutate(Age_group = cut(Age,
                         breaks = c(0, 25, 30, 40, 50, 60, 70, 80, Inf),
                         labels = c("18-25", "26-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81+"),
                         right = FALSE))

df_pca <- df_pca %>%
# Modify or select variables
  mutate(LOS_group = ntile(Admission_Length_of_Stay, 5))  # 5 bins

df_pca <- df_pca %>%
# Modify or select variables
  select(-Age, -Admission_Length_of_Stay)


df_pca_dummy <- dummyVars("~ .", data = df_pca)
# Make predictions using model
df_pca_transformed <- as.data.frame(predict(df_pca_dummy, newdata = df_pca))

df_pca_scaled <- df_pca_transformed %>%
# Modify or select variables
  select(-ADR) %>%
  scale()

pca_model <- prcomp(df_pca_scaled, center = TRUE, scale. = TRUE)

fviz_eig(pca_model)

pca_data <- data.frame(ADR = df_pca$ADR, pca_model$x)

# Create a plot
ggplot(pca_data, aes(PC1, PC2, color = as.factor(ADR))) +
  geom_point(alpha = 0.6) +
  labs(title = "PCA after Binning Age and LOS",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "ADR Status") +
  theme_minimal() +
  scale_color_manual(values = c("0" = "blue", "1" = "red"))

# View model summary
summary(pca_model)

explained_variance <- cumsum(pca_model$sdev^2) / sum(pca_model$sdev^2)
plot(explained_variance, type = "b", xlab = "Number of Principal Components", ylab = "Cumulative Variance Explained", main = "Cumulative Explained Variance")


library(plotly)
pca_model <- prcomp(df_pca_scaled, center = TRUE, scale. = TRUE)
pca_data <- data.frame(pca_model$x[, 1:3], ADR = df_pca$ADR)
pca_3d_plot <- plot_ly(pca_data, x = ~PC1, y = ~PC2, z = ~PC3, color = ~as.factor(ADR), 
                       colors = c("red", "blue"), 
                       type = "scatter3d", mode = "markers", marker = list(size = 5)) %>%
  layout(title = "3D PCA: ADR vs. Non-ADR",
         scene = list(
           xaxis = list(title = 'Principal Component 1'),
           yaxis = list(title = 'Principal Component 2'),
           zaxis = list(title = 'Principal Component 3')
         ),
         margin = list(l = 0, r = 0, b = 0, t = 40))


pca_3d_plot

library(cluster)

final_vars <- c("Medication_Dosage", "Age", "Gender", "Race", "Insurance_type", 
                "Marital_Status", "Admission_Length_of_Stay", "used_warfarin", 
                "used_ppi", "used_other_statins", "used_nsaid", "used_clopidogrel", 
                "used_anticoagulant", "used_ace_arb")

# Subset the data
df_pca <- cohort_data %>%
# Modify or select variables
  select(ADR, all_of(final_vars)) #%>%
# Modify or select variables
#mutate(ADR = ifelse(ADR == "ADR during re-admission", 1, 0))  # Binary encoding for ADR

df_pca_dummy <- dummyVars("~ .", data = df_pca)
# Make predictions using model
df_pca_transformed <- predict(df_pca_dummy, newdata = df_pca) %>% as.data.frame()

df_pca_scaled <- df_pca_transformed %>%
# Modify or select variables
  #select(-ADR) %>%  # Exclude the ADR column for clustering
  scale()  # Standardizing the data

set.seed(123)
kmeans_model <- kmeans(df_pca_scaled, centers = 2, nstart = 25)
df_pca_scaled_with_clusters <- as.data.frame(df_pca_scaled)  # Ensure it's a data frame
df_pca_scaled_with_clusters$cluster <- as.factor(kmeans_model$cluster)
df_pca_scaled_with_clusters$PC1 <- pca_model$x[, 1]  
df_pca_scaled_with_clusters$PC2 <- pca_model$x[, 2]

# Create a plot
ggplot(df_pca_scaled_with_clusters, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "K-Means Clustering: ADR vs Non-ADR",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()


# Generate frequency table
table(Cluster = df_pca_scaled_with_clusters$cluster, ADR = df_pca$ADR)

# Create a plot
library(ggplot2)

# Generate frequency table
cluster_table <- table(Cluster = df_pca_scaled_with_clusters$cluster, ADR = df_pca$ADR)
cluster_table_df <- as.data.frame(cluster_table)

colnames(cluster_table_df) <- c("Cluster", "ADR", "Count")

# Create a plot
ggplot(cluster_table_df, aes(x = Cluster, y = Count, fill = ADR)) +
  geom_bar(stat = "identity", position = "fill") +  # position = "fill" shows proportions
  labs(title = "Proportion of ADR vs Non-ADR within Each K-means Cluster",
       y = "Proportion",
       x = "Cluster",
       fill = "ADR Status") +
  theme_minimal()




library(mclust)

adjustedRandIndex(df_pca_scaled_with_clusters$cluster, df_pca$ADR)
### Try k = 3


set.seed(123)


kmeans_model_3 <- kmeans(df_pca_scaled, centers = 3, nstart = 25)


df_pca_scaled_with_clusters_3 <- as.data.frame(df_pca_scaled)
df_pca_scaled_with_clusters_3$cluster <- as.factor(kmeans_model_3$cluster)

df_pca_scaled_with_clusters_3$PC1 <- pca_model$x[, 1]
df_pca_scaled_with_clusters_3$PC2 <- pca_model$x[, 2]

# Create a plot
ggplot(df_pca_scaled_with_clusters_3, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "K-Means Clustering (K = 3): ADR vs Non-ADR",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()

# Generate frequency table
table(Cluster = df_pca_scaled_with_clusters_3$cluster, ADR = df_pca$ADR)




# Generate frequency table
cluster_table <- table(Cluster = df_pca_scaled_with_clusters_3$cluster, ADR = df_pca$ADR)
cluster_table_df <- as.data.frame(cluster_table)


colnames(cluster_table_df) <- c("Cluster", "ADR", "Count")

# Create a plot
ggplot(cluster_table_df, aes(x = Cluster, y = Count, fill = ADR)) +
  geom_bar(stat = "identity", position = "fill") +  # position = "fill" shows proportions
  labs(title = "Proportion of ADR vs Non-ADR within Each K-means Cluster",
       y = "Proportion",
       x = "Cluster",
       fill = "ADR Status") +
  theme_minimal()

adjustedRandIndex(df_pca_scaled_with_clusters_3$cluster, df_pca$ADR)
## Elbow plot

wss <- vector()

# Try K = 1 to 10
for (k in 1:15) {
  kmeans_result <- kmeans(df_pca_scaled, centers = k, nstart = 25)
  wss[k] <- kmeans_result$tot.withinss
}

# Make elbow plot
elbow_df <- data.frame(Clusters = 1:15, WSS = wss)

# Create a plot
ggplot(elbow_df, aes(x = Clusters, y = WSS)) +
  geom_point() +
  geom_line() +
  labs(title = "Elbow Method for Finding Optimal K",
       x = "Number of Clusters (K)",
       y = "Total Within-Cluster Sum of Squares (WSS)") +
  theme_minimal()


### Try k = 10

set.seed(123)

kmeans_model_10 <- kmeans(df_pca_scaled, centers = 10, nstart = 25)

df_pca_scaled_with_clusters_10 <- as.data.frame(df_pca_scaled)
df_pca_scaled_with_clusters_10$cluster <- as.factor(kmeans_model_10$cluster)

df_pca_scaled_with_clusters_10$PC1 <- pca_model$x[, 1]
df_pca_scaled_with_clusters_10$PC2 <- pca_model$x[, 2]


# Create a plot
ggplot(df_pca_scaled_with_clusters_10, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "K-Means Clustering (K = 10): ADR vs Non-ADR",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Cluster") +
  theme_minimal()

# Generate frequency table
table(Cluster = df_pca_scaled_with_clusters_10$cluster, ADR = df_pca$ADR)

# Generate frequency table
cluster_table <- table(Cluster = df_pca_scaled_with_clusters_10$cluster, ADR = df_pca$ADR)
cluster_table_df <- as.data.frame(cluster_table)


colnames(cluster_table_df) <- c("Cluster", "ADR", "Count")

# Create a plot
ggplot(cluster_table_df, aes(x = Cluster, y = Count, fill = ADR)) +
  geom_bar(stat = "identity", position = "fill") +  # position = "fill" shows proportions
  labs(title = "Proportion of ADR vs Non-ADR within Each K-means Cluster",
       y = "Proportion",
       x = "Cluster",
       fill = "ADR Status") +
  theme_minimal()

adjustedRandIndex(df_pca_scaled_with_clusters_10$cluster, df_pca$ADR)

### the hierarchical clusteirng

library(tidyverse)

dist_matrix <- dist(df_pca_scaled, method = "euclidean")  # or method = "manhattan" if you prefer

hclust_model <- hclust(dist_matrix, method = "ward.D2")  # ward.D2 tends to form compact clusters

plot(hclust_model, labels = FALSE, hang = -1, main = "Hierarchical Clustering Dendrogram")

cluster_cut <- cutree(hclust_model, k = 10)  # set k = 10

df_pca_scaled_with_hclust <- as.data.frame(df_pca_scaled)
df_pca_scaled_with_hclust$hclust_cluster <- as.factor(cluster_cut)

df_pca_scaled_with_hclust$PC1 <- pca_model$x[, 1]
df_pca_scaled_with_hclust$PC2 <- pca_model$x[, 2]

# Create a plot
ggplot(df_pca_scaled_with_hclust, aes(x = PC1, y = PC2, color = hclust_cluster)) +
  geom_point(alpha = 0.6) +
  labs(title = "Hierarchical Clustering: 10 Clusters in PCA Space",
       x = "Principal Component 1",
       y = "Principal Component 2",
       color = "Hierarchical Cluster") +
  theme_minimal()


# Generate frequency table
table(Cluster = df_pca_scaled_with_hclust$hclust_cluster, ADR = df_pca$ADR)


# Generate frequency table
hclust_table <- table(Cluster = df_pca_scaled_with_hclust$hclust_cluster, ADR = df_pca$ADR)
hclust_table_df <- as.data.frame(hclust_table)

print(hclust_table_df)

# Create a plot
library(ggplot2)

custom_colors <- c("1" = "#E74C3C",  # bright red
                   "0" = "#3498DB")      

# Create a plot
ggplot(hclust_table_df, aes(x = as.factor(Cluster), y = Freq, fill = ADR)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(values = custom_colors) +
  labs(title = "Proportion of ADR vs Non-ADR within Each Hierarchical Cluster",
       x = "Hierarchical Cluster",
       y = "Proportion",
       fill = "ADR Status") +
  theme_minimal()





