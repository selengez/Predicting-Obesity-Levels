###packages and libraries ###
install.packages("ggplot2")
install.packages("readr")
install.packages("lattice")
install.packages("stats")
install.packages("nnet")  
install.packages("kableExtra")
install.packages("caret")
install.packages("lightgbm")

library(lightgbm)
library(readr)
library(dplyr) 
library(factoextra)  
library(ggplot2)     
library(scales)
library(caret)
library(tidyverse)
library(knitr)
library(kableExtra)
library(nnet) 
library(corrplot) 
library(gplots)
library(class)
library(cluster)
library(rpart)

require(MASS)
#####################
##Reading the data
#####################


data<- read_csv("Obesity.csv")

#####################
##Checking data
#####################


head(data)
tail(data)
summary(data)
str(data)
dim(data)




description <- matrix(NA, nrow = ncol(data), ncol = 2)
colnames(description) <- c("Variable", "Data Type")


for (i in 1:ncol(data)) {
  description[i, "Variable"] <- names(data)[i]
  description[i, "Data Type"] <- class(data[[i]])
  
}

print(description)

#########################################################################################################
#######################################################################################################################
##################### Data Visualizations##########################################
####################################################################################

#####################
#Data Explatory Analysis
#####################

#| Abbreviation   | Full Form                                 |
# |:---------------|:------------------------------------------|
#| FAVC           | Frequent consumption of high caloric food |
#| FCVC           | Frequency of consumption of vegetables    |
#| NCP            | Number of main meals                      |
#| CAEC           | Consumption of food between meals         |
#| CH20           | Consumption of water daily                |
#| CALC           | Consumption of alcohol                    |
#| SCC            | Calories consumption monitoring           |
#| FAF            | Physical activity frequency               |
#| TUE            | Time using technology devices             |
#| MTRANS         | Transportation used   #*  

### pie charts

NObeyesdad_pie<-pie(table(data$NObeyesdad) ,radius = 0.8,col = c("yellowgreen", "violetred1", "salmon",
                                                                 "yellow4", "steelblue1", "lightskyblue1","orange2"), 
                    main = "Pie Chart of Obesity Level")


colors()


## pie chart with percentages 
obesity_table <- table(data$NObeyesdad)
obesity_percentages <- percent(as.vector(obesity_table) / sum(obesity_table))

custom_labels <- paste(names(obesity_table), ": ", obesity_percentages)


NObeyesdad_pie <- pie(obesity_table, radius = 0.8,
                      col = c("yellowgreen", "pink", "salmon",
                              "lightgreen", "steelblue1", "lightskyblue1", "orange2"), 
                      main = "Pie Chart of Obesity Level",
                      labels = custom_labels,
                      cex = 0.8)


###### gender 
gender_table <- table(data$Gender)
gender_percentages <- percent(as.vector(gender_table) / sum(gender_table))

gender_labels <- paste(names(gender_table), ": ", gender_percentages)


gender_pie <- pie(gender_table, radius = 0.8,
                  col = c("pink3",
                          "lightskyblue1"), 
                  main = "Pie Chart of Gender",
                  labels = gender_labels,
                  cex = 0.8)




###### family history 
familyhistory_table <- table(data$family_history_with_overweight)
familyhistory_percentages <- percent(as.vector(familyhistory_table) / sum(familyhistory_table))

familyhistory_labels <- paste(names(familyhistory_table), ": ", familyhistory_percentages)


familyhistory_pie <- pie(familyhistory_table, radius = 0.8,
                         col = c("slateblue2",
                                 "lavenderblush2"), 
                         main = "Pie Chart of Family History",
                         labels = familyhistory_labels,
                         cex = 0.8)

familyhistory_bar <- barplot(familyhistory_table, 
                             col = c("lightblue", "lavenderblush2"),
                             main = "Histogram of Family History",
                             xlab = "Family History",
                             ylab = "Frequency",
                             names.arg = names(familyhistory_table),
                             cex.names = 0.8)



######FAVC
FAVC_table <- table(data$FAVC)
FAVC_percentages <- percent(as.vector(FAVC_table) / sum(FAVC_table))

FAVC_labels <- paste(names(FAVC_table), ": ", FAVC_percentages)


FAVC_pie <- pie(FAVC_table, radius = 0.8,
                col = c("slateblue2",
                        "lavenderblush2"), 
                main = "Pie Chart of Frequent consumption of high caloric food",
                labels = FAVC_labels,
                cex = 0.8)

######CAEC
CAEC_table <- table(data$CAEC)
CAEC_percentages <- percent(as.vector(CAEC_table) / sum(CAEC_table))

CAEC_labels <- paste(names(CAEC_table), ": ", CAEC_percentages)


CAEC_pie <- pie(CAEC_table, radius = 0.8,
                col = c("slateblue2",
                        "lavenderblush2"), 
                main = "Pie Chart of Consumption of food between meals ",
                labels = CAEC_labels,
                cex = 0.8)

######CALC
CALC_table <- table(data$CALC)
CALC_percentages <- percent(as.vector(CALC_table) / sum(CALC_table))

CALC_labels <- paste(names(CALC_table), ": ", CALC_percentages)


CALC_pie <- pie(CALC_table, radius = 0.8,
                col = c("slateblue2",
                        "lavenderblush2"), 
                main = "Pie Chart of Consumption of alcohol",
                labels = CALC_labels,
                cex = 0.8)


######MTRANS
MTRANS_table <- table(data$MTRANS)
MTRANS_percentages <- percent(as.vector(MTRANS_table) / sum(MTRANS_table))

MTRANS_labels <- paste(names(MTRANS_table), ": ", MTRANS_percentages)


MTRANS_pie <- pie(MTRANS_table, radius = 0.8,
                  col = c("slateblue2",
                          "lavenderblush2"), 
                  main = "Pie Chart of Transportation used",
                  labels = MTRANS_labels,
                  cex = 0.9)

######Skewness graph

###Age
age_column <- data$Age
age_column

hist(age_column, main = "Distribution of Age",
     xlab = "Age", ylab = "Density", col = "skyblue", border = "white", freq = FALSE)


curve(dnorm(x, mean = mean(age_column), sd = sd(age_column)), add = TRUE, col = "red", lwd = 2)

#  legend
legend("topright", legend = c("Age Distribution", "Normal Curve"),
       col = c("skyblue", "red"), lty = 1, lwd = 2)

###Height
Height_column <- data$Height
mean(Height_column)



hist(age_column, main = "Distribution of Height",
     xlab = "Height", ylab = "Density", col = "skyblue", border = "white", freq = FALSE)

curve(dnorm(x, mean = mean(Height_column), sd = sd(Height_column)), add = TRUE, col = "red", lwd = 2)

legend("topright", legend = c("Height Distribution", "Normal Curve"),
       col = c("skyblue", "red"), lty = 1, lwd = 2)





# Group by Gender and calculate summary statistics
data %>%
  group_by(Gender) %>%
  summarise(across(c(Age, Height, Weight), list(mean = ~mean(.), 
                                                median = ~median(.), 
                                                min = ~min(.), 
                                                max = ~max(.), 
                                                sd = ~sd(.)))) %>%
  mutate_if(is.numeric, round, 2) -> summary_stats


#  heatmap of summary statistics
summary_stats %>%
  pivot_longer(-Gender, names_to = "Metrics", values_to = "Value") %>%
  ggplot(aes(Gender, Metrics, fill = Value)) + 
  geom_tile() + 
  scale_fill_gradient(low = "lightblue", high = "blue") + 
  theme_minimal()

#scatter plot of Weight vs Height, colored by Gender and NObeyesdad
ggplot(data, aes(x = Weight, y = Height, color = NObeyesdad)) + 
  geom_point(size = 3) + 
  facet_wrap(~ Gender, scales = "free") + 
  theme_minimal()

data %>%
  count(Gender, NObeyesdad) %>%
  arrange(desc(n)) %>%
  mutate_if(is.numeric, round, 2) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped")


######

ggplot(data = data, aes(x = Weight, y = Height, color = Gender)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, size = 1) +
  scale_color_manual(values = c("Female" = "#c90076", "Male" = "#2986cc")) +
  labs(title = "Height and Weight VS Gender", 
       x = "Weight", 
       y = "Height",
       color = "Gender") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, color = "#0a2e4f"))


###########


# Boxplot with different colors for each category
ggplot(data, aes(x = Age, y = NObeyesdad, fill = NObeyesdad)) +
  geom_boxplot(color = "black") +
  geom_point(stat = "summary", fun = mean, shape = 15, size = 2, color = "white") +
  scale_fill_manual(values = c("Insufficient_Weight" = "blue", "Overweight_Level_I" = "lightgreen", "Overweight_Level_II" = "orange", 
                               "Obesity_Type_I" = "purple", "Normal_Weight" = "red", 
                               "Obesity_Type_III" = "brown", "Obesity_Type_II" = "yellow")) +
  theme_minimal() +
  labs(x = "Age", y = "NObeyesdad") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  coord_cartesian(ylim = c(0, 6))  

age_range_target <- data %>%
  group_by(NObeyesdad) %>%
  summarise(Age_min = min(Age), Age_max = max(Age)) %>%
  mutate(Range = paste(Age_min, "To", Age_max),
         Color = case_when(
           NObeyesdad == "Insufficient_Weight" ~ "blue",
           NObeyesdad == "Normal_Weight"  ~ "red",
           NObeyesdad == "Overweight_Level_I" ~ "lightgreen",
           NObeyesdad == "Overweight_Level_II" ~ "orange",
           NObeyesdad == "Obesity_Type_I" ~ "purple",
           NObeyesdad == "Obesity_Type_II" ~ "yellow",
           NObeyesdad == "Obesity_Type_III" ~ "brown",
           TRUE ~ "black" # Default color if new categories added
         ))


s<-unique(data$NObeyesdad)

print(age_range_target)

colors()

#########################################################################################################
#######################################################################################################################
##################### Data Pre-Processing ##########################################
####################################################################################



#####################
#Checking missing values
#####################


colSums(is.na(data))


#####################
# select categorical columns
#####################

get_categorical_columns <- function(data) {
  categorical_columns <- sapply(data, function(col) is.factor(col) | is.character(col))
  return(names(data)[categorical_columns])
}

categorical_cols <- get_categorical_columns(data)

print(categorical_cols)

##"Gender","family_history_with_overweight","FAVC","CAEC","SMOKE","SCC","CALC","MTRANS","NObeyesdad"



#####################
#data encoding#####


levels <- c("Insufficient_Weight", "Normal_Weight", "Overweight_Level_I", "Overweight_Level_II", 
            "Obesity_Type_I", "Obesity_Type_II", "Obesity_Type_III")
numerical_values <- 1:7
data$NObeyesdad <- factor(data$NObeyesdad, levels = levels, labels = numerical_values)

data$Gender <- as.factor(data$Gender)
levels2 <- c("Female","Male" )
numerical_values <- 1:2
data$Gender <- factor(data$Gender, levels = levels2, labels = numerical_values)

data$family_history_with_overweight <- as.factor(data$family_history_with_overweight)
levels3 <- c("no","yes" )
numerical_values <- 0:1
data$family_history_with_overweight <- factor(data$family_history_with_overweight, levels = levels3, labels = numerical_values)

data$FAVC <- as.factor(data$FAVC)
levels3 <- c("no","yes" )
numerical_values <- 0:1
data$FAVC <- factor(data$FAVC, levels = levels3, labels = numerical_values)

data$SMOKE <- as.factor(data$SMOKE)
levels3 <- c("no","yes" )
numerical_values <- 0:1
data$SMOKE <- factor(data$SMOKE, levels = levels3, labels = numerical_values)

data$SCC <- as.factor(data$SCC)
levels3 <- c("no","yes" )
numerical_values <- 0:1
data$SCC <- factor(data$SCC, levels = levels3, labels = numerical_values)


data$CAEC <- as.factor(data$CAEC)
levels4 <- c("no","Sometimes","Frequently","Always")
numerical_values <- 1:4
data$CAEC <- factor(data$CAEC, levels = levels4, labels = numerical_values)


data$CALC <- as.factor(data$CALC)
levels4<- c("no","Sometimes","Frequently","Always")
numerical_values <- 1:4
data$CALC <- factor(data$CALC, levels = levels4, labels = numerical_values)

data$MTRANS <- as.factor(data$MTRANS)
levels5<- c("Public_Transportation","Walking","Automobile","Motorbike","Bike" )
numerical_values <- 1:5
data$MTRANS <- factor(data$MTRANS, levels = levels5, labels = numerical_values)


#####################
###  transform all columns as numeric
#####################


for(col_name in names(data)) {
  if(class(data[[col_name]]) != "num") {
    data[[col_name]] <- as.numeric(factor(data[[col_name]]))
  }
}


######## "YeoJohnson" encoding

encoder <- preProcess(data, method = "YeoJohnson")

for(col_name in names(data)) {
  if(class(data[[col_name]]) == "character") {
    data[[col_name]] <- as.numeric(factor(data[[col_name]]))
  }
}
str(data)



#####################
####data scaling
#####################

scaled_data <- scale(data)
print(scaled_data)



#####################
##  correlation  and correlation matrix
#####################
corr <- cor(data)


 
corrplot(corr, method="color", type="upper", order="hclust", 
         tl.col="black", tl.srt=45, diag=FALSE)




corr_matrix <- cor(data)
corr_matrix

heatmap.2(corr_matrix,
          trace = "none",                  
          col = colorRampPalette(c("blue", "white", "red"))(100),  
          dendrogram = "none",             
          margins = c(5, 5),             
          main = "Correlation Heatmap",    
          key = TRUE,                      # Include legend
          keysize = 1.0,                   # Size of legend
          symkey = TRUE,                   # Show symmetric key
          density.info = "none",           # Do not include density plot
          notecol = "black",               # Color of the correlation values
          cexRow = 1.0,                    # Row text size
          cexCol = 1.0,                    # Column text size
          cellnote = round(corr_matrix, 2), 
)

nobe_correlation <- corr_matrix["NObeyesdad", ]
sorted_nobe_correlation <- sort(nobe_correlation, decreasing = TRUE)

print(sorted_nobe_correlation)


##############################################################################
#############################################################################
##################### Supervised Learning ###################################
#############################################################################


#####Logistic Regression######
###################
set.seed(42)


X <- data[, !(names(data) %in% "NObeyesdad")]
y <- data$NObeyesdad

y
y <- as.factor(y)


train_index <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_index, ]
y_train <- y[train_index]
X_test <- X[-train_index, ]
y_test <- y[-train_index]


model <- multinom(NObeyesdad ~ ., data = data)
summary(model)


fit <- model

predictions <- predict(fit, newdata = X_test, type = "class")
predictions


accuracy_lr <- mean(predictions == y_test)
accuracy_lr


conf_mat_lr <- table(predictions, y_test)
precision_lr <- diag(conf_mat_lr) / rowSums(conf_mat_lr)
recall_lr <- diag(conf_mat_lr) / colSums(conf_mat_lr)
f1_score_lr <- 2 * precision_lr * recall_lr / (precision_lr + recall_lr)

cat("Accuracy:", accuracy_lr, "\n")
cat("Precision:", mean(precision_lr, na.rm = TRUE), "\n")
cat("Recall:", mean(recall_lr, na.rm = TRUE), "\n")
cat("F1 Score:", mean(f1_score_lr, na.rm = TRUE), "\n")


# visualization confusion matrix
hm <- as.data.frame(as.table(conf_mat_lr))

ggplot(hm, aes(x = predictions, y = y_test, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "pink", size = 5) +
  
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))




#### KNN method


k <- 5  
knn_model <- knn(train = X_train, test = X_test, cl = y_train, k = k)

confusionMatrix(table(knn_model, y_test))


conf_matrix <- table(knn_model, y_test)


accuracy_knn <- sum(diag(conf_matrix)) / sum(conf_matrix)
precision_knn <- diag(conf_matrix) / rowSums(conf_matrix)
recall_knn <- diag(conf_matrix) / colSums(conf_matrix)

f1_score_knn <- 2 * precision_knn * recall_knn / (precision_knn + recall_knn)


cat("Accuracy:", accuracy_knn, "\n")
cat("Precision:", mean(precision_knn, na.rm = TRUE), "\n")
cat("Recall:", mean(recall_knn, na.rm = TRUE), "\n")
cat("F1 Score:", mean(f1_score_knn, na.rm = TRUE), "\n")


hm_knn <- as.data.frame(as.table(conf_matrix))

ggplot(hm_knn, aes(x = knn_model, y = y_test, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Pinks", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "purple", size = 5) +
  
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))


##### LGBM



model_lgbm <- lgb.train(data = lgb.Dataset(as.matrix(X_train), label = as.integer(y_train) - 1),
                        params = list(objective = "multiclass",
                                      num_class = length(levels(y_train))),
                        nrounds = 100)


predictions <- predict(model_lgbm, newdata = as.matrix(X_test))


predictions <- apply(predictions, 1, which.max)


accuracy_lgbm <- mean(predictions == y_test)


confusionMatrix(predictions, y_test)


conf_matrix_lgbm <- table(predictions, y_test)


accuracy_lgbm <- sum(diag(conf_matrix_lgbm)) / sum(conf_matrix_lgbm)
precision_lgbm <- diag(conf_matrix_lgbm) / rowSums(conf_matrix_lgbm)
recall_lgbm <- diag(conf_matrix_lgbm) / colSums(conf_matrix_lgbm)

f1_score_lgbm <- 2 * precision_lgbm * recall_lgbm / (precision_lgbm + recall_lgbm)


cat("Accuracy:", accuracy_lgbm, "\n")

cat("Precision:", mean(precision_lgbm, na.rm = TRUE), "\n")
cat("Recall:", mean(recall_lgbm, na.rm = TRUE), "\n")
cat("F1 Score:", mean(f1_score_lgbm, na.rm = TRUE), "\n")


hm_knn <- as.data.frame(as.table(conf_matrix_lgbm))

ggplot(hm_knn, aes(x = predictions, y = y_test, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "pink", size = 5) +
  
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))





#####.Decision Tree



model_tree <- rpart(y_train ~ ., data = data.frame(cbind(y_train, X_train)))


predictions <- predict(model_tree, newdata = data.frame(X_test), type = "class")


accuracy_tree <- mean(predictions == y_test)


cat("Accuracy:", accuracy_tree, "\n")



confusionMatrix(predictions, y_test)


conf_matrix_tree<- table(predictions, y_test)


accuracy_tree <- sum(diag(conf_matrix_tree)) / sum(conf_matrix_tree)
precision_tree <- diag(conf_matrix_tree) / rowSums(conf_matrix_tree)
recall_tree <- diag(conf_matrix_tree) / colSums(conf_matrix_tree)

f1_score_tree <- 2 * precision_tree * recall_tree / (precision_tree + recall_tree)


cat("Accuracy:", accuracy_tree, "\n")

cat("Precision:", mean(precision_tree, na.rm = TRUE), "\n")
cat("Recall:", mean(recall_tree, na.rm = TRUE), "\n")
cat("F1 Score:", mean(f1_score_tree, na.rm = TRUE), "\n")


hm_knn <- as.data.frame(as.table(conf_matrix_tree))

ggplot(hm_knn, aes(x = predictions, y = y_test, fill = Freq)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette = "Pinks", direction = 1) +
  guides(fill = FALSE) +
  geom_text(aes(label = Freq), color = "purple", size = 5) +
  
  theme(axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6),
        axis.title.x = element_text(size = 6),
        axis.title.y = element_text(size = 6))




############################# Linear Discriminant Model




lda.fit=lda(NObeyesdad ~ ., data = data)
lda.fit
plot(lda.fit)
NObeyesdad=subset(data)
lda.pred=predict(lda.fit,NObeyesdad)
class(lda.pred)
data.frame(lda.pred)[1:5,]
table(lda.pred$class,NObeyesdad$NObeyesdad)
lda_mean<- mean(lda.pred$class==NObeyesdad$NObeyesdad)
lda_mean


###################### Comparing Results####



accuracy_results <- c(accuracy_lr, accuracy_knn, accuracy_lgbm, accuracy_tree)

accuracy_results
model_names <- c("Logistic Regression", "K-Nearest Neighbors", "LightGBM", "Decision Tree")


accuracy_df <- data.frame(Model = model_names, Accuracy = accuracy_results)



ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(title = "Comparison of Model Accuracies",
       x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")



ggplot(accuracy_df, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  geom_text(aes(label = round(Accuracy, digits = 4)), vjust = -0.5, size = 3, color = "black") +  # Add labels
  labs(title = "Comparison of Model Accuracies",
       x = "Model", y = "Accuracy") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")


#########################################################################################################
#######################################################################################################################
##################### Unsupervised Learning ##########################################
####################################################################################


#################K-means



features <- data[, !names(data) %in% "NObeyesdad"]
scaled_data <- scale(features)
set.seed(42)
k <- 4


kmeans_result <- kmeans(scaled_data, centers = k)

t
cluster_assignments <- kmeans_result$cluster
cluster_assignments

cluster_centroids <- kmeans_result$centers
cluster_centroids

cluster_sizes <- table(cluster_assignments)
cluster_sizes


cluster_NObeyesdad_table <- table(cluster_assignments, data$NObeyesdad)

cluster_NObeyesdad_table


#####ratios
cluster_ratios <- (cluster_NObeyesdad_table[, 2] / (cluster_NObeyesdad_table[, 1]+cluster_NObeyesdad_table[, 2]))*100
cluster_ratios



#####PCA
pca_result <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_result$x[, 1:2])  

pca_data$cluster <- as.factor(cluster_assignments)


ggplot(pca_data, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("pink", "red", "orange", "lightgreen")) +
  labs(x = "PC1", y = "PC2", color = "Cluster") +
  ggtitle("K-means Clustering") +
  theme_minimal()


# the size of each cluster
cluster_sizes <- table(cluster_assignments)
cluster_sizes

cluster_NObeyesdad_table <- table(cluster_assignments, data$NObeyesdad)
cluster_NObeyesdad_table




#####Elbow


calculate_wcss <- function(data, kmax) {
  wcss <- numeric(kmax)
  for (i in 1:kmax) {
    kmeans_result <- kmeans(data, centers = i)
    wcss[i] <- kmeans_result$tot.withinss
  }
  return(wcss)
}

kmax <- 10


wcss_values <- calculate_wcss(scaled_data, kmax)


plot(1:kmax, wcss_values, type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters (k)", ylab = "Within-cluster sum of squares (WCSS)",
     main = "Elbow Method for Optimal k")
abline(v = 4, lty = 2, col = "red")  
text(4, wcss_values[4], "Potential Elbow Point", pos = 3, col = "red")  






