# ---- Data Preparation: Task 1 ---- 
# You can retrieve this data set in R using the following code. Here we assume that you have saved the file “BreastCancer.csv” in your working directory:
#  • Question 1: What is the function to use for reading the file?
setwd("~/Desktop/2 semester /molekylær data science 2 BMB547") 
BreastCancer <- read.csv("BreastCancer.csv")
View(BreastCancer)

BreastCancer_narm <- BreastCancer[,2:32]

#  • Question 2: How many samples are in the data set? How many different tumor features are in

# Number of samples
samples <- dim(BreastCancer_narm)[1]
samples
# There are 569 samples

# Number of different tumor features
features <- dim(BreastCancer_narm)[2]
features
# There are 32 features

#the data set?
#  • Question 3: Which feature do you consider to be the one that should be predicted? And why?
# Se docs fil

#  • Question 4: What is the class distribution of the data set? How many samples are benign and how many are malignant?
BreastCancer_narm_diagnosis <- as.factor(BreastCancer_narm$diagnosis)
BreastCancer_narm_diagnosis

class_dis <- table(BreastCancer_narm$diagnosis)
class_dis

# Der er 357 bening og 212 malignant

# ---- Data exploration: Task 2 ----

#  • Question 5: Which feature has the highest mean value? Which feature has the lowest mean value?
means <- colMeans(BreastCancer_narm[, sapply(BreastCancer_narm, is.numeric)])
sds <- apply(BreastCancer_narm[, sapply(BreastCancer_narm, is.numeric)], 2, sd)
HM_feature <- names(means)[which.max(means)]
LM_feature <- names(means)[which.min(means)]
HM_feature
LM_feature

#  • Question 6: Which feature has the highest standard deviation? Which feature has the lowest standard deviation?
HSD_feature <- names(sds)[which.max(sds)]
LSD_feature <- names(sds)[which.min(sds)]
HSD_feature
LSD_feature
# print alle 

print(paste("Feature with highest mean value:", HM_feature))
# Feature with highest mean value: id
mean(BreastCancer_narm$area_worst)
# mean of area_worst: 880.5831

print(paste("Feature with lowest mean value:", LM_feature))
# Feature with lowest mean value: fractal_dimension_se
mean(BreastCancer_narm$fractal_dimension_se)
# mean of fractal_dimension_se: 0.003794904

print(paste("Feature with highest standard deviation:", HSD_feature))
# Feature with highest standard deviation: id
sd(BreastCancer_narm$area_worst)
# sd for area_worst: 569.357

print(paste("Feature with lowest standard deviation:", LSD_feature))
# Feature with lowest standard deviation: fractal_dimension_se
sd(BreastCancer_narm$fractal_dimension_se)
# mean for fractal_dimension_se: 0.002646071

#  • Question 7: How would one make these differences more equivalent to avoid that some features become more 
#                influential than others?
a <- c(1,2,3,4)
b <- c(100, 200, 300, 400)
(a-mean(a))/sd(a)
(b-mean(b))/sd(b)


# Se docs
# Dette ikke korrekt
standard_data <- scale(BreastCancer_narm[, sapply(BreastCancer_narm, is.numeric)])
# Beregn middelverdier for standard data
means_standard <- colMeans(standard_data)

# Beregning av standardavvik for standardiserte data
sds_standard <- apply(standard_data, 2, sd)

# Find navnene på egenskabene med højste og lavste standard deviation
highest_sd_feature <- names(sds_standard)[which.max(sds_standard)]
lowest_sd_feature <- names(sds_standard)[which.min(sds_standard)]

# Print resultaterne
cat("Feature with the highest standard deviation:", highest_sd_feature, "\n")
cat("Feature with the lowest standard deviation:", lowest_sd_feature, "\n")




# ---- Task 3 ----
# Question 8: What is the Pearson correlation? What is the range of values?
# se docs

# Question 9: Which feature pair is most correlated? Which feature pair the least?
BreastCancer_num <- BreastCancer[,3:32]
cor_BreastCancer <- cor(BreastCancer_num, method = c("pearson"))
library(GGally)
barplot(cor_BreastCancer)
ggcorr(cor_BreastCancer)

cor_BreastCancer == 1
cor_BreastCancer[cor_BreastCancer == 1] <- NA

min(cor_BreastCancer, na.rm = TRUE)
max(cor_BreastCancer, na.rm = TRUE)

abs_BreastCancer <- abs(cor_BreastCancer)
min(abs_BreastCancer, na.rm = TRUE)
Ind2 <- which(abs_BreastCancer == min(abs_BreastCancer, na.rm = TRUE), arr.ind = TRUE)


max(abs_BreastCancer, na.rm = TRUE)
Ind1 <- which(abs_BreastCancer == max(abs_BreastCancer, na.rm = TRUE), arr.ind = TRUE)


# Question 10: When you get a negative value for the correlation, what does that mean?
# se docs


# ---- Task 4 ----
# Question 11: Are these features potentially valuable indicators for the diagnosis of a tumor?
plot(BreastCancer_num[,1],
     BreastCancer_num[,2],
     col = as.factor(BreastCancer_narm$diagnosis),
     xlab = "Radius Mean",
     ylab = "Texture Mean",
     main = "First two features")

# Question 12: How similar are the first two features? 
# How does the figure change when you plot the most and least correlated features?
plot(BreastCancer_num[,Ind1[1]],
     BreastCancer_num[,Ind1[2]],
     col = as.factor(BreastCancer_narm$diagnosis),
     main = "Most correlated features")

plot(BreastCancer_num[,Ind2[1]],
     BreastCancer_num[,Ind2[2]],
     col = as.factor(BreastCancer_narm$diagnosis),
     main = "Least correlated features")

# Question 13: Which features are potentially more useful for classification? 
# Highly correlated features or features that are not correlated?

# ---- Task 5 ---- 
BC_matrix <- as.matrix(BreastCancer_num)
heatmap(BC_matrix, 
        Rowv = NA, 
        Colv = NA, 
        scale = "column",
        margins = c(5,10),
        RowSideColors = rainbow(2)[as.factor(BreastCancer$diagnosis)],
        main = "Heatmap of Breast Cancer dataset", 
        xlab = "Features",
        ylab = "Samples")
legend("bottomright", 
       fill = rainbow(2),
       legend = c("malignent", "benign"))

# Question 14: Which part of the figure tells which tumor diagnosis is more likely? 
# Now, change the arguments Rowv and Colv to TRUE and rerun the code.
heatmap(BC_matrix, 
        Rowv = TRUE, 
        Colv = TRUE, 
        scale = "column",
        margins = c(5,10),
        RowSideColors = rainbow(2)[as.factor(BreastCancer$diagnosis)],
        main = "Heatmap of Breast Cancer dataset", 
        xlab = "Features",
        ylab = "Samples")
legend("bottomright", 
       fill = rainbow(2),
       legend = c("malignent", "benign"))

is.numeric(BC_matrix)
sapply(BC_matrix, is.numeric)

# Question 15: What is the difference between the two figures?

# Question 16: When ordering samples and features according to their similarity, 
# do you get a better separation of the two classes? Why would that be?



# ---- Task 6 ----

# udregner the principla componets 
my_pca <- prcomp(BreastCancer_num,
                 center=TRUE, 
                 scale=TRUE)

  # Sub-task 1: Inspect the return element my_pca. Familiarize yourself with 
  # the structure of the return Element. You can also read the documentation by calling ?prcomp.

my_pca

  # Sub-task 2: Visualize the dataset when plotted onto the first two principal components.
  # Color according to the tumor type. You can use ggplot2 for this, or alternatively 
  # any other means of plotting. Hint: the rotated data is contained in the return 
  # object of prcomp in the variable my_pca$x.

# laver det til en data frame 

pca_df <- as.data.frame(my_pca$x)

pca_cdf <- cbind(pca_df,BreastCancer[2])

#laver så ggplot over de to og definere at det skal tælle PC værdierne

ggplot(pca_cdf, aes(x=PC1, y=PC2,
                    color = diagnosis)) + geom_point() + scale_color_manual(values = c("blue", "red"),
                                                                            labels = c("B", "M"),
                                                                            name = "Diagnois")
# ---- Task 7 ----

#lav logastik på plottet 
## Question 21: What do you observe when running the logisitc regression? Are there errors or warnings?

BC <- read.csv("BreastCancer.csv")
BC$diagnosis <- as.factor(BC$diagnosis)
#fortolk som enten eller
BC_train <- BC[,2:32]

glm_BC <- glm(diagnosis ~ ., data = BC_train, family = binomial)
summary(glm_BC)

# Problemet at man får frem at alt er signifikant, hvilker det ikke er.
#Error fordi vi får noget der er voldsomt korreleret features.

## Question 22: Which features are significant (assuming a significance level of <0.05)?

library(tidyverse)
library(caret)
library(lattice)

BC <- read.csv("BreastCancer.csv")
num_BC <- BC[,3:32]
BC$diagnosis <- as.factor(BC$diagnosis)

# The plus two is needed since the first two columns were non-numerical
features_to_remove = findCorrelation(cor(num_BC), cutoff = 0.9) + 2
BC_train = BC[,-features_to_remove]

#Discard the ID and X column
BC_train = BC_train[,-c(1,23)]
glm_BC <- glm(diagnosis ~ ., data = BC_train, family = binomial)
summary(glm_BC)

# opgave 25 

# ---- Task 8 ----

  # We have done something one is not suppsoed to do: We have used all our data for 
  # training and evaluation. Let’s see how well we fare with cross-validation. Therefore, 
  # we perform a 10-fold cross- varliation. Luckily, there is a package which does that 
  # for us automatically:

library(caret)
train_control <- trainControl(method = "cv", 
                              number = 10)
model <- train(diagnosis ~ ., 
               data = BreastCancer_train,
               method = "glm",
               trControl = train_control)
print(model) 
# Perform the cross-validation using the 10ncode from above. 
# Answer the following questions: 
#  • Question 26: What is the average accuracy you achieve?
# The average accuracy is 0.9684491
# Stoler mere på denne model

#  • Question 27: Compare the accuracy from the task before. 
#                 Are the differences expected?
# The average accuracy is 0.9877 

# ---- Task IX ----
# As last step, we will run a classification with the commonly more powerful 
# random forest algorithm. Since a Random Forest is less prone to overfitting, 
# we can use the entire dataset for training. We will use the OOB error estimate 
# as well as the whole training data to evaluate our performance.
#  • Question 28: What performance does the randomForest achieve as OOB error rate?
library(randomForest)
rf_BC <- randomForest(diagnosis ~ .,
                      data = BreastCancer_train,
                      importance = TRUE)
predicted <- predict(rf_BC, BreastCancer_train, type = "response")
confusionMatrix(BreastCancer_train$diagnosis, predicted)

#  • Question 29: How well does it perform on the entire dataset? 
#                 Can you argue why the OOB is worse then the overall classification?


#  • Question 30: Plot and compare the most important features. 
#                 Compare with the results for the linear regression.
library(ggplot2)

importance(rf_BC)
varImpPlot(rf_BC)

# Extract feature importance from the random forest model
importance <- rf_BC$importance

# Convert importance to a data frame for easier manipulation
importance_df <- as.data.frame(importance)

# Order the features by importance
importance_df <- importance_df[order(importance_df$MeanDecreaseGini, decreasing = TRUE), ]

# Select the top N features
top_features <- importance_df[1:10, ]

# Plot the top features
barplot(top_features$MeanDecreaseGini, 
        names.arg = rownames(top_features),
        main = "Top 10 Features by Importance",
        xlab = "Feature",
        ylab = "Mean Decrease in Gini",
        col = "skyblue",
        ylim = c(0, max(top_features$MeanDecreaseGini) * 1.1))

# Compare with linear regression results
summary(glm_BC)