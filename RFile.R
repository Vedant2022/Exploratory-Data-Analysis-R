#Name - Vedant Thakur
#NTUID - N1288583
#Module - MATH40031: Statistical Data Analysis & Visualization

# For answers to questions scroll down to the end of this file

#---------------------------------------IMPLEMENTATION--------------------------------------#

#TASK 1: a)Visualize the distributions of each variable and provide their
#summary statistics

#Loading Dataset
data <- read.csv("bodyfat.csv", header = TRUE)
data

#Checking for NULL Values
sum(is.na(data))
#No missing values found

#Checking number of variables
#To get number of variables we can simply count number of columns
ncol(data)
#Question: How many variables are in the Dataset?
#Answer: 15

#Type of Variable
str(data)
#Question: What types of variables are they?
#Answer: All variables are num(Numeric) type except Age which is int(Integer)
#type.

#Summary of Dataset
summary(data)
#Question: Is the dataset balanced? Explain.
#Answer: No, the dataset is moderately unbalanced. As seen in the results most
#of the variables are balanced because their mean values and median values are
#very close to each other #(For example: Neck_Mean=37.99 and Neck_Median=38.00).
#While most of the variables are balanced but variables like Weight, Height &
#BodyFat are unbalanced due to extreme values. Hence, I conclude that dataset is
#slightly unbalanced.

#Visualization of Dataset

#Histogram for Variables
hist(data$Density)
hist(data$BodyFat)
hist(data$Age)
hist(data$Weight)
hist(data$Height)
hist(data$Neck)
hist(data$Chest)
hist(data$Abdomen)
hist(data$Hip)
hist(data$Thigh)
hist(data$Knee)
hist(data$Ankle)
hist(data$Biceps)
hist(data$Forearm)
hist(data$Wrist)


#Pair Plot for variables
library(GGally)

variables <- data[,c("Density", "BodyFat", "Age", "Weight", "Height", "Neck",
                     "Chest", "Abdomen", "Hip", "Thigh", "Knee", "Ankle",
                     "Biceps", "Forearm", "Wrist")]

ggpairs(variables, title = "Pair Plot of variables")


#---------------------------------------------------------------------------------#

#TASK 2: Test the assumption that all predictor variables are independent using
#correlation coefficients or tests of associations. Visualize your results using
#scatter plots and a correlation matrix.

data <- read.csv("bodyfat.csv")

#Getting Correlation Matrix
correlation_matrix <- cor(data[, -which(names(data) == "BodyFat")],
                          use = "complete.obs")
print(correlation_matrix)

#Visualizing the Correlation Matrix
library(ggcorrplot)
ggcorrplot(correlation_matrix, method = "square", type = "full", lab = TRUE)

#Visualizing Scatter Plots
library(ggplot2)

#Scatter Plot of Highly Correlated variables of dataset i.e. Weight and Hip
ggplot(data, aes(x = Weight, y = Hip)) + geom_point() +
  geom_smooth(method = "lm", col = "green") +
  labs(title = "Scatter plot of Weight vs Hip", x = "Weight", y = "Hip")

#Scatter Plot of Less Correlated variables of dataset i.e. Age and Weight
ggplot(data, aes(x = Age, y = Weight)) + geom_point() +
  geom_smooth(method = "lm", col = "red") +
  labs(title = "Scatter plot of Age vs Weight", x = "Age", y = "Weight")

#---------------------------------------------------------------------------------#

#TASK 3: c)	Using linear regression modelling, describe the variables which you
# believe have influence on body fat and describe your model with coefficients.
# Note, you may consider other models beyond taught material which may be more
# appropriate for modelling the outcome variable.


#Importing important libraries
library(tidyverse)
library(car)

data <- read.csv("bodyfat.csv")

#Linear Regression Model
#As seen from the correlation matrix I can see that Abdomen and Chest are
#the variables that are dominantly effecting the Density variable inversely with
#percentage over 79% and 68% respectively.

LinearRegression_model <- lm(Density ~ Abdomen + Chest, data = data)
summary(LinearRegression_model)

#Surprisingly, from the summary of Linear Regression Model I see that Abdomen is
#negatively affecting(as Abdomen increases Density decreases)
#Density (p-value < 2e-16) which states Abdomen variable is highly and
#negatively significant to Density variable while Chest is less and slightly
#positively affecting(as Chest increases Density slightly increase)
#Density variable (p-value = 0.0012). This is because Abdomen and Chest are
#correlated t0 each other with 91% approximately.Here comes picture of
#multicollinearity.

#Checking Multicollinearity with VIF(Variance Inflation Factor)

vif(LinearRegression_model)

#As VIF value for both variables is greater than 5, it is clear these two
#variables are affecting my model.

#Other Models to handle Multicollinearity

#Random Forest Model
install.packages("randomForest")
library(randomForest)

RandomForest_model <- randomForest(Density ~ Abdomen + Chest, data = data,
                                   ntree = 500)
plot(RandomForest_model)

#After seeing the results of Random Forest I can see that initially the error
#rate was higher but as the number of trees increases the error rate decreases
#with a high speed. This states my Random Forest Model is well-trained.

#Decision Tree Model
install.packages("rpart","rpart.plot")
library(rpart)
library(rpart.plot)

DecisionTree_model <- rpart(Density ~ Abdomen + Chest, data = data,
                            method = "anova")

rpart.plot(DecisionTree_model, tweak = 1.2)

#From the Decision Tree I can conclude that, clearly Abdomen is the strongest
#predictor variable of Density. While on the other hand Chest has a minor role
#as a predictor.

#R-Squared Calculation of Models

#R-Squared for Linear Regression
value_lm <- predict(LinearRegression_model, data)
rsquared_lm <- 1 - sum((data$Density - value_lm)^2)/sum((data$Density -
                                                          mean(data$Density))^2)

#R-Squared for Random Forest
value_rf <- predict(RandomForest_model, data)
rsquared_rf <- 1 - sum((data$Density - value_rf)^2)/sum((data$Density -
                                                          mean(data$Density))^2)

#R-Squared for Decision Tree
value_dt <- predict(DecisionTree_model, data)
rsquared_rf <- 1 - sum((data$Density - value_dt)^2)/sum((data$Density -
                                                          mean(data$Density))^2)

print(paste("R-Squared for Linear Regression: ",round(rsquared_lm,2)))
print(paste("R-Squared for Random Forest: ",round(rsquared_rf,2)))
print(paste("R-Squared for Decission Tree: ",round(rsquared_rf,2)))

#After calculating R-Squared Values I can see that Random Forest and Decision
#Tree models are clearly better than Linear Regression as they explains more
#variance in the target variable i.e. Density.


#Let's see out of Random Forest and Decision Tree which model is better

#Residual Plots of the models

#Residual Plot for Random Forest
plot(value_rf, data$Density - value_rf,
     main = "Residual Plot of Random Forest", xlab = "Predicted Values",
     ylab = "Residuals", pch = 16)
abline(h=0,col="green",lwd=2)

#Residual Plot for Decision Tree
plot(value_dt, data$Density - value_dt,
     main = "Residual Plot of Decision Tree", xlab = "Predicted Values",
     ylab = "Residuals", pch = 16)
abline(h=0,col="blue",lwd=2)

#From the plots I can conclude that Random Forest is the best model(despite
#having same R-Squared values) for the provided dataset as its residuals are
#more evenly distributed around zero and without forming any pattern. While
#Decision Tree residuals are forming patter and not distributed around zero.


#---------------------------------------------------------------------------------#


#TASK 4: d)	Split the body fat data into two, creating a new variable
# BodyFat-split. Assuming all predictor variables are independent of one another,
# perform statistical tests to assess differences in central tendencies of your
# predictor variables with respect to the newly formed categorical variables
# BodyFat-split. Visualize your results appropriately. Write your assumptions
#and state your hypotheses.

#Importing important libraries
library(ggplot2)
library(dplyr)

data <- read.csv("bodyfat.csv")

#To split the data I will be using "median" of dataset

median_value <- median(data$BodyFat)
data$BodyFat_split <- ifelse(data$BodyFat < median_value, "Low", "High")

str(data)
#I have successfully created a new column named BodyFat_Split

predictors <- colnames(data)[!colnames(data) %in% c("BodyFat", "Density",
                                                    "BodyFat_split")]
#As my target variable is Density rest every variable is Predictor

#Statistical Tests for each predictor variables

#Performing T-test(Parametric)
for (x in predictors) {
  t_test_result <- t.test(data[[x]] ~ data$BodyFat_split)
  print(paste("T-test result for", x))
  print(t_test_result)
  cat("\n")
}

#When looking into the output of t-test I can see that there is significant
#differences in all the predictor variables except "Height" variable. The
#p-values of all variables other than "Height" is below 0.05(For eg. Abdomen
#p-value < 2.2e-16) while for "Height" it is only 0.9368. Also the mean
#differences in these variables is high(eg. Abdomen it is almost 14) while in
#height it is 0.03748 which can be neglected.

#Let's check the normality of predictor variables using Shapiro-Wilk Test
for (x in predictors) {
  if (is.numeric(data[[x]])) {
    shapiro_test_result <- shapiro.test(data[[x]])
    print(paste("Shapiro-Wilk test for", x))
    print(shapiro_test_result)
    cat("\n")
  }
}

#After getting results from Shapiro-Wilk Test I can see most of the variables
#have p-values less than 0.05 however, for "Wrist" it is >0.05. But as most
#of them has lesser p-values it states that variables are not normally
#distributed. Hence, performing t-test is not suitable here. I will perform
#non-parametric test in TASK 5.


#Visualizing the results using QQ Plots, Box Plots and Bar Plots
library(gridExtra)
grid_list <- list() #For getting all plots in one image

#QQ Plot
for (x in predictors) {
  if (is.numeric(data[[x]])) {
    qq_plot <- ggplot(data, aes(sample = .data[[x]])) +
      geom_qq() +
      geom_qq_line() +
      labs(title = paste("QQ Plot for", x),
           x = "Theoretical Quantiles",
           y = "Sample Quantiles") +
      theme_minimal()
    grid_list[[paste0("qq_", x)]] <- qq_plot
  }
}

#Box Plot
for(x in predictors){
  if(is.numeric(data[[x]])){
    box_plot <- ggplot(data, aes(x = BodyFat_split, y = .data[[x]],
                                 fill = BodyFat_split))+ geom_boxplot()+
      labs(title=paste("Box Plot of",x),x = "BodyFat Split" , y = x)+
      theme_minimal()+
      scale_fill_manual(values = c("Low" = "orchid", "High" = "green"))+
      theme(legend.position = "none")
    grid_list[[paste0("box_", x)]] <- box_plot
  }
}
#This gives us the box plot for every predictor variable separately

#Bar Plot
for (x in predictors) {
  if(is.numeric(data[[x]])) {
    bar_plot <- ggplot(data, aes(x = BodyFat_split, y = .data[[x]],
                                 fill = BodyFat_split)) +
      stat_summary(fun = "mean", geom = "bar", position = "dodge") +
      labs(title = paste("Bar Plot of", x), x = "BodyFat Split",
           y = paste("Mean", x)) + theme_minimal() +
      scale_fill_manual(values = c("Low" = "deeppink", "High" = "blue")) +
      theme(legend.position = "none")
    grid_list[[paste0("bar_", x)]] <- bar_plot
  }
}
#This gives us the bar plot for every predictor variable separately

#Creating a grid of 3 x 3
n_plots <- length(grid_list)
plots_per_page <- 9

# Split the list of plots into chunks of `plots_per_page` each
split_plots <- split(grid_list, ceiling(seq_along(grid_list) / plots_per_page))

for (i in 1:length(split_plots)) {
  grid.arrange(grobs = split_plots[[i]], nrow = 3, ncol = 3)
}

#NOTE: For Hypothesis and Assumptions please refer my report. Thank You!

#---------------------------------------------------------------------------------#

#TASK 5: e)	Using ANOVA testing or the nonparametric equivalent, Kruskal- Wallis
# testing, to assess differences in central tendencies of the interval predictor
# variables (eg height, weight etc.) with respect to body fat. For this, you may
# want to create three or four age groups. Perform additional post-hoc tests.
# Write your assumptions and state your hypotheses.

#Importing important libraries
library(tidyr)
library(DescTools)

str(data)

#Creating age groups of 18-28, 29-39, 39-49, 50+
data$Age_groups <- cut(data$Age, breaks = c(18, 28, 39, 49, Inf),
                       labels = c("18-28", "29-39", "40-50", "50+"),
                       right = FALSE)
#Inf is use to define all other ages after 49

#As seen in TASK 4 I have seen the data is not normally distributed hence we
#cannot perform parametric test(i.e. ANOVA) as performing this test on
#non-normally distributed data leads to inaccurate results. Hence we will
#follow the Kruskal-Wallis Test which is an equivalent to ANOVA parametric test.
#I will also perform another non-parametric test called Mann-Whitney U-Test also
#called Wilcoxon Rank Sum Test.


#Kruskal-Wallis Test
library(dunn.test)
#Package for Dunn Test if p-value in Kruskal-Wallis Test is < 0.05

for (x in predictors) {
  if (is.numeric(data[[x]])) {
    kruskal_result <- kruskal.test(data[[x]] ~ data$Age_groups)
    print(paste("Kruskal-Wallis test for", x))
    print(kruskal_result)

    #post-hoc dunn test
    if (kruskal_result$p.value < 0.05) {
      data$Age_groups <- as.factor(data$Age_groups)
      dunn_result <- dunn.test(data[[x]], data$Age_groups)
      print(paste("Dunn Post-hoc test for", x))
      print(dunn_result)
    }
  }
}

#Mann Whitney U-Test (Wilcoxon Rank Sum Test)
library(rstatix)

for (x in predictors) {
  if (is.numeric(data[[x]])) {
    mann_whitney_result <- wilcox.test(data[[x]] ~ data$BodyFat_split)

    print(paste("Mann-Whitney U test for", x))
    print(mann_whitney_result)

    #post-hoc Wilcoxon Test(Pairwise Test)
    if (mann_whitney_result$p.value < 0.05) {
      pairwise_result <- pairwise_wilcox_test(
        data,
        formula = as.formula(paste(x, "~ BodyFat_split")),
        p.adjust.method = "bonferroni"
      )

      print(paste("Post-hoc Wilcoxon test for", x))
      print(pairwise_result)
    }
  }
}

#NOTE: For Hypothesis and Assumptions please refer my report. Thank You!

#---------------------------------------------------------------------------------#

#TASK 6: Perform a PCA Analysis and Hierarchical Model on this dataset to
# identify the underlying factors that contribute to body fat decomposition.
# Discuss your findings. You may use other models beyond taught material in the
# course.

#For PCA it is important to have standardized data and this data(bodyfat.csv) is
#not as all variables have different ranges (For eg. Weight ranges from 118.5
#to 363.15). To perform PCA we need to standardize this data first.

data <- read.csv("bodyfat.csv")

#Standardizing the data
data_standardized <- scale(data[,predictors])
print(data_standardized)

#Performing PCA
PCA <- prcomp(data_standardized[,1:6], center = TRUE, scale = FALSE)
summary(PCA)

#Plotting Scree Plot for PCA
screeplot(PCA, main = "Scree Plot", col = "orchid", pch = 19)

#Plotting Biplot for PCA
biplot(PCA)

#For Hierarchical Model or Clustering I need a distance matrix to calculate
#pairwise distance

#Distance Matrix
distance_matrix <- dist(data_standardized)
print(distance_matrix)

#Performing Hierarchical Clustering
Hierarchical_Clustering <- hclust(distance_matrix, method = "ward.D2")
summary(Hierarchical_Clustering)

#Plotting Hierarchical Clustering's Dendogram
plot(Hierarchical_Clustering, main = "Dendogram for Hierarchical Clustering")

#Underlying Factors
#PC1 is the major factor explaining 60.25% variance and its capturing most
#important variation in the dataset. PC2 explains 20.45% of variance which
#brings cumulative variance to 80.7% which means first two PCA defines most of
#the dataset while PC3 also brings 12.64% of variance bringing total of 93.34%
#of variance. However, PC4, PC5 and PC6 contributes less variance.

#I can conclude that PC1 is the most important factor likely to represent
#body fat decomposition. The main contributors to body fat decomposition can
#be Weight.

#---------------------------------------------------------------------------------#

#Implementation part ends here for the detailed explanation please refer to my
#report. Thank You!

