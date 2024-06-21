---
title: "Data Visualization | Mini-Project 01"
author: "Marckenrold Cadet | `mcadet6565@floridapoly.edu`"
output: 
  html_document:
    keep_md: true
    toc: true
    toc_float: true
---

# Introduction
In the realm of industrial manufacturing, machine failures can lead to significant downtime, reduced productivity, and increased maintenance costs. Predictive analysis of machine failures can provide valuable insights to preemptively address potential issues, thereby optimizing operations and minimizing disruptions. This project focuses on analyzing machine failure data to uncover patterns and trends that can inform maintenance strategies and improve overall machine performance.

# Background
Machine failures in industrial settings are inevitable, but understanding the underlying causes and patterns can help mitigate their impact. Predictive maintenance, driven by data analysis and visualization, has become a critical component in modern manufacturing. By leveraging historical data on machine failures, it is possible to identify trends and anticipate future breakdowns.

The dataset used in this project originates from the AI4I 2020 Predictive Maintenance Dataset, which includes records of various machine failures, operating conditions, and sensor readings. The objective is to explore this dataset through a series of visualizations, providing a comprehensive view of machine performance and identifying key factors that contribute to failures.

# Methodology
For this project involved several key steps. First, the AI4I 2020 Predictive Maintenance Dataset was acquired and cleaned to ensure consistency and accuracy. Exploratory data analysis (EDA) was then conducted to understand the dataset's basic characteristics and identify initial patterns. A set of visualizations, including K-means, Principal Component Analysis (CPA), Gaussian distribution, Receiver Operating Characteristic (ROC) and control charts, were implemented using R and the ggplot2 among other libraries to address different aspects of the data. Finally, these visualizations were analyzed to extract meaningful insights, focusing on machine failure distributions, high-risk machine types, and trends over time, with a particular emphasis on creating a Six Sigma control chart to monitor machine performance.

# Analysis 1: Exploratory data analysis (EDA)

```r
# load necessary libraries
library(tidyverse)
```

```
## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
## ✔ dplyr     1.1.3     ✔ readr     2.1.4
## ✔ forcats   1.0.0     ✔ stringr   1.5.0
## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
## ✔ purrr     1.0.2     
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## ✖ dplyr::filter() masks stats::filter()
## ✖ dplyr::lag()    masks stats::lag()
## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors
```

```r
library(dplyr)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
## 
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(ggplot2)
library(factoextra)
```

```
## Welcome! Want to learn more? See two factoextra-related books at https://goo.gl/ve3WBa
```

```r
library(e1071)  
library(ggpubr)  
library(caret)
```

```
## Loading required package: lattice
## 
## Attaching package: 'caret'
## 
## The following object is masked from 'package:purrr':
## 
##     lift
```

```r
library(pROC)
```

```
## Type 'citation("pROC")' for a citation.
## 
## Attaching package: 'pROC'
## 
## The following objects are masked from 'package:stats':
## 
##     cov, smooth, var
```

```r
library(qcc)
```

```
## Package 'qcc' version 2.7
## Type 'citation("qcc")' for citing this R package in publications.
```


```r
# load data
file_path <- "~/Desktop/2024/Summer/Data Viz/Mini-Project_1/data/ai4i2020.csv"
data <- read_csv(file_path)
```

```
## Rows: 10000 Columns: 14
## ── Column specification ────────────────────────────────────────────────────────
## Delimiter: ","
## chr  (2): Product ID, Type
## dbl (12): UDI, Air temperature [K], Process temperature [K], Rotational spee...
## 
## ℹ Use `spec()` to retrieve the full column specification for this data.
## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.
```

```r
# read sample of data
sample_n(data, 6)
```

```
## # A tibble: 6 × 14
##     UDI `Product ID` Type  `Air temperature [K]` `Process temperature [K]`
##   <dbl> <chr>        <chr>                 <dbl>                     <dbl>
## 1  6397 L53576       L                      300                       310.
## 2  1678 M16537       M                      298.                      308.
## 3  4774 L51953       L                      304.                      312 
## 4  2254 L49433       L                      299.                      308.
## 5  9184 L56363       L                      298.                      309.
## 6  5851 L53030       L                      301.                      311.
## # ℹ 9 more variables: `Rotational speed [rpm]` <dbl>, `Torque [Nm]` <dbl>,
## #   `Tool wear [min]` <dbl>, `Machine failure` <dbl>, TWF <dbl>, HDF <dbl>,
## #   PWF <dbl>, OSF <dbl>, RNF <dbl>
```

```r
# using the janitor package - let's clean the columns name
data <- data %>% clean_names()
data
```

```
## # A tibble: 10,000 × 14
##      udi product_id type  air_temperature_k process_temperature_k
##    <dbl> <chr>      <chr>             <dbl>                 <dbl>
##  1     1 M14860     M                  298.                  309.
##  2     2 L47181     L                  298.                  309.
##  3     3 L47182     L                  298.                  308.
##  4     4 L47183     L                  298.                  309.
##  5     5 L47184     L                  298.                  309.
##  6     6 M14865     M                  298.                  309.
##  7     7 L47186     L                  298.                  309.
##  8     8 L47187     L                  298.                  309.
##  9     9 M14868     M                  298.                  309.
## 10    10 M14869     M                  298.                  309 
## # ℹ 9,990 more rows
## # ℹ 9 more variables: rotational_speed_rpm <dbl>, torque_nm <dbl>,
## #   tool_wear_min <dbl>, machine_failure <dbl>, twf <dbl>, hdf <dbl>,
## #   pwf <dbl>, osf <dbl>, rnf <dbl>
```


```r
# let's check the structure of the data
str(data)
```

```
## spc_tbl_ [10,000 × 14] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
##  $ udi                  : num [1:10000] 1 2 3 4 5 6 7 8 9 10 ...
##  $ product_id           : chr [1:10000] "M14860" "L47181" "L47182" "L47183" ...
##  $ type                 : chr [1:10000] "M" "L" "L" "L" ...
##  $ air_temperature_k    : num [1:10000] 298 298 298 298 298 ...
##  $ process_temperature_k: num [1:10000] 309 309 308 309 309 ...
##  $ rotational_speed_rpm : num [1:10000] 1551 1408 1498 1433 1408 ...
##  $ torque_nm            : num [1:10000] 42.8 46.3 49.4 39.5 40 41.9 42.4 40.2 28.6 28 ...
##  $ tool_wear_min        : num [1:10000] 0 3 5 7 9 11 14 16 18 21 ...
##  $ machine_failure      : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  $ twf                  : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  $ hdf                  : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  $ pwf                  : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  $ osf                  : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  $ rnf                  : num [1:10000] 0 0 0 0 0 0 0 0 0 0 ...
##  - attr(*, "spec")=
##   .. cols(
##   ..   UDI = col_double(),
##   ..   `Product ID` = col_character(),
##   ..   Type = col_character(),
##   ..   `Air temperature [K]` = col_double(),
##   ..   `Process temperature [K]` = col_double(),
##   ..   `Rotational speed [rpm]` = col_double(),
##   ..   `Torque [Nm]` = col_double(),
##   ..   `Tool wear [min]` = col_double(),
##   ..   `Machine failure` = col_double(),
##   ..   TWF = col_double(),
##   ..   HDF = col_double(),
##   ..   PWF = col_double(),
##   ..   OSF = col_double(),
##   ..   RNF = col_double()
##   .. )
##  - attr(*, "problems")=<externalptr>
```

```r
# Summary statistics 
summary(data)
```

```
##       udi         product_id            type           air_temperature_k
##  Min.   :    1   Length:10000       Length:10000       Min.   :295.3    
##  1st Qu.: 2501   Class :character   Class :character   1st Qu.:298.3    
##  Median : 5000   Mode  :character   Mode  :character   Median :300.1    
##  Mean   : 5000                                         Mean   :300.0    
##  3rd Qu.: 7500                                         3rd Qu.:301.5    
##  Max.   :10000                                         Max.   :304.5    
##  process_temperature_k rotational_speed_rpm   torque_nm     tool_wear_min
##  Min.   :305.7         Min.   :1168         Min.   : 3.80   Min.   :  0  
##  1st Qu.:308.8         1st Qu.:1423         1st Qu.:33.20   1st Qu.: 53  
##  Median :310.1         Median :1503         Median :40.10   Median :108  
##  Mean   :310.0         Mean   :1539         Mean   :39.99   Mean   :108  
##  3rd Qu.:311.1         3rd Qu.:1612         3rd Qu.:46.80   3rd Qu.:162  
##  Max.   :313.8         Max.   :2886         Max.   :76.60   Max.   :253  
##  machine_failure       twf              hdf              pwf        
##  Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000  
##  Mean   :0.0339   Mean   :0.0046   Mean   :0.0115   Mean   :0.0095  
##  3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000  
##       osf              rnf        
##  Min.   :0.0000   Min.   :0.0000  
##  1st Qu.:0.0000   1st Qu.:0.0000  
##  Median :0.0000   Median :0.0000  
##  Mean   :0.0098   Mean   :0.0019  
##  3rd Qu.:0.0000   3rd Qu.:0.0000  
##  Max.   :1.0000   Max.   :1.0000
```

```r
# Check for missing values
colSums(is.na(data))
```

```
##                   udi            product_id                  type 
##                     0                     0                     0 
##     air_temperature_k process_temperature_k  rotational_speed_rpm 
##                     0                     0                     0 
##             torque_nm         tool_wear_min       machine_failure 
##                     0                     0                     0 
##                   twf                   hdf                   pwf 
##                     0                     0                     0 
##                   osf                   rnf 
##                     0                     0
```

```r
# Grouping data by machine type and summarizing key metrics
summary_stats <- data %>%
  group_by(type) %>%
  summarize(
    count = n(),
    mean_failures = mean(machine_failure, na.rm = TRUE),
    median_failures = median(machine_failure, na.rm = TRUE),
    sd_failures = sd(machine_failure, na.rm = TRUE),
    min_failures = min(machine_failure, na.rm = TRUE),
    max_failures = max(machine_failure, na.rm = TRUE),
    mean_tool_wear = mean(tool_wear_min, na.rm = TRUE),
    mean_air_temp = mean(air_temperature_k, na.rm = TRUE),
    mean_process_temp = mean(process_temperature_k, na.rm = TRUE),
    mean_rotational_speed = mean(rotational_speed_rpm, na.rm = TRUE),
    mean_torque = mean(torque_nm, na.rm = TRUE),
    mean_tool_wear_min = mean(tool_wear_min, na.rm = TRUE)
  )

# view output
summary_stats
```

```
## # A tibble: 3 × 13
##   type  count mean_failures median_failures sd_failures min_failures
##   <chr> <int>         <dbl>           <dbl>       <dbl>        <dbl>
## 1 H      1003        0.0209               0       0.143            0
## 2 L      6000        0.0392               0       0.194            0
## 3 M      2997        0.0277               0       0.164            0
## # ℹ 7 more variables: max_failures <dbl>, mean_tool_wear <dbl>,
## #   mean_air_temp <dbl>, mean_process_temp <dbl>, mean_rotational_speed <dbl>,
## #   mean_torque <dbl>, mean_tool_wear_min <dbl>
```

```r
# Convert the summary statistics
summary_stats_formatted <- summary_stats %>%
  mutate_if(is.numeric, round, 2)

summary_stats_formatted
```

```
## # A tibble: 3 × 13
##   type  count mean_failures median_failures sd_failures min_failures
##   <chr> <dbl>         <dbl>           <dbl>       <dbl>        <dbl>
## 1 H      1003          0.02               0        0.14            0
## 2 L      6000          0.04               0        0.19            0
## 3 M      2997          0.03               0        0.16            0
## # ℹ 7 more variables: max_failures <dbl>, mean_tool_wear <dbl>,
## #   mean_air_temp <dbl>, mean_process_temp <dbl>, mean_rotational_speed <dbl>,
## #   mean_torque <dbl>, mean_tool_wear_min <dbl>
```

# Analysis 2: K-means
K-means clustering is a popular method for partitioning data into distinct groups based on similarity. In this analysis, we will use K-means clustering to identify groups of machines with similar failure patterns and characteristics.


```r
# Select columns for clustering
clustering_data <- data %>%
  select(machine_failure, tool_wear_min, air_temperature_k, process_temperature_k, rotational_speed_rpm, torque_nm)

# Normalize data
clustering_data <- scale(clustering_data)
```

### Determine the Optimal Number of Clusters: 
Use the Elbow Method to determine the optimal number of clusters.

```r
# looking for the optimal number of clusters using the Elbow Method
set.seed(123)
fviz_nbclust(clustering_data, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method for Optimal Clusters")
```

![](lastname_project_01_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
<br>
### Perform K-means Clustering: 

Apply K-means clustering with the chosen number of clusters.

```r
# Set the number of clusters
k <- 3
set.seed(123)
kmeans_result <- kmeans(clustering_data, centers = k, nstart = 25)
```

### Visualize the Clusters: 
Visualize the clustering results using a scatter plot.

```r
# Add the cluster assignment to the original data
data$cluster <- as.factor(kmeans_result$cluster)

# Visualize the clusters
fviz_cluster(kmeans_result, data = clustering_data, geom = "point",
             ellipse.type = "convex", ggtheme = theme_minimal(),
             main = "K-means Clustering of Machine Failures")
```

![](lastname_project_01_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

<br>By performing K-means clustering, I was able to identify distinct groups of machines based on their failure characteristics and operating conditions. This analysis helps in understanding the underlying patterns and can inform targeted maintenance strategies for different groups of machines.


# Analysis 3: Principal Component Analysis
Principal Component Analysis (PCA) is a dimensionality reduction technique used to reduce the number of variables in a dataset while retaining most of the variability in the data. In this analysis, we will use PCA to understand the underlying structure of the machine failure data and identify the principal components that explain the most variance.


```r
# Select columns for PCA
pca_data <- data %>%
  select(machine_failure, tool_wear_min, air_temperature_k, process_temperature_k, rotational_speed_rpm, torque_nm)

# Normalize data
pca_data <- scale(pca_data)
```

### Perform & Summarize PCA Results:
Summarize and visualize the PCA results to understand the variance explained by each principal component.

```r
# Perform PCA
pca_result <- prcomp(pca_data, scale. = TRUE)

# Summary of PCA results
summary(pca_result)
```

```
## Importance of components:
##                           PC1    PC2    PC3    PC4     PC5     PC6
## Standard deviation     1.3845 1.3693 1.0430 0.9409 0.35079 0.33447
## Proportion of Variance 0.3195 0.3125 0.1813 0.1475 0.02051 0.01865
## Cumulative Proportion  0.3195 0.6320 0.8133 0.9608 0.98135 1.00000
```

```r
# Using Scree plot to visualize the variance explained by each principal component
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))
```

![](lastname_project_01_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
<br>
### Visualize PCA Results:
Visualize the principal components and how the original variables contribute to them.

```r
# Biplot 
fviz_pca_biplot(pca_result, repel = TRUE,
                col.var = "blue",
                col.ind = "cos2", 
                gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                addEllipses = TRUE, ellipse.type = "confidence",
                geom.ind = "point", pointshape = 21, pointsize = 2.5,
                ggtheme = theme_minimal())
```

```
## Warning: Computation failed in `stat_conf_ellipse()`
## Caused by error in `if (scale[1] > 0) ...`:
## ! missing value where TRUE/FALSE needed
```

![](lastname_project_01_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
<br>The PCA analysis will provide insights into the most significant variables that contribute to machine failures and how these variables are related. This biplot helps in understanding the relationships between variables and identifying any clusters or patterns in the data.

# Analysis 4: Gaussian Distribution
Gaussian distribution, also known as the normal distribution, is a continuous probability distribution characterized by its bell-shaped curve. In this analysis, we will explore whether the key variables in the dataset follow a Gaussian distribution. This can help in understanding the underlying data distribution and its characteristics.


```r
# Select columns 
gaussian_data <- data %>%
  select(machine_failure, tool_wear_min, air_temperature_k, process_temperature_k, rotational_speed_rpm, torque_nm)
```

### Plot Histograms and Density Plots:
Create histograms and density plots to visually inspect the distribution of each variable.

```r
# Histogram & density plot 
plot_list <- list()
for (col in colnames(gaussian_data)) {
  p <- ggplot(gaussian_data, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", alpha = 0.7) +
    geom_density(color = "darkblue", size = 1) +
    labs(title = paste("Histogram and Density Plot of", col),
         x = col, y = "Density") +
    theme_minimal()
  plot_list[[col]] <- p
}
```

```
## Warning: `aes_string()` was deprecated in ggplot2 3.0.0.
## ℹ Please use tidy evaluation idioms with `aes()`.
## ℹ See also `vignette("ggplot2-in-packages")` for more information.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```
## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
## ℹ Please use `linewidth` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

```r
#  plots
ggarrange(plotlist = plot_list, ncol = 2, nrow = 3)
```

```
## Warning: The dot-dot notation (`..density..`) was deprecated in ggplot2 3.4.0.
## ℹ Please use `after_stat(density)` instead.
## This warning is displayed once every 8 hours.
## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
## generated.
```

![](lastname_project_01_files/figure-html/unnamed-chunk-14-1.png)<!-- -->

### Calculate Skewness and Kurtosis:
Calculate skewness and kurtosis to quantify the asymmetry and peakedness of the distribution.

```r
# Function to calculate skewness and kurtosis
calc_skew_kurt <- function(data) {
  data.frame(
    Variable = names(data),
    Skewness = sapply(data, skewness),
    Kurtosis = sapply(data, kurtosis)
  )
}

skew_kurt_result <- calc_skew_kurt(gaussian_data)
skew_kurt_result
```

```
##                                    Variable     Skewness    Kurtosis
## machine_failure             machine_failure  5.150306333 24.52810816
## tool_wear_min                 tool_wear_min  0.027284052 -1.16712042
## air_temperature_k         air_temperature_k  0.114239641 -0.83657644
## process_temperature_k process_temperature_k  0.015022760 -0.50058443
## rotational_speed_rpm   rotational_speed_rpm  1.992573093  7.38657138
## torque_nm                         torque_nm -0.009513741 -0.01443114
```
<br>This analysis provides visual and statistical insights into whether the key variables follow a Gaussian distribution. The histogram and density plots shows the shape of the distribution, while skewness, and kurtosis test results quantify the normality.

# Analysis 5: ROC 
Receiver Operating Characteristic (ROC) curve is a graphical representation of the performance of a classification model at different threshold settings. It is used to evaluate the diagnostic ability of a binary classifier system. In this analysis, we will use ROC curve analysis to evaluate the performance of a machine failure prediction model.


```r
# Select columns for analysis 
data <- data %>%
  mutate(machine_failure = as.factor(machine_failure)) %>%
  select(machine_failure, tool_wear_min, air_temperature_k, process_temperature_k, rotational_speed_rpm, torque_nm)
```

### Split Data into Training and Testing Sets:
Split the dataset into training and testing sets for model evaluation.

```r
set.seed(123)  
train_index <- createDataPartition(data$machine_failure, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]
```

### Train a Classification Model:
Train a logistic regression model to predict machine failure.

```r
# Train logistic regression for model
model <- glm(machine_failure ~ ., data = train_data, family = binomial)

# Summarize 
summary(model)
```

```
## 
## Call:
## glm(formula = machine_failure ~ ., family = binomial, data = train_data)
## 
## Coefficients:
##                         Estimate Std. Error z value Pr(>|z|)    
## (Intercept)           -4.115e+01  1.652e+01  -2.491   0.0127 *  
## tool_wear_min          1.378e-02  1.281e-03  10.751  < 2e-16 ***
## air_temperature_k      8.632e-01  8.403e-02  10.272  < 2e-16 ***
## process_temperature_k -8.199e-01  1.102e-01  -7.437 1.03e-13 ***
## rotational_speed_rpm   1.184e-02  6.014e-04  19.685  < 2e-16 ***
## torque_nm              2.913e-01  1.318e-02  22.104  < 2e-16 ***
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## (Dispersion parameter for binomial family taken to be 1)
## 
##     Null deviance: 2374.2  on 8000  degrees of freedom
## Residual deviance: 1491.0  on 7995  degrees of freedom
## AIC: 1503
## 
## Number of Fisher Scoring iterations: 8
```

### Make Predictions and Calculate Probabilities:
Use the model to make predictions on the test data and calculate probabilities.

```r
# Make predictions on the test data
test_data$predicted_prob <- predict(model, test_data, type = "response")
test_data$predicted_class <- ifelse(test_data$predicted_prob > 0.5, 1, 0)
```

### Generate and Plot ROC Curve:
Generate the ROC curve and calculate the Area Under the Curve (AUC).

```r
# Generate ROC curve
roc_curve <- roc(test_data$machine_failure, test_data$predicted_prob)
```

```
## Setting levels: control = 0, case = 1
```

```
## Setting direction: controls < cases
```

```r
# Plot 
plot(roc_curve, col = "blue", main = "ROC Curve for Machine Failure Prediction")
abline(a = 0, b = 1, lty = 2, col = "red")  
```

![](lastname_project_01_files/figure-html/unnamed-chunk-20-1.png)<!-- -->

```r
roc_curve <- roc(test_data$machine_failure, test_data$predicted_prob)
```

```
## Setting levels: control = 0, case = 1
## Setting direction: controls < cases
```

```r
# Calculate AUC
auc_value <- auc(roc_curve)
auc_value
```

```
## Area under the curve: 0.8574
```
<br>The ROC curve shows the trade-off between the true positive rate and the false positive rate at different threshold settings, while the AUC value provides a single metric to assess the model's performance.

# Analysis 6: Six Sigma 
Six Sigma is a data-driven methodology for eliminating defects and improving quality in processes. In this analysis, we will apply Six Sigma principles to the machine failure data to identify periods of instability and deviations from acceptable performance limits. We will use control charts to monitor machine performance and calculate Six Sigma limits.


```r
# convert columns to numeric
sigma_data <- data %>%
  mutate(across(where(is.character), as.numeric)) %>%
  mutate(across(where(is.factor), as.numeric)) %>%
  select(machine_failure, tool_wear_min, air_temperature_k, process_temperature_k, rotational_speed_rpm, torque_nm)

# Add a time sequence for control chart plotting
sigma_data$time <- seq_len(nrow(sigma_data))
```

### Calculate Six Sigma Limits:
Define a function to calculate Six Sigma limits for a given variable.

```r
six_sigma_limits <- function(data, column) {
  mean_val <- mean(data[[column]], na.rm = TRUE)
  sd_val <- sd(data[[column]], na.rm = TRUE)
  upper_limit <- mean_val + 6 * sd_val
  lower_limit <- mean_val - 6 * sd_val
  return(c(lower_limit, upper_limit))
}

# Calculate Six Sigma limits
six_sigma_failure_limits <- six_sigma_limits(sigma_data, "machine_failure")
six_sigma_failure_limits
```

```
## [1] -0.05198506  2.11978506
```

### Create Control Chart:
Use ggplot2 to create a control chart with Six Sigma limits.

```r
# Function to create a control chart
control_chart <- function(data, column) {
  mean_val <- mean(data[[column]], na.rm = TRUE)
  sigma_val <- sd(data[[column]], na.rm = TRUE)
  upper_limit_3sigma <- mean_val + 3 * sigma_val
  lower_limit_3sigma <- mean_val - 3 * sigma_val
  upper_limit_6sigma <- mean_val + 6 * sigma_val
  lower_limit_6sigma <- mean_val - 6 * sigma_val

  ggplot(data, aes(x = time, y = data[[column]])) +
    geom_line(color = "blue") +
    geom_point(color = "blue") +
    geom_hline(yintercept = mean_val, color = "green", linetype = "dashed") +
    geom_hline(yintercept = upper_limit_3sigma, color = "red", linetype = "dashed") +
    geom_hline(yintercept = lower_limit_3sigma, color = "red", linetype = "dashed") +
    geom_hline(yintercept = upper_limit_6sigma, color = "darkred", linetype = "dashed") +
    geom_hline(yintercept = lower_limit_6sigma, color = "darkred", linetype = "dashed") +
    labs(title = paste("Control Chart for", column),
         x = "Time",
         y = "Sensor Reading") +
    theme_minimal()
}

# Plot 
control_chart(sigma_data, 'machine_failure')
```

```
## Warning: Use of `data[[column]]` is discouraged.
## ℹ Use `.data[[column]]` instead.
## Use of `data[[column]]` is discouraged.
## ℹ Use `.data[[column]]` instead.
```

![](lastname_project_01_files/figure-html/unnamed-chunk-23-1.png)<!-- -->
<br>The Six Sigma analysis provides a visual representation of machine performance over time, highlighting deviations from the mean and identifying periods where performance exceeded Six Sigma limits.

# Key Strategies for Improving Machine Performance and Reducing Failures

Based on the analyses performed (summary statistics, K-means clustering, PCA, Gaussian distribution, ROC curve analysis, and Six Sigma analysis), several key strategies can be implemented to improve machine performance and reduce failures:

#### 1. **Predictive Maintenance:**
   - **Data-Driven Maintenance Scheduling:** Use the predictive analysis from the ROC curve and Six Sigma control charts to schedule maintenance activities before a failure occurs. This proactive approach can minimize downtime and prevent unexpected breakdowns.
   - **Monitoring Critical Variables:** Focus on critical variables identified through PCA and K-means clustering, such as tool wear, air temperature, and rotational speed. Regularly monitor these parameters to detect early signs of potential failures.

#### 2. **Process Optimization:**
   - **Six Sigma Methodology:** Implement Six Sigma principles to continuously monitor and improve machine performance. Use control charts to identify variations and take corrective actions to maintain processes within acceptable limits.
   - **Root Cause Analysis:** Perform root cause analysis on significant deviations and outliers identified in the control charts to understand and address underlying issues.

#### 3. **Data Integration and Visualization:**
   - **Integrated Dashboard:** Develop an integrated dashboard using tools like Power BI or R Shiny to visualize real-time data and historical trends. This allows for quick identification of issues and informed decision-making.
   - **Customizable Alerts:** Set up customizable alerts for key metrics that exceed predefined thresholds, enabling timely interventions.

#### 4. **Employee Training and Engagement:**
   - **Training Programs:** Implement training programs to educate employees on the importance of data-driven decision-making and how to use data visualization tools effectively.
   - **Active Leadership:** Encourage active leadership and involvement from all levels of the organization to foster a culture of continuous improvement and accountability.

#### 5. **Statistical and Machine Learning Techniques:**
   - **Advanced Analytics:** Utilize statistical and machine learning techniques to enhance predictive models. Techniques such as logistic regression, decision trees, and neural networks can provide more accurate predictions of machine failures.
   - **Anomaly Detection:** Implement anomaly detection algorithms to identify unusual patterns or behaviors that may indicate potential failures.

#### 6. **Continuous Improvement:**
   - **Regular Audits:** Conduct regular audits of maintenance processes and data collection methods to ensure accuracy and reliability.
   - **Feedback Loop:** Establish a feedback loop where insights from data analyses are used to refine and improve maintenance strategies continuously.

### Implementation Plan

1. **Phase 1: Data Collection and Monitoring**
   - Install sensors and data collection devices on critical machines.
   - Develop a dashboard to monitor real-time data and historical trends.
   - Train employees on data collection and initial analysis techniques.

2. **Phase 2: Predictive Analytics and Maintenance**
   - Implement predictive maintenance schedules based on the analysis.
   - Use ROC curve and Six Sigma analyses to identify critical failure points.
   - Set up alerts for key metrics.

3. **Phase 3: Process Optimization**
   - Conduct root cause analysis for identified deviations.
   - Apply Six Sigma methodologies to reduce variability and improve processes.
   - Regularly update predictive models with new data.

4. **Phase 4: Continuous Improvement and Training**
   - Conduct regular training sessions for employees on new tools and techniques.
   - Perform regular audits and reviews of the maintenance processes.
   - Incorporate feedback and new insights into the maintenance strategy.

# Conclusion

By leveraging data-driven techniques and strategies, organizations can significantly enhance machine performance, reduce failure rates, and optimize maintenance processes. Continuous monitoring, predictive analytics, and proactive maintenance are essential components of an effective strategy to ensure operational efficiency and reliability.



