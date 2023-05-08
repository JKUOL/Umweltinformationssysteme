---
title: "2. Fish Classifier"
author: "Justin König"
date: "08.05.2023"
output:
  html_document:
    keep_md: yes
    code_folding: hide
    theme: yeti
    toc: yes
    toc_float: yes
  pdf_document:
    toc: yes
editor_options:
  markdown:
    wrap: 72
---





```r
# load data frame
fish_df <- read.csv2("fishcatch.csv")

# remove unnecessary data
fish_df <- fish_df[48:nrow(fish_df),]

colnames(fish_df) <- c("Species", "Length1", "Length2", "Length3","Height", "Width",
                       "sex", "Weight")

fish_df <- as.data.frame(apply(fish_df, 2, function(x) gsub(",", ".", x)))

# List the columns you want to convert to numeric
columns_to_convert <- c("Length1", "Length2", "Length3","Height", "Width",
                       "Weight")

# counts fish classes
fish_count <- fish_df %>%
  group_by(Species) %>%
  summarise(Count = n())

# Convert the specified columns to numeric
fish_df <- fish_df %>%
  mutate(across(all_of(columns_to_convert), as.numeric))


train_data <- fish_df[!(seq_len(nrow(fish_df)) %% 10 == 0), ]

# Testdatensatz erstellen (jeder 10. Fisch)
test_data <- fish_df[seq(1, nrow(fish_df), by = 10), ]
```


```r
ggplot(fish_count, aes(x = Species, y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_classic() +
  labs(title = "Fish Count per Species",
       x = "Species",
       y = "Count")
```

![](Fish-Classifier_files/figure-html/visualitation-1.png)<!-- -->




```r
# Get the list of unique species
unique_species <- unique(fish_df$Species)

# Calculate the mean for each parameter for each species
mean_species_list <- lapply(unique_species, function(fish_species) {
  mean_species <- fish_df %>%
    filter(Species == fish_species) %>%
    summarise(Mean_Length1 = mean(Length1, na.rm = TRUE),
              Mean_Length2 = mean(Length2, na.rm = TRUE),
              Mean_Length3 = mean(Length3, na.rm = TRUE),
              Mean_Height = mean(Height, na.rm = TRUE),
              Mean_Width = mean(Width, na.rm = TRUE),
              Mean_Weight = mean(Weight, na.rm = TRUE))
  mean_species
})

# Assign names to the list elements based on species
names(mean_species_list) <- unique_species

# Print the mean values for each species
mean_species_list
```

```
## $`1`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1     30.32941     33.14118     38.38529    39.59118   14.14706         626
## 
## $`2`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1         28.8     31.31667     34.31667        29.2       15.9         531
## 
## $`3`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1       20.645       22.275        24.97      26.735     14.605      152.05
## 
## $`4`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1     18.72727     20.34545     22.79091    39.30909   14.08182    154.8182
## 
## $`5`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1     11.25714     11.92143     13.03571    16.88571   10.22143    11.17857
## 
## $`6`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1     42.47647     45.48235     48.71765    15.84118   10.43529    718.7059
## 
## $`7`
##   Mean_Length1 Mean_Length2 Mean_Length3 Mean_Height Mean_Width Mean_Weight
## 1     25.73571     27.89286     29.57143    26.25714   15.83929    382.2393
```


```r
# Load the data
df <- fish_df

# Split the data into training and testing sets
set.seed(42)
split_index <- sample(1:nrow(fish_df), 0.7 * nrow(fish_df))
train_data <- fish_df[split_index, ]
test_data <- fish_df[-split_index, ]

# Calculate the mean, covariance, and a-priori probability for each fish species
species_list <- unique(train_data$Species)
stats_list <- lapply(species_list, function(species) {
  species_data <- train_data[train_data$Species == species, c("Length1", "Length2", "Length3", "Height", "Width")]
  n <- nrow(species_data)
  
  list(mean = colMeans(species_data),
       cov = cov(species_data) + diag(1e-6, ncol(species_data)),
       prior = n / nrow(train_data))
})

names(stats_list) <- species_list

# Multivariate Gaussian density function
multivariate_gaussian <- function(x, mean, cov) {
  k <- length(mean)
  exp(-0.5 * t(x - mean) %*% solve(cov, x - mean)) / sqrt((2 * pi)^k * det(cov))
}

# QDA classifier
classify_fish_qda <- function(lengths) {
  likelihoods <- sapply(stats_list, function(params) {
    likelihood <- multivariate_gaussian(lengths, params$mean, params$cov)
    likelihood * params$prior
  })
  
  names(likelihoods)[which.max(likelihoods)]
}

# Classify each fish in the test set
predicted_species <- apply(test_data[, c("Length1", "Length2", "Length3", "Height", "Width")], 1, classify_fish_qda)

# Calculate the accuracy
correct_predictions <- sum(predicted_species == test_data$Species)
total_predictions <- length(predicted_species)

accuracy <- correct_predictions / total_predictions
cat("The accuracy of the QDA classifier on the test set is:", accuracy * 100, "%\n")
```

```
## The accuracy of the QDA classifier on the test set is: 81.25 %
```
