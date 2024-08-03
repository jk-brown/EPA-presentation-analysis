library(ggplot2)
library(dplyr)

# Assuming params_weights is your data frame with parameter combinations and probabilistic weights
weighted_params <- weights_params %>%
  mutate(across(everything(), ~replace_na(.x, 0)))  # Replace NA with 0 for PCA

# Perform PCA
pca_result <- prcomp(weighted_params %>% select(-prob_weight), scale. = TRUE)

# Get the PCA scores
pca_scores <- as.data.frame(pca_result$x)

# Add the probabilistic weights back to the PCA scores
pca_scores$prob_weight <- weighted_params$prob_weight

# Extract the prob_weight as a numeric vector
pca_scores$prob_weight <- as.numeric(pca_scores$prob_weight$weights)

# Verify the structure again
str(pca_scores)

# Plot the PCA results
ggplot(pca_scores, aes(x = PC1, y = PC2, color = prob_weight)) +
  geom_point() +
  scale_color_continuous() +
  labs(title = "PCA of Parameter Combinations", x = "Principal Component 1", y = "Principal Component 2")


set.seed(123)  # For reproducibility

# Number of samples you want to draw
n_samples <- 100

# Function to sample based on probabilistic weights
sample_parameters <- function(pca_scores, n_samples) {
  sampled_indices <- sample(1:nrow(pca_scores), size = n_samples, prob = pca_scores$prob_weight, replace = TRUE)
  sampled_params <- pca_scores[sampled_indices, ]
  return(sampled_params)
}

# Sample the parameter sets
sampled_params <- sample_parameters(pca_scores, n_samples)

# Optionally, plot the sampled parameters
ggplot(sampled_params, aes(x = PC1, y = PC2, color = prob_weight)) +
  geom_point() +
  scale_color_continuous() +
  labs(title = "Sampled Parameter Sets in PCA Space", x = "Principal Component 1", y = "Principal Component 2")


# Summary of PCA
summary(pca_result)

# Create a scree plot
explained_variance <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

# Plot the explained variance
scree_plot <- data.frame(
  PC = 1:length(explained_variance),
  ExplainedVariance = explained_variance,
  CumulativeVariance = cumulative_variance
)

ggplot(scree_plot, aes(x = PC)) +
  geom_bar(aes(y = ExplainedVariance), stat = "identity", fill = "skyblue") +
  geom_line(aes(y = CumulativeVariance), color = "red", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "red", size = 2) +
  labs(title = "Scree Plot", x = "Principal Component", y = "Proportion of Variance Explained") +
  theme_minimal()
