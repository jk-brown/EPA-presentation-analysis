# Assume `results` is your data frame containing parameters and their probabilistic weights
params_weights <- weighted_metrics_df %>%
   reweight() %>% 
   select(run_number, reweighted_value) %>% 
   rename("prob_weight" = reweighted_value) %>% 
   distinct(run_number, prob_weight) %>% 
   left_join(params, by = "run_number") %>% 
   select(-run_number)

# Function to normalize weights and sample one parameter
sample_one_parameter <- function(param, current_params_weights) {
  # Collapse the space onto the current parameter
  param_pdf <- current_params_weights %>%
    group_by(across(all_of(param))) %>%
    summarise(weight = sum(prob_weight, na.rm = TRUE), .groups = 'drop') %>%
    mutate(weight = weight / sum(weight))  # Normalize weights
  
  # Check if there are weights
  if (nrow(param_pdf) == 0) {
    stop(paste("No data found for parameter", param))
  }
  
  # Check if weights sum to one
  if (sum(param_pdf$weight) != 1){
    stop(paste("Parameter weights do not sum to 1."))
  }
  
  # Sample the current parameter
  sampled_value <- sample(param_pdf[[param]], size = 1, prob = param_pdf$weight, replace = TRUE)
  
  # Filter the current_params_weights based on the sampled value
  new_params_weights <- current_params_weights %>% filter(get(param) == sampled_value)
  
  list(sampled_value = sampled_value, current_params_weights = new_params_weights)
}

# Function to sample parameters sequentially
sample_parameters <- function(params_weights, param_names, n_samples) {
  sample_one_set <- function(i) {
    current_params_weights <- params_weights
    sampled_values <- sapply(param_names, function(param) {
      result <- sample_one_parameter(param, current_params_weights)
      current_params_weights <- result$current_params_weights
      result$sampled_value
    })
    return(sampled_values)
  }
  
  # Sample n_sets parameter sets
  sampled_params_list <- replicate(n_samples, sample_one_set(1), simplify = FALSE)
  
  # Convert the list to a data frame
  sampled_params <- do.call(rbind, sampled_params_list)
  colnames(sampled_params) <- param_names
  
  return(as.data.frame(sampled_params))
}

# Parameters names
param_names <- c("BETA", "Q10_RH", "NPP_FLUX0", "AERO_SCALE", "DIFFUSIVITY", "ECS")

# sample size 
n_samples <- 1000

# Example params_weights data frame (to be replaced with your actual data)
# params_weights <- data.frame(
#   BETA = runif(100),
#   Q10_RH = runif(100),
#   NPP_FLUX0 = runif(100),
#   AERO_SCALE = runif(100),
#   DIFFUSIVITY = runif(100),
#   ECS = runif(100),
#   prob_weight = runif(100)
# )

# Call the function to sample parameters
sampled_params <- sample_parameters(params_weights, param_names, n_samples)

# Display the sampled parameters
head(sampled_params)


################

# split params into chunks
param_chunks <- split(sampled_params, 1:100)

# initializing a cluster
cl <- makeCluster(detectCores() - 1)

# Export required functions and objects to the cl cluster we just created
clusterExport(cl, c("param_chunks",
                    "ini_list",
                    "newcore",
                    "iterate_model"))

# run the model with parallel computing
new_sample_result <- parLapply(cl, names(ini_list), function(scenario_name){
  
  # extract the scenario information from the ini_list 
  # using the scenario name
  scenario <- ini_list[[scenario_name]]
  
  # initialize model core for the current scenario
  core <- newcore(scenario, name = scenario_name)
  
  # run the model looping across param_chunks for the current core
  result_list <- lapply(param_chunks, function(chunk) {
    
    iterate_model(core = core, 
                  params = chunk, 
                  save_years = 1800:2100,
                  save_vars = c("global_tas", "gmst", "ocean_uptake", "CO2_concentration"))
  })
  
  ## This step ensures a correct run_numbers are added to each model run ##
  # Starting with the second data frame of the current scenario
  for (i in 2:length(result_list)) {
    
    # calculate the max value of the previous element in the result list
    max_run_number <- max(result_list[[i - 1]]$run_number)
    
    # Add the max value of the previous element to the run_number of the current 
    # element to get an updated run_number that is continuous from the previous element.
    result_list[[i]]$run_number <- result_list[[i]]$run_number + max_run_number
    
  }
  
  # bind 
  result <- do.call(rbind, result_list)
  
  return(result)
  
})

#close cluster
stopCluster(cl)

# apply scenario names to list elements
names(new_sample_result) <- c("SSP1-1.9", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5")

# save data 
#saveRDS(result, "data/result-10k-run.RDS")

# normalize warming values to reference period
new_warming_data <- lapply(new_sample_result, function(df){
  
  subset(df,
         variable == "global_tas" 
         & year > 1849 
         & year < 2101)
  
})

new_result_95_to_14 <- lapply(new_warming_data, function(df){
  
  # Filter data for the reference period
  reference_period <- subset(df,
                             year > 1994 &
                               year < 2015
  )
  
  # Calculate the mean values of reference period
  mean_reference_period <- mean(reference_period$value)
  
  # Calculate normalized values for each year in the data set
  ## subtract data values by reference period mean
  normalized_values <- df$value - mean_reference_period
  
  # adding this column to each df
  df$value <- normalized_values
  
  return(df)
  
})


# bind results to create a data frame
new_sample_result_df <- do.call(rbind, new_result_95_to_14)
new_sample_result_df$weight <- NA
row.names(new_sample_result_df) <- NULL


## compare plots 
library(ggplot2)

# original result
ggplot(data = new_sample_result_df)) +
  geom_line(
    aes(x = year, 
        y = value, 
        group = run_number)) +
  facet_wrap(~scenario, scales = "free_y")
