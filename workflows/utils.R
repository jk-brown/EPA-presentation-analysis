score_sensitivity <- function(result_df, criterion_weights = NULL, mc_weights_name = "mc_weights"){
  
  # Scoring with temperature
  scores_temp = score_runs(result_df,
                           criterion_gmst_obs(),
                           score_bayesian)
  
  # Scoring with co2 
  scores_co2 = score_runs(result_df,
                          criterion_co2_obs(), 
                          score_bayesian)
  
  # Scoring with ocean_c_uptake
  scores_ocean_c = score_runs(result_df,
                              criterion_ocean_uptake,
                              score_bayesian)
  
  # creating score_list
  score_list = list(scores_temp, scores_co2, scores_ocean_c)
  
  # computing criterion weights
  if (is.null(criterion_weights)) {
    mc_weights = multi_criteria_weighting(score_list, criterion_weights = criterion_weights)
  } else {
    mc_weights = multi_criteria_weighting(score_list)
  }
  
  # merge with data frame
  result_scored_df <- merge(result_df, scores_temp, by = "run_number")
  result_scored_df <- merge(result_scored_df, scores_co2, by = "run_number")
  result_scored_df <- merge(result_scored_df, scores_ocean_c, by = "run_number") 
  result_scored_df <- merge(result_scored_df, mc_weights, by = "run_number")
  
  # rename mc_weights column 
  names(result_scored_df)[names(result_scored_df) == "mc_weight"] <- mc_weights_name
  names(result_scored_df)[names(result_scored_df) == "weights.x"] <- "temp_score"
  names(result_scored_df)[names(result_scored_df) == "weights.y"] <- "co2_score"
  names(result_scored_df)[names(result_scored_df) == "weights"] <- "ocean_c_score"
  
  return(result_scored_df)        
}


# Write function to normalize Matilda data to reference period
normalize_temperature <- function(data, reference_start_year, reference_end_year) {
  # Filter data for the reference period
  reference_period <- subset(
    data,
    year >= reference_start_year &
      year <= reference_end_year
  )
  
  # Calculate the mean values of reference period
  mean_reference_period <- mean(reference_period$value)
  
  # Calculate normalized values for each year in the data set
  ## subtract data values by reference period mean
  normalized_values <- data$value - mean_reference_period
  
  # Create a new data frame with the normalized data
  normalized_data <- data.frame(
    year = data$year,
    adjusted_value = normalized_values
  )
  
  return(normalized_data)
}
