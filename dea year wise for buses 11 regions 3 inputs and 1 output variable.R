# Read the dataset
# Assuming the file is comma-separated. 
dea_data <- DEA_Input_Bus_Basic_May_5_2025

# Install and load the Benchmarking and necessary libraries
# install.packages("Benchmarking") # Uncomment if you haven't installed it
# install.packages("readr") # Uncomment if you haven't installed it
# install.packages("dplyr") # Uncomment if you haven't installed it
library(Benchmarking)
library(readr) # To read the CSV file
library(dplyr) # To use mutate

# Display the first few rows and structure of the data to verify
print(head(dea_data))
print(str(dea_data))

# Define the input and output variables
input_vars <- c("Trip Count", "Wait Time", "Time Gap")
output_vars <- c("OR")
year_var <- "Year"
dmu_vars <- c("Location 1", "Location 2", "Journey")

# Get the unique years in the dataset
years <- sort(unique(dea_data[[year_var]]))

# Perform DEA for each year and store results
dea_analysis_results_benchmarking <- list()

for (year in years) {
  cat("\nPerforming DEA using Benchmarking package for year:", year, "\n")
  
  # Filter data for the current year 
  data_year <- dea_data[dea_data[[year_var]] == year, ]
  
  # Create a unique DMU identifier for the current year
  data_year <- data_year %>%
    mutate(DMU = paste(!!sym(dmu_vars[1]), !!sym(dmu_vars[2]), !!sym(dmu_vars[3]), sep = "-"))
  
  # Check if there are enough DMUs and variables for DEA
  num_dmus <- nrow(data_year)
  num_inputs <- length(input_vars)
  num_outputs <- length(output_vars)
  
  if (num_dmus < (num_inputs * 3)) {
    cat("Warning: Not enough DMUs for year", year, "to perform a robust DEA analysis (need at least", num_inputs * 3, "have", num_dmus, "). Skipping this year.\n")
    dea_analysis_results_benchmarking[[as.character(year)]] <- "Skipped due to insufficient DMUs"
    next
  }
  
  
  # Select input and output matrices and set row names to DMU names
  inputs <- as.matrix(data_year[, input_vars])
  rownames(inputs) <- data_year$DMU
  outputs <- as.matrix(data_year[, output_vars])
  rownames(outputs) <- data_year$DMU
  
  
  # Perform DEA (input orientation, CRS)
  tryCatch({
    dea_model_benchmarking <- dea(X = inputs, Y = outputs,
                                  RTS = "crs",       # Corrected argument name
                                  ORIENTATION = "in") # Corrected argument name
    
    # Store the full model result
    dea_analysis_results_benchmarking[[as.character(year)]] <- dea_model_benchmarking
    
    # Print efficiency scores
    cat("\nEfficiency Scores for Year:", year, "\n")
    scores <- dea_model_benchmarking$eff
    print(scores)
    
    # Print Benchmarking Information (Peers/Lambdas and Targets)
    cat("\nBenchmarking Information (Peers and Targets) for Year:", year, "\n")
    
    # Get peers (the efficient DMUs forming the benchmark) and their weights (lambdas)
    lambda_matrix <- dea_model_benchmarking$lambda
    
    # Get targets (projected input values)
    input_targets <- dea_model_benchmarking$x_input
    
    cat("\nInput Targets (Efficient Input Levels):\n")
    print(input_targets)
    
    
    cat("\nPeers (Efficient Benchmarks) and their Weights (Lambdas):\n")
    # Iterate through each DMU to show its benchmarks
    for (i in 1:num_dmus) {
      dmu_name <- rownames(inputs)[i]
      cat("  DMU:", dmu_name, "\n")
      
      # Find non-zero lambdas for this DMU. These correspond to its peers.
      peer_weights <- lambda_matrix[i, ]
      # Find the names of the DMUs with non-zero weights
      active_peers_indices <- which(peer_weights > 1e-9) # Use a small tolerance for zero
      active_peer_names <- rownames(lambda_matrix)[active_peers_indices]
      
      if (length(active_peer_names) > 0) {
        cat("    Benchmarks (Efficient Peers) and Weights:\n")
        # Filter to show only truly efficient peers
        efficient_peers_weights <- peer_weights[active_peers_indices]
        names(efficient_peers_weights) <- active_peer_names
        
        # Check which of these active peers are actually efficient based on their scores
        truly_efficient_peers <- names(efficient_peers_weights)[scores[names(efficient_peers_weights)] == 1]
        
        if(length(truly_efficient_peers) > 0){
          for (peer_name in truly_efficient_peers) {
            cat("      -", peer_name, "(Weight:", round(efficient_peers_weights[peer_name], 4), ")\n")
          }
        } else {
          # This case might happen if a DMU is weakly efficient and its projection
          # involves other weakly efficient or the DMU itself with a weight of 1
          if(scores[dmu_name] == 1){
            cat("    Is efficient (is its own benchmark).\n")
          } else {
            cat("    Could be weakly efficient, benchmarks might not be strictly on the efficient frontier.\n")
          }
        }
        
      } else {
        # If efficiency is 1, it's its own benchmark
        if (scores[dmu_name] == 1) {
          cat("    Is efficient (is its own benchmark).\n")
        } else {
          cat("    No specific efficient peers identified (may be weakly efficient or on a facet).\n")
        }
      }
    }
    
    
  }, error = function(e) {
    cat("Error performing DEA for year", year, ":", e$message, "\n")
    dea_analysis_results_benchmarking[[as.character(year)]] <- paste("Error:", e$message)
  })
}


# Save the list of DEA results for all years
saveRDS(dea_analysis_results_benchmarking, "dea_results_crs_input.rds")

loaded_results <- readRDS("dea_results_crs_input.rds")

# Create an empty data frame to store efficiency scores for all years
all_years_efficiency_scores <- data.frame(DMU = character(), Year = integer(), Efficiency = numeric(), stringsAsFactors = FALSE)

# Loop through the results and extract scores
for (year_str in names(dea_analysis_results_benchmarking)) {
  results_year <- dea_analysis_results_benchmarking[[year_str]]
  
  # Check if the analysis was successful for the year
  if (class(results_year) == "Farrell") {
    scores <- results_year$eff
    year_data <- data.frame(DMU = names(scores), Year = as.integer(year_str), Efficiency = scores, stringsAsFactors = FALSE)
    all_years_efficiency_scores <- bind_rows(all_years_efficiency_scores, year_data)
  }
}

# Save the combined efficiency scores to a CSV file
write_csv(all_years_efficiency_scores, "basic_bus_efficiency_scores_crs_input_2018_2024.csv")


