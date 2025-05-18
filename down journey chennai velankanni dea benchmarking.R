# R Code using Benchmarking Package for Year-wise DEA

# --- 1. Install and Load Packages ---
# Install necessary packages if you haven't already
# install.packages("dplyr")
# install.packages("lubridate")
#install.packages("Benchmarking") # Install the Benchmarking package

# Load the libraries
library(dplyr)
library(lubridate)
library(Benchmarking) # Load the Benchmarking library


# Output file settings for the CSV results
# Use a placeholder for the city pair name - you will set this below
RESULTS_CSV_FILE_TEMPLATE <- "dea_efficiency_down_results_%s.csv" # %s will be replaced by city pair name
# --- End Configuration ---

# --- IMPORTANT: Set the City Pair Name and File Path for THIS run ---
# You need to change these two lines for each city pair you process

current_city_pair_name <- "Chennai-Velankanni" # <--- CHANGE THIS for each city pair
file_path <- 'chennaivelankannifinal2.csv'     # <--- CHANGE THIS for each city pair file
# --- End IMPORTANT Section ---


# Construct the output CSV file name based on the city pair name
output_csv_file <- sprintf(RESULTS_CSV_FILE_TEMPLATE, gsub("[^a-zA-Z0-9_.-]", "_", current_city_pair_name))
# The gsub cleans up the name for use in a filename 

# Plot output settings
PLOT_FILE_NAME <- "chennaivelankannidea_down.png" # Name of the file to save the plot
PLOT_DPI <- 300 # Resolution (dots per inch)
PLOT_WIDTH <- 8 # Width in inches
PLOT_HEIGHT <- 6 # Height in inches
# --- End Configuration ---

# --- 2. Load Data ---
file_path <- 'chennaivelankannifinal2.csv'

# Check if file exists
if (!file.exists(file_path)) {
  stop(paste("Error: File not found at", file_path))
}

df <- read.csv(file_path)

# --- 3. Data Preparation and Aggregation (Your specific variables) ---

# Ensure DATE column is in date format and extract Year
if ("DATE" %in% colnames(df)) {
  df$DATE <- as.Date(df$DATE)
  df$Year <- year(df$DATE)
} else {
  stop("Error: 'DATE' column not found in the data frame.")
}

# Perform grouping and aggregation using base R aggregate
df_grouped_inputs <- aggregate(
  cbind(DN_PEAK_FREQUENCY, DN_OFF_FREQUENCY) ~ Year + CLASS,
  data = df,
  FUN = sum
)

df_grouped_output <- aggregate(
  cbind(`DN.OR`) ~ Year + CLASS,
  data = df,
  FUN = mean
)

df_grouped <- merge(df_grouped_inputs, df_grouped_output, by = c("Year", "CLASS"))

# --- FILTERING: Exclude Year 2025 ---
df_grouped <- df_grouped %>%
  filter(Year != 2025)
# --- END FILTERING ---

# Handle R's column naming (space to dot) for the aggregated data frame
selected_cols <- c("Year", "CLASS", "DN_PEAK_FREQUENCY", "DN_OFF_FREQUENCY", "DN.OR")
if (!all(selected_cols %in% colnames(df_grouped))) {
  missing <- setdiff(selected_cols, colnames(df_grouped))
  stop(paste("Error: Required columns missing after aggregation/merge:", paste(missing, collapse = ", ")))
}

df_grouped <- df_grouped %>%
  select(all_of(selected_cols))


print("Data grouped successfully based on your selected variables. Head of grouped data:")
print(head(df_grouped))


# Define input and output variable names (matching grouped data column names after R processing)
input_vars_r <- c('DN_PEAK_FREQUENCY', 'DN_OFF_FREQUENCY')
output_vars_r <- c('DN.OR') # R converts space to dot

# --- 4. Perform Year-wise DEA (Using Benchmarking Package) ---

# Benchmarking package can also benefit from handling zeros and scaling,
# but its sensitivity might be different. Let's start with zero handling
# and scaling by maximum as these were useful steps.

# Define fixed epsilon value for zero handling (used before scaling)
FIXED_EPSILON <- 0.00 # Or try 0.01

unique_years <- unique(df_grouped$Year)
efficiency_results_bm <- list() # Use a list to store Benchmarking results for THIS city pair

cat("\nStarting DEA analysis (Benchmarking Package) for", length(unique_years), "years...\n")

for (year in unique_years) {
  cat("\n--- Performing Benchmarking DEA for year:", year, "---\n")
  
  # Filter data for the current year
  df_year <- df_grouped %>% filter(Year == year)
  
  # --- ZERO HANDLING: Replace zero inputs with the FIXED epsilon ---
  if(all(input_vars_r %in% colnames(df_year))) {
    df_year[, input_vars_r] <- lapply(df_year[, input_vars_r, drop = FALSE], function(x) {
      replace(x, x == 0, FIXED_EPSILON)
    })
  } else {
    cat("Warning: Input columns not found for zero handling in year", year, ". Skipping zero handling.\n")
  }
  # --- END ZERO HANDLING ---
  
  # --- SCALING: Scale inputs and outputs by their MAXIMUM for the year ---
  # Benchmarking might also be sensitive to scale. Let's apply scaling by maximum.
  input_subset <- df_year[, input_vars_r, drop = FALSE] # Use data AFTER epsilon replacement
  output_subset <- df_year[, output_vars_r, drop = FALSE]
  
  input_maximums <- apply(input_subset, 2, max, na.rm = TRUE)
  output_maximums <- apply(output_subset, 2, max, na.rm = TRUE)
  
  input_maximums[input_maximums == 0] <- 1
  output_maximums[output_maximums == 0] <- 1
  
  df_year_scaled <- df_year
  df_year_scaled[, input_vars_r] <- sweep(df_year_scaled[, input_vars_r, drop = FALSE], 2, input_maximums, "/")
  df_year_scaled[, output_vars_r] <- sweep(df_year_scaled[, output_vars_r, drop = FALSE], 2, output_maximums, "/")
  
  # --- DEBUG PRINT: Inspect the data AFTER scaling ---
  # cat("Data frame for year", year, "AFTER scaling by maximum and fixed epsilon:\n")
  # print(df_year_scaled)
  # cat("\n")
  # --- END DEBUG PRINT ---
  # --- END SCALING ---
  
  
  # --- Validation for DEA ---
  num_dmus <- nrow(df_year_scaled) # Use count from scaled df
  num_inputs <- length(input_vars_r)
  num_outputs <- length(output_vars_r)
  min_dmus_vrs <- num_inputs + num_outputs + 1 # Heuristic
  
  if (num_dmus == 0) {
    cat("No data (DMUs) for year", year, ". Skipping.\n")
    next
  }
  if (num_inputs == 0 || num_outputs == 0) {
    cat("Error: No inputs (", num_inputs, ") or outputs (", num_outputs, ") defined. Check config. Skipping year", year, ".\n")
    next
  }
  
  if (num_dmus < min_dmus_vrs) {
    cat("Warning: Not enough DMUs (", num_dmus, ") for year", year, "to perform VRS DEA robustly (heuristic need is", min_dmus_vrs, "). Proceeding with Benchmarking.\n")
    # Proceeding with Benchmarking
  }
  
  required_cols_r <- c("CLASS", input_vars_r, output_vars_r)
  if (!all(required_cols_r %in% colnames(df_year_scaled))) { # Check against scaled df
    missing_cols <- setdiff(required_cols_r, colnames(df_year_scaled))
    cat("Error: Missing required columns for DEA in year", year, ":", paste(missing_cols, collapse = ", "), ". Skipping year.\n")
    next
  }
  
  # Prepare input and output matrices for Benchmarking package
  # Benchmarking::dea function often takes matrices directly
  inputs_matrix_bm <- as.matrix(df_year_scaled[, input_vars_r, drop = FALSE])
  outputs_matrix_bm <- as.matrix(df_year_scaled[, output_vars_r, drop = FALSE])
  
  # Get DMU names (CLASS) for this year from the scaled data frame
  dmu_names <- df_year_scaled$CLASS
  
  
  # --- Run DEA using Benchmarking::dea ---
  cat("Running Benchmarking VRS Output-oriented DEA for", num_dmus, "DMUs using scaled data...\n")
  
  tryCatch({
    # Benchmarking::dea(X, Y, RTS, ORIENTATION)
    # X is input matrix, Y is output matrix
    # RTS = "vrs", "crs", etc.
    # ORIENTATION = "out" or "in"
    
    # Setting print.info to FALSE might suppress solver messages if they are just warnings
    # The Benchmarking::dea function returns an object containing efficiency scores
    dea_model_bm <- Benchmarking::dea(
      X = inputs_matrix_bm,
      Y = outputs_matrix_bm,
      RTS = "vrs",
      ORIENTATION = "out" # Output-oriented as per your goal
      # , print.info = FALSE # Uncomment to suppress potential solver info
    )
    
    # --- Check if dea_model_bm is NULL or has efficiency scores ---
    # Benchmarking::dea returns an object with 'eff' as the efficiency scores
    if (is.null(dea_model_bm) || !("eff" %in% names(dea_model_bm))) {
      cat("Warning: Benchmarking DEA model did not return expected results (likely solver issue) for year", year, ".\n")
      # print(dea_model_bm) # Uncomment to see what the problematic object is
    } else {
      # Extract efficiency scores from the 'eff' component
      efficiency_scores_bm <- dea_model_bm$eff
      
      # Store results
      # Use dmu_names to set the names for the scores
      efficiency_results_bm[[as.character(year)]] <- setNames(efficiency_scores_bm, dmu_names)
      cat("Benchmarking DEA calculation successful.\n")
    }
    # --- END CHECK ---
    
    
  }, error = function(e) {
    cat("An error occurred DURING Benchmarking DEA calculation for year", year, ":", e$message, "\n")
  })
  
}

# --- 5. Present Results ---
cat("\n--- DEA Efficiency Scores (Year-wise, Benchmarking VRS Output-Oriented) ---\n")

if (length(efficiency_results_bm) == 0) {
  cat("No Benchmarking DEA results were calculated for any year. Check warnings and errors above.")
} else {
  sorted_years <- sort(as.numeric(names(efficiency_results_bm)))
  
  for (year_str in sorted_years) {
    year <- as.character(year_str)
    scores <- efficiency_results_bm[[year]]
    
    if (length(scores) > 0) {
      cat("\nYear:", year, "\n")
      sorted_scores_by_name <- scores[order(names(scores))]
      for (class_name in names(sorted_scores_by_name)) {
        score <- sorted_scores_by_name[class_name]
        if (!is.na(score) && is.finite(score)) {
          cat(" ", "CLASS", class_name, ":", sprintf("%.4f", score), "\n")
        } else {
          cat(" ", "CLASS", class_name, ": NaN (Calculation Error or Missing)\n")
        }
      }
    } else {
      cat("\nYear:", year, "\n  No DEA scores available for this year.")
    }
  }
}

# --- 5. Present Results (Benchmarking Package Output-Oriented) ---
cat("\n--- DEA Efficiency Scores (Year-wise, Benchmarking VRS Output-Oriented) ---\n")
cat("Note: Scores are 0-1 efficiency (1 = Efficient)\n") # Added note

if (length(efficiency_results_bm) == 0) {
  cat("No Benchmarking DEA results were calculated for any year. Check warnings and errors above.")
} else {
  sorted_years <- sort(as.numeric(names(efficiency_results_bm)))
  
  for (year_str in sorted_years) {
    year <- as.character(year_str)
    scores <- efficiency_results_bm[[year]] # These are the factors (>= 1)
    
    if (length(scores) > 0) {
      cat("\nYear:", year, "\n")
      
      # --- MODIFIED: Calculate the standard 0-1 efficiency score ---
      standard_efficiency_scores <- 1 / scores
      # --- END MODIFIED ---
      
      # Sort scores by class name for consistent output
      # Use the standard_efficiency_scores for sorting and printing
      sorted_scores_by_name <- standard_efficiency_scores[order(names(standard_efficiency_scores))]
      for (class_name in names(sorted_scores_by_name)) {
        score <- sorted_scores_by_name[class_name]
        if (!is.na(score) && is.finite(score)) {
          cat(" ", "CLASS", class_name, ":", sprintf("%.4f", score), "\n")
        } else {
          cat(" ", "CLASS", class_name, ": NaN (Calculation Error or Missing)\n")
        }
      }
    } else {
      cat("\nYear:", year, "\n  No DEA scores available for this year.")
    }
  }
}

# R Code for Plotting Benchmarking DEA Results (Time Series Line Plot)
# --- Ensure Packages are Loaded ---
# Make sure you have these libraries loaded if running separately
library(dplyr)
library(ggplot2)
library(tidyr)
# You also need the Benchmarking results object 'efficiency_results_bm' from the previous code

# --- Define the CLASS Label Mapping ---
class_label_mapping <- c(
  '0' = 'AC',
  '1' = 'ASL',
  '2' = 'ASS',
  '3' = 'NSL',
  '4' = 'NSS',
  '5' = 'UD',
  '6' = 'UDT'
)

# --- Prepare Results for Plotting ---

cat("\nPreparing DEA results for plotting...\n")

# Combine the list of results (efficiency_results_bm) into a single data frame
results_df_list <- lapply(names(efficiency_results_bm), function(year_str) {
  scores_vector <- efficiency_results_bm[[year_str]] # Get scores for the year (factors >= 1)
  
  # Calculate standard 0-1 efficiency scores (1 / factor)
  standard_efficiency_scores <- 1 / scores_vector
  
  data.frame(
    Year = as.numeric(year_str),
    CLASS_ID = names(standard_efficiency_scores),
    Efficiency = standard_efficiency_scores
  )
})

all_results_df <- bind_rows(results_df_list)

# Apply the class label mapping
all_results_df$CLASS_Name <- factor(all_results_df$CLASS_ID,
                                    levels = names(class_label_mapping),
                                    labels = class_label_mapping,
                                    ordered = FALSE)

if (any(is.na(all_results_df$CLASS_Name))) {
  warning("Some CLASS IDs found in data were not present in the class_label_mapping and are shown as NA.")
}

print("Combined and prepared data for plotting. Head:")
print(head(all_results_df))


# --- Generate and Save Plot ---

# Create the ggplot object
plot_efficiency_trends <- ggplot(all_results_df, aes(x = Year, y = Efficiency, color = CLASS_Name, group = CLASS_Name)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = unique(all_results_df$Year)) +
  scale_y_continuous(limits = c(0.5, 1), breaks = seq(0.5, 1, 0.05)) +
  labs(
    title = "Chennai - Velankanni", # Your specified title
    x = "Year",
    y = "Efficiency Score", # Your specified y-axis label
    color = "Bus Class"
  ) +
  theme_minimal() +
  theme(
    # Base font settings - Use default font
    text = element_text(size = 12), # Default font, size 12 for all text
    
    # Title specific settings
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5), # Default font, size 16, bold, centered
    
    # Axis text settings (will inherit from base text unless overridden)
    axis.text = element_text(size = 12),
    
    # Axis title settings (will inherit from base text unless overridden)
    axis.title = element_text(size = 12),
    
    # Legend text settings (will inherit from base text unless overridden)
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 12),
    
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# Save the plot to a file with high resolution
ggsave(
  PLOT_FILE_NAME,      # File name defined in configuration
  plot = plot_efficiency_trends, # The ggplot object to save
  width = PLOT_WIDTH,  # Width defined in configuration
  height = PLOT_HEIGHT, # Height defined in configuration
  dpi = PLOT_DPI       # DPI defined in configuration
)

cat("\nLine plot generated and saved as", PLOT_FILE_NAME, " with default font and specified formatting.\n")

# --- END OF PLOTTING CODE ---

# --- 5. Prepare and Save Results to CSV ---

cat("\n--- Preparing and Saving Results for", current_city_pair_name, "to CSV ---\n")

if (length(efficiency_results_bm) > 0) {
  # Combine the list of yearly results for this city pair into a data frame
  # Calculate the standard 0-1 efficiency score (1 / factor) here
  current_city_pair_results_df <- bind_rows(lapply(names(efficiency_results_bm), function(year_str) {
    scores_vector <- efficiency_results_bm[[year_str]] # Factors >= 1
    standard_efficiency_scores <- 1 / scores_vector # 0-1 scores
    
    data.frame(
      Year = as.numeric(year_str),
      CLASS_ID = names(standard_efficiency_scores),
      Efficiency = standard_efficiency_scores,
      CityPair = current_city_pair_name # Add the CityPair identifier
    )
  }))
  
  # Apply the class label mapping to the CLASS_ID column
  if (nrow(current_city_pair_results_df) > 0) {
    current_city_pair_results_df$CLASS_Name <- factor(current_city_pair_results_df$CLASS_ID,
                                                      levels = names(class_label_mapping),
                                                      labels = class_label_mapping,
                                                      ordered = FALSE)
    # Reorder columns for clarity
    current_city_pair_results_df <- current_city_pair_results_df %>%
      select(CityPair, Year, CLASS_ID, CLASS_Name, Efficiency)
    
    
    # Save the data frame to a CSV file
    write.csv(current_city_pair_results_df, file = output_csv_file, row.names = FALSE)
    
    cat("Results for", current_city_pair_name, "saved to", output_csv_file, "\n")
    print(head(current_city_pair_results_df)) # Print head of saved data
    
  } else {
    cat("No results calculated for", current_city_pair_name, ". No CSV file saved.\n")
  }
  
} else {
  cat("No DEA results were calculated for", current_city_pair_name, "for any year. No CSV file saved.\n")
}

# --- END OF CODE FOR ONE CITY PAIR + CSV SAVE ---