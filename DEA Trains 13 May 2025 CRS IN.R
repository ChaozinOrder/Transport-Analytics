library(Benchmarking)

input_cols <- c(
  "Duration (minutes)",
  "Distance (km)",
  "Number of Halts",
  "Total Halt Time (minutes)",
  "Average Speed (km/h)",
  "Average Elevation (m)",      
  "CAP1A", "CAP2A", "CAP2S", "CAP3A", "CAP3E", "CAPCC", "CAPEC", "CAPSL", "CAPFC",  
  "Train Type.1",    
  "Travel Days",
  "Zone of Destination.1",
  "Weekend/Day",           
  "Night/Day-Dep",         
  "Night/Day-Arr"          
)

#"CAPTotal", 
#, "OCCTotal"

output_cols <- c(
  "OCC1A", "OCC2A", "OCC2S", "OCC3A", "OCC3E", "OCCCC", "OCCEC", "OCCSL", "OCCFC")

if (!all(input_cols %in% names(DEARaw))) {
  missing_inputs <- setdiff(input_cols, names(DEARaw))
  stop(paste("Error: Not all specified input columns found in DEARaw. Missing:", paste(missing_inputs, collapse = ", ")))
}
if (!all(output_cols %in% names(DEARaw))) {
  missing_outputs <- setdiff(output_cols, names(DEARaw))
  stop(paste("Error: Not all specified output columns found in DEARaw. Missing:", paste(missing_outputs, collapse = ", ")))
}
if (!"Train Number" %in% names(DEARaw)) {
  stop("Error: 'Train Number' column not found in DEARaw. Cannot set row names.")
}
if (any(is.na(DEARaw[, input_cols])) || any(DEARaw[, input_cols] < 0)) {
  stop("Error: Missing values (NA) or negative values found in input columns. Please handle them before running DEA.")
}
if (any(is.na(DEARaw[, output_cols])) || any(DEARaw[, output_cols] < 0)) {
  stop("Error: Missing values (NA) or negative values found in output columns. Please handle them before running DEA.")
}

X <- as.matrix(DEARaw[, input_cols])
Y <- as.matrix(DEARaw[, output_cols])

row.names(X) <- DEARaw$"Train Number" 
row.names(Y) <- DEARaw$"Train Number"

# --- Perform DEA ---

print("Performing DEA with CRS (Input-Oriented)...")
crs_io_result <- dea(X, Y, RTS = "CRS", ORIENTATION = "in", SLACK = TRUE)

# --- Analyze and Access Results ---

crs_efficiency_scores <- crs_io_result$eff

print("\nDEA Results:")
print("CRS Input-Oriented Efficiency Scores (first 10 DMUs):")
print(head(crs_efficiency_scores, 266)) 

DEARaw$CRS_Efficiency_IO <- crs_efficiency_scores

output_csv_path <- "./dea_scores.csv"

write.csv(DEARaw, file = output_csv_path, row.names = FALSE)

print(paste("DEARaw data frame exported successfully to:", output_csv_path))

# Summary of results
#summary(crs_io_result)

#crs_io_result$slack
#peers(crs_io_result)