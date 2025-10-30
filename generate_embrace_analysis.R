#!/usr/bin/env Rscript
# EMBRACE Analysis Generation Script
# 
# This script creates subfolders and runs the main EMBRACE-II analysis functions:
# 1. emii_verify_all_events() - Event verification
# 2. emii_get_clean_data() - Both minimal and non-minimal versions
#
# Author: Generated for EMBRACE analysis
# Date: 2024

# Load required libraries
library(embraceR)
library(dplyr)
library(here)
library(glue)

# Set up logging
cat("=== EMBRACE Analysis Generation Script ===\n")
cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

# Create main output directory with timestamp
timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
main_output_dir <- glue("embrace_analysis_{timestamp}")
dir.create(main_output_dir, showWarnings = FALSE)
cat("Created main output directory:", main_output_dir, "\n")

# Create subdirectories
subdirs <- c(
  "01_event_verification",
  "02_clean_data_minimal", 
  "03_clean_data_full",
  "04_logs"
)

for (subdir in subdirs) {
  dir.create(file.path(main_output_dir, subdir), showWarnings = FALSE)
  cat("Created subdirectory:", file.path(main_output_dir, subdir), "\n")
}

# Function to run analysis with error handling
run_analysis <- function(func_name, func_call, output_dir, description) {
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  cat("Running:", description, "\n")
  cat("Function:", func_name, "\n")
  cat("Output directory:", output_dir, "\n")
  cat("Started at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
  
  # Change to output directory
  old_wd <- getwd()
  setwd(output_dir)
  
  tryCatch({
    # Run the function
    result <- eval(parse(text = func_call))
    
    cat("SUCCESS: Function completed successfully\n")
    cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Log success
    log_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      function_name = func_name,
      status = "SUCCESS",
      description = description,
      output_files = paste(list.files(".", pattern = "\\.(xlsx|xls)$"), collapse = ", ")
    )
    
    return(list(success = TRUE, result = result, log = log_entry))
    
  }, error = function(e) {
    cat("ERROR: Function failed\n")
    cat("Error message:", e$message, "\n")
    cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
    
    # Log error
    log_entry <- data.frame(
      timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      function_name = func_name,
      status = "ERROR",
      description = description,
      output_files = "",
      error_message = e$message
    )
    
    return(list(success = FALSE, result = NULL, log = log_entry))
  }, finally = {
    # Return to original directory
    setwd(old_wd)
  })
}

# Initialize results log
results_log <- data.frame()

# 1. Run Event Verification Analysis
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("STEP 1: EVENT VERIFICATION ANALYSIS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

event_verification_dir <- file.path(main_output_dir, "01_event_verification")
result1 <- run_analysis(
  func_name = "emii_verify_all_events",
  func_call = "emii_verify_all_events(save_excel = TRUE)",
  output_dir = event_verification_dir,
  description = "Event verification analysis with Excel output"
)

results_log <- rbind(results_log, result1$log)

# 2. Run Clean Data Analysis - Minimal Version
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("STEP 2: CLEAN DATA ANALYSIS - MINIMAL VERSION\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

minimal_data_dir <- file.path(main_output_dir, "02_clean_data_minimal")
result2 <- run_analysis(
  func_name = "emii_get_clean_data",
  func_call = "emii_get_clean_data(minimal = TRUE, save_excel = TRUE)",
  output_dir = minimal_data_dir,
  description = "Clean data analysis - minimal version with Excel output"
)

results_log <- rbind(results_log, result2$log)

# 3. Run Clean Data Analysis - Full Version
cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("STEP 3: CLEAN DATA ANALYSIS - FULL VERSION\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

full_data_dir <- file.path(main_output_dir, "03_clean_data_full")
result3 <- run_analysis(
  func_name = "emii_get_clean_data",
  func_call = "emii_get_clean_data(minimal = FALSE, save_excel = TRUE)",
  output_dir = full_data_dir,
  description = "Clean data analysis - full version with Excel output"
)

results_log <- rbind(results_log, result3$log)

# Save results log
log_dir <- file.path(main_output_dir, "04_logs")
log_file <- file.path(log_dir, glue("analysis_log_{timestamp}.csv"))
write.csv(results_log, log_file, row.names = FALSE)
cat("\nAnalysis log saved to:", log_file, "\n")

# Create summary report
summary_file <- file.path(log_dir, glue("analysis_summary_{timestamp}.txt"))
sink(summary_file)

cat("EMBRACE Analysis Generation Summary\n")
cat("====================================\n")
cat("Generated on:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("Main output directory:", main_output_dir, "\n\n")

cat("Analysis Steps Completed:\n")
cat("------------------------\n")
for (i in 1:nrow(results_log)) {
  row <- results_log[i, ]
  cat(sprintf("%d. %s - %s\n", i, row$description, row$status))
  if (row$status == "SUCCESS" && row$output_files != "") {
    cat(sprintf("   Output files: %s\n", row$output_files))
  } else if (row$status == "ERROR") {
    cat(sprintf("   Error: %s\n", row$error_message))
  }
}

cat("\nDirectory Structure:\n")
cat("-------------------\n")
cat(main_output_dir, "\n")
for (subdir in subdirs) {
  cat("├──", subdir, "\n")
}

cat("\nTotal execution time:", format(Sys.time() - as.POSIXct(results_log$timestamp[1]), "%H:%M:%S"), "\n")

sink()

cat("\n", paste(rep("=", 60), collapse = ""), "\n")
cat("ANALYSIS COMPLETE\n")
cat(paste(rep("=", 60), collapse = ""), "\n")
cat("Summary report saved to:", summary_file, "\n")
cat("All results saved in:", main_output_dir, "\n")
cat("Finished at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")

# Print final status
successful_runs <- sum(results_log$status == "SUCCESS")
total_runs <- nrow(results_log)
cat(sprintf("\nFinal Status: %d/%d analyses completed successfully\n", successful_runs, total_runs))

if (successful_runs == total_runs) {
  cat("🎉 All analyses completed successfully!\n")
} else {
  cat("⚠️  Some analyses failed. Check the log for details.\n")
}