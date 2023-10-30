## General Page for functions not specifically for any task

# Function to subset and rename columns to match the general format
subset_and_rename <- function(df, column_map) {
  # Check if all columns in the mapping exist in the dataframe
  if (all(names(column_map) %in% colnames(df))) {
    # Subset the dataframe
    df <- df[, names(column_map)]
    
    # Rename the columns
    colnames(df) <- column_map
    
    return(df)
  } else {
    stop("Some columns from the mapping are missing in the dataframe.")
  }
}

# Function for ensuring directory exists or creating it if not
ensure_dir <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, showWarnings = FALSE)
  }
}

# plot a png and then move it to the right directory --> this was a problem for some of my png images
plot_and_move <- function(filename, file_path, plot_expr) {
  png(filename = filename)
  eval(plot_expr)
  dev.off()
  fs::file_move(filename, file.path(file_path, filename))
}


# Find all instances of a particualr file
find_all_instances <- function(root_dir, filename) {
  list.files(root_dir, pattern = filename, full.names = TRUE, recursive = TRUE)
}


# Display all the parameters
display_experiment_params <- function(
    ExperimentNumber, MovementData, test_individuals, current_Hz, desired_Hz,
    columnSubset, selectedBehaviours, featuresList, trainingPercentage, 
    threshold, window, overlap, splitMethod, data_presentations
) {
  # Create the wide data frame
  experiment_params <- data.frame(
    ExperimentNumber = ExperimentNumber,
    MovementData = MovementData,
    TestIndividuals = test_individuals,
    CurrentHz = current_Hz,
    DesiredHz = desired_Hz,
    Columns = paste(names(columnSubset), collapse = ", "),
    SelectedBehaviours = paste(selectedBehaviours, collapse = ", "),
    FeaturesList = paste(featuresList, collapse = ", "),
    TrainingPercentage = trainingPercentage,
    Threshold = threshold,
    Window = window,
    Overlap = paste(overlap, collapse = ", "),
    SplitMethod = paste(splitMethod, collapse = ", "), 
    data_presentations = paste(data_presentations, collapse = ",")
  )
  
  # Save table
  write.csv(experiment_params, paste0("Experiment_", ExperimentNumber, "/Experiment_params.csv"))
  return(experiment_params)
}
