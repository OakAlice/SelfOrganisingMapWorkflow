# Function for reformatting the data

format_movement_data <- function(MovementData, columnSubset, test_individuals = NULL, desired_Hz = NULL, current_Hz = NULL, selectedBehaviours, ExperimentNumber) {

  # select and rename the relevant columns
  MoveData <- subset_and_rename(MoveData0, columnSubset)
  
  # only select the test individuals
  if (!is.null(test_individuals)) {
    selected_ids <- unique(MoveData$ID)[1:test_individuals]
    MoveData <- subset(MoveData, ID %in% selected_ids)
  }
  
  # format time
  # This part is commented out because it's specific to certain studies.
  # MoveData$time <- as.POSIXct((MoveData$time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
  
  # potentially downsample the data
  if (!is.null(desired_Hz) && !is.null(current_Hz)) {
    if (desired_Hz < current_Hz) {
      skip <- current_Hz / desired_Hz
      MoveData <- MoveData[seq(1, nrow(MoveData), by = skip), ]
    } else if (desired_Hz > current_Hz) {
      message("desired_Hz is higher than the current_Hz. Cannot upsample.")
    }
    # If desired_Hz == current_Hz, no action is needed
  } else {
    message("Desired_Hz and current_Hz not both defined.")
  }
  
  # select only the chosen behaviours
  MoveData <- MoveData[MoveData$activity %in% selectedBehaviours, ]
  
  # create directory if it doesn't exist
  output_dir <- paste0('Experiment_', ExperimentNumber)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir)
  }
  
  # save the output
  write.csv(MoveData, paste0(output_dir, '/Formatted_MoveData.csv'))
  
  # Optionally, return the data
  return(MoveData)
}