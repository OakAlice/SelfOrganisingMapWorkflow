# Given the window sizes, overlaps, and list of features specified on the main page
# process the data by that specification

source("VariableEntry.R")

# compute features based on the features list provided
compute_features <- function(window_chunk, featuresList) {
  
  # Determine the available axes from the dataset
  all_axes <- c("X_accel", "Y_accel", "Z_accel", "X_gyro", "Y_gyro", "Z_gyro")
  available_axes <- intersect(colnames(window_chunk), all_axes) # the ones we actually have
  
  result <- data.frame(row.names = 1)

  for (axis in available_axes) {
    
    # axis = "X_accel"
    
    if ("mean" %in% featuresList) {
      result[paste0("mean_", axis)] <- mean(window_chunk[[axis]])
    }
    
    if ("max" %in% featuresList) {
      result[paste0("max_", axis)] <- max(window_chunk[[axis]])
    }
    
    if ("min" %in% featuresList) {
      result[paste0("min_", axis)] <- min(window_chunk[[axis]])
    }
    
    if ("sd" %in% featuresList) {
      result[paste0("sd_", axis)] <- sd(window_chunk[[axis]])
    }
  }
  
  accel_axes <- intersect(available_axes, c("X_accel", "Y_accel", "Z_accel"))
  
  if (length(accel_axes) > 0 && ("SMA" %in% featuresList)) {
    result$SMA <- sum(rowSums(abs(window_chunk[, accel_axes]))) / nrow(window_chunk)
  }
  
  if (length(accel_axes) > 0 && ("minODBA" %in% featuresList || "maxODBA" %in% featuresList)) {
    ODBA <- rowSums(abs(window_chunk[, accel_axes]))
    result$minODBA <- min(ODBA)
    result$maxODBA <- max(ODBA)
  }
  
  if (length(accel_axes) > 0 && ("minVDBA" %in% featuresList || "maxVDBA" %in% featuresList)) {
    VDBA <- sqrt(rowSums(window_chunk[, accel_axes]^2))
    result$minVDBA <- min(VDBA)
    result$maxVDBA <- max(VDBA)
  }
  
  if ("cor" %in% featuresList) {
    for (i in 1:(length(accel_axes) - 1)) {
      for (j in (i+1):length(accel_axes)) {
        axis1 <- accel_axes[i]
        axis2 <- accel_axes[j]
        result[paste0("cor_", axis1, "_", axis2)] <- cor(window_chunk[[axis1]], window_chunk[[axis2]], use="complete.obs")
      }
    }
  }
  
  result$activity <- names(which.max(table(window_chunk$activity)))
  
  return(result)
}

process_data <- function(MoveData, featuresList, window, overlap) {
  
  # Initialize an empty list to store the processed data chunks
  processed_windows <- list()
  
  # Update starting and ending points for the next chunk
  window_samples = window * desired_Hz
  
  # Define the starting and ending points for the chunks
  st <- 1
  fn <- window_samples
  
  # Iterate over the chunks of data
  while (fn <= nrow(MoveData)) {
    
    # Extract the current chunk
    window_chunk <- MoveData[st:fn, ]
    
    # Compute features for the chunk
    features_data <- compute_features(window_chunk, featuresList)
    
    # Add the processed chunk to the list
    processed_windows <- c(processed_windows, list(features_data))
    
    if (overlap == 0) { # if no overlap, advance by a full window length
      st <- st + window_samples
      fn <- fn + window_samples
    } else { # if there is some overlap, calculate it
      overlapped_samples <- (overlap / 100) * window_samples
      st <- st + (window_samples - overlapped_samples)
      fn <- fn + (window_samples - overlapped_samples)
    }
    
    
  }
  
  # Combine all the processed chunks into a single data frame
  processed_data <- do.call(rbind, processed_windows)
  
  return(processed_data)
}
