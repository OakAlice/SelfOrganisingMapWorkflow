# Functions to test multiple shapes to find the optimal shape for SOM
# Adapted from Gaschk et al., 2023
  
# run the actual SOM tests combining many of the functions above
run_som_tests <- function(trDat, tstDat, file_path) {
  
  som_shapes <- generate_shapes() # Generate a list of SOM shapes to test
  AllIterationsResults <- data.frame() # Initialize an empty dataframe to store results
  
  for (shape_index in 1:nrow(som_shapes)) { # Loop through the different shapes
      print(shape_index)
    for (iter in 1:1) { # Run multiple iterations for each shape
      print(iter)
      iteration_results <- testing_the_SOM(trDat, tstDat, som_shapes[shape_index, 1], som_shapes[shape_index, 2])
      iteration_results$Width <- som_shapes[shape_index, 1] # Add width to the results
      iteration_results$Height <- som_shapes[shape_index, 2] # Add height to the results
      AllIterationsResults <- rbind(AllIterationsResults, iteration_results) # Combine iteration results
    }
  }
  
  # Calculate mean accuracy for each width-height combination
  mean_accuracy_scores <- AllIterationsResults %>%
    group_by(Width, Height) %>%
    summarize(mean_accuracy = mean(ACCU), .groups = "drop")
  
  # Determine the shape with the highest average accuracy
  best_shape <- mean_accuracy_scores[which.max(mean_accuracy_scores$mean_accuracy), ]
  best_width <- best_shape$Width
  best_height <- best_shape$Height
  
  # Return the best shape information
  return("best_shape" = best_shape)
}


# choose the best shape
determine_best_shape <- function(average_accuracies) {
  best_shape <- average_accuracies[which.max(average_accuracies$mean_acc), ]
  return(list("width" = best_shape$width, "height" = best_shape$height))
}

testing_the_SOM <- function(trDat, tstDat, width, height) {
  
  statisticsTable <- data.frame()
  
  # build the som using the training data
  ssom <- supersom(trDat, grid = somgrid(width, height, "hexagonal"), whatmap = c("measurements", "activity"))
  
  # predict on the testing data
  tryCatch({
    ssom.pred <- predict(ssom, newdata = tstDat, whatmap = "measurements")
  }, error = function(e) {
    if (grepl("Number of columns of newdata do not match codebook vectors", e$message)) {
      print("Error encountered with mismatched columns. Skipping this iteration.")
      return(invisible())  # Use return() to exit the function
    } else {
      stop(e)  # If it's a different error, continue with the error propagation
    }
  })
  
  # save the results as a table
  resultsTable <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  # use table to make statistics for understanding the model performance
  true_positives  <- diag(resultsTable)
  false_positives <- rowSums(resultsTable) - true_positives
  false_negatives <- colSums(resultsTable) - true_positives
  true_negatives  <- rep(sum(resultsTable), length(true_positives)) - rowSums(resultsTable) - colSums(resultsTable) + true_positives
  
  SENS <- true_positives / (true_positives + false_negatives)
  PREC <- true_positives / (true_positives + false_positives)
  SPEC <- true_negatives / (true_negatives + false_positives)
  ACCU <- sum(true_positives) / sum(resultsTable)
  
  # Prepare the statistics data frame
  statisticsTable <- as.data.frame(rbind(SENS, PREC, SPEC, ACCU=ACCU))
  
  # Add 'width' and 'height' to the data frame
  statisticsTable$Width <- width
  statisticsTable$Height <- height
  
  # save as a table that gives the results, size, and time it took to compute
  overallResultsTable <- cbind(Test=rownames(statisticsTable), statisticsTable)
  
  # return that data frame
  return(overallResultsTable)
}


# generate shapes to test
generate_shapes <- function() {
  somsize <- rep(seq(6,9,1),4) # Create some widths
  somsize2 <- rep(6:9, times=1, each=4) # Create some lengths
  somsize3 <- cbind(somsize, somsize2) # Combine the sizes
  return(somsize3)
}

# generate an accuracy heatmap matrix
generate_accuracy_score <- function(AllIterationsResults, selectedBehaviours) {
  # Extract the accuracy scores for each behavior
  accuracy_scores <- subset(AllIterationsResults, Test == 'ACCU')
  
  # Make sure we're not trying to gather more columns than exist
  max_col_index <- min(ncol(accuracy_scores), length(selectedBehaviours) + 1) # +1 for the 'test' column
  
  # Rearrange to be a long dataframe
  long_accuracy_scores <- accuracy_scores %>%
    pivot_longer(
      cols = -c(Test, Width, Height),  # Exclude Test, Width, and Height from the gathering to long format
      names_to = "Behaviour",
      values_to = "Accuracy"
    ) %>%
    select(Behaviour, Accuracy, Width, Height)
  
  # Calculate mean accuracy scores
  mean_accuracy_scores <- long_accuracy_scores %>%
    group_by(Width, Height) %>%
    summarise(mean_accuracy = mean(Accuracy, na.rm = TRUE), .groups = "drop")
  
  
  # Return the mean accuracy scores
  return(mean_accuracy_scores)
}

# Function to create and save an accuracy heatmap
create_heatmap <- function(average_accuracies, file_path) {
  # Create a matrix
  df2 <- with(average_accuracies, tapply(mean_acc, list(shape = width, height), FUN= mean, na.rm=TRUE))
  
  # Define custom color map
  colours_heat3 = c('#F4E119', '#F7C93B', '#C4BB5F', '#87BE76', '#59BD87', '#2CB6A0', '#00AAC1', '#1B8DCD', '#3D56A6', '#3A449C')
  
  # Save the heatmap to a temporary location (because % wont work in the filepath here)
  png("heatmap_temp_save.png")
  
  # Create the heatmap using Lattice plot
  heatmap <- levelplot(t(df2), cex.axis=1.0, cex.lab=1.0, col.regions=colorRampPalette(rev(colours_heat3)), 
                       screen = list(z = -90, x = -60, y = 0),
                       xlab=list(label='height', cex = 1.0),
                       ylab=list(label='width', cex = 1.0),
                       main=list(label= paste0('ACCU'), cex=1.0), 
                       colorkey=list(labels=list(cex=1.0)),
                       scales = list(cex=1.0),
                       asp=1)
  print(heatmap)
  
  # Finish saving to PNG file
  dev.off()
  
  # Now, move the heatmap from the temporary location to your desired location
  # Use fs::file_move() to rename/move the file
  fs::file_move("heatmap_temp_save.png", paste0(file_path, "/heatmap.png"))
  
  return(heatmap)
}
