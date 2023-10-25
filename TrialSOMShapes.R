# Script to test multiple shapes to find the optimal shape for SOM
# Adapted from Gaschk et al., 2023

library(pacman)
p_load(parallel, here, dplyr, tidyverse, kohonen, RColorBrewer, data.table, sentimentr, lattice, glue, parallel, foreach, doParallel, moments)

setwd(here())


#### FUNCTIONS ####

# run the actual SOM tests combining many of the functions above
run_som_tests <- function(TrDat, TstDat) {
  
  results <- list()
  shape_accuracy_score <- list() # to store accuracy scores for each iteration
  somsize3 = generate_shapes()
  
  AllIterationsResults <- data.frame() # initialize an empty dataframe outside the loop
  
  for (bb in 1:36) { # loop through the 36 different shapes
    
    for (iter in 1:10) { # Go through the 10 iterations for each shape
      iteration_results <- testing_the_SOM(TrDat, TstDat, somsize3[bb,1], somsize3[bb,2])
      AllIterationsResults <- rbind(AllIterationsResults, iteration_results)
    }
    
    shape_accuracy_score[[bb]] <- generate_accuracy_score(AllIterationsResults)
    results[[bb]] <- list("acc3list" = AllIterationsResults)
  }
  
  # Combine all accuracy scores
  all_shape_averages <- do.call(rbind, shape_accuracy_score)
  
  # Calculate mean accuracy for each width-height combination 
  average_accuracies <- all_shape_averages %>%
    group_by(width, height) %>%
    summarize(mean_acc = mean(acc), .groups = "drop")
  
  # make the heatmap using the average_accuracies
  heatmap <- create_heatmap(average_accuracies)
  
  best_shape <- determine_best_shape(average_accuracies)
  best_width <- best_shape$width
  best_height <- best_shape$height
  
  # Add the heatmap to the results list
  results[["average_heatmap"]] <- heatmap
  
  # Return the results, including the best shapes 
  return(list("best_width" = best_width, "best_height" = best_height))
}

# choose the best shape
determine_best_shape <- function(average_accuracies) {
  best_shape <- average_accuracies[which.max(average_accuracies$mean_acc), ]
  return(list("width" = best_shape$width, "height" = best_shape$height))
}

# the actual test that's performed on the training/testing data, returns overallResultsTable
testing_the_SOM <- function(TrDat, TstDat, width, height) {  # originally doSOMperf
  
  # build the som using the training data
  ssom <- supersom(TrDat, grid = somgrid(width, height, "hexagonal"))
  # predict on the testing data
  ssom.pred <- predict(ssom, newdata = TstDat)
  # save the results as a table
  resultsTable <- table(predictions = ssom.pred$predictions$act, act = TstDat$act)
  
  # use table to make statistics for understanding the model performance
  true_positives  <- diag(resultsTable)
  false_positives <- rowSums(resultsTable) - true_positives
  false_negatives <- colSums(resultsTable) - true_positives
  true_negatives  <- sum(resultsTable) - true_positives - false_positives - false_negatives
  SENS<-c(true_positives/(true_positives+false_negatives), shape=width)
  PREC<-c(true_positives/(true_positives+false_positives), shape=width)
  SPEC<-c(true_negatives/(true_negatives+false_positives), shape=width)
  ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives), shape=width)
  
  # save the statistics 
  statisticsTable <- as.data.frame(rbind(SENS,PREC,SPEC,ACCU))
  # save as a table that gives the results, size, and time it took to compute
  overallResultsTable <- cbind(test = rownames(statisticsTable), statisticsTable, width=width, height=height)
  # return that dataframe
  return(overallResultsTable)
}

# generate shapes to test
generate_shapes <- function() {
  somsize <- rep(seq(4,9,1),6) # Create some widths
  somsize2 <- rep(4:9, times=1, each=6) # Create some lengths
  somsize3 <- cbind(somsize, somsize2) # Combine the sizes
  return(somsize3)
}

# generate an accuracy heatmap matrix
generate_accuracy_score <- function(AllIterationsResults) {
  accuracy_scores <- subset(AllIterationsResults, test=='ACCU') # extract the accuracy scores for each
  # rearrange to be a long dataframe
  long_accuracy_scores <- accuracy_scores %>%
    gather(key = "behavior", value = "accuracy", 
           `Carrying object`:`Walking`) %>%
            select(behavior, accuracy, width, height)
  mean_accuracy_scores <- long_accuracy_scores %>%
    group_by(width, height) %>%
    summarise(mean_accuracy = mean(accuracy), .groups = "drop")
  
  shape_accuracy_score <- data.frame(acc=mean_accuracy_scores$mean_accuracy, width=mean_accuracy_scores$width, height=mean_accuracy_scores$height)
  
  return(shape_accuracy_score)
}

# Function to create and save an accuracy heatmap
create_heatmap <- function(average_accuracies, filename = "heatmap.png") {
  # Create a matrix
  df2 <- with(average_accuracies, tapply(mean_acc, list(shape = width, height), FUN= mean, na.rm=TRUE))
  
  # Define custom color map
  colours_heat3 = c('#F4E119', '#F7C93B', '#C4BB5F', '#87BE76', '#59BD87', '#2CB6A0', '#00AAC1', '#1B8DCD', '#3D56A6', '#3A449C')
  
  # Start saving to PNG file
  png(filename)
  
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
  
  return(heatmap)
}


#### INPUT: Names of everything ####
tests <- c("Condition1Random", "Condition2Random", "Condition1Chron", "Condition2Chron")

savepaths <- list("Condition1Random" = "Condition1/Random", "Condition2Random" = "Condition2/Random",
                  "Condition1Chron" = "Condition1/Chronological", "Condition2Chron" = "Condition2/Chronological")

#### INPUT: Execution of the script ####
results_list <- list()

for (test in tests) {
  
  #test = "Condition2andom"
  
  setwd(savepaths[[test]]) # set the working directory to our spot
  
  load("TrDat.rda")
  load("TstDat.rda")
  
  # Run the test 10 times and select the optimal shape
  optimal_dimensions <- run_som_tests(trDat, tstDat)
  
  print(paste(test[1], ": width", optimal_dimensions$best_width, "height", optimal_dimensions$best_height), sep = " ")

  setwd(here()) # reset the working directory before next loop
}