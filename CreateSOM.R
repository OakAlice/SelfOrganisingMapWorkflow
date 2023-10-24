# Now this will be the code that creates the SOM from the training and testing data we created
# It has two tasks: 1) Testing multiple shapes to find optimal. 2) Applying that optimal shape to a final SOM.
# Adapted from Gaschk et al., 2023

library(pacman)
p_load(parallel)

numCores <- detectCores()
registerDoParallel(numCores)

#### FUNCTIONS ####

# the actual test that's performed on the training/testing data, returns overallResultsTable
testing_the_SOM <- function(TrainingData, TestingData, width, height) {  # originally doSOMperf
  
  # build the som using the training data
  ssom <- supersom(TrainingData, grid = somgrid(width, height, "hexagonal"))
  # predict on the testing data
  ssom.pred <- predict(ssom, newdata = TestingData)
  # save the results as a table
  resultsTable <- table(predictions = ssom.pred$predictions$act, act = TestingData$act)
  
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
  overallResultsTable <- cbind(test = rownames(statisticsTable), statisticsTable, time=time_out[3], width=width, height=height)
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

# execute a test of the SOM
single_som_test <- function(TrainingData, TestingData, width, height) {
  AllIterationsResults <- foreach(i = 1:36, .packages=c('kohonen'), .combine=rbind) %dopar% {
    testing_the_SOM(TrainingData, TestingData, width, height)
  }
  return(AllIterationsResults)
}

# generate a heatmap matrix
generate_heatmap_matrix <- function(AllIterationsResults) {
  long_DF <- AllIterationsResults %>% gather(act, acc3, c("Lying chest", "Panting", "Playing", "Sitting", "Sniffing",       
                                              "Trotting", "Walking", "Shaking", "Eating",         
                                              "Pacing", "Standing", "Drinking", "Galloping", "Carrying object"))
  
  acc4 <- subset(long_DF, test=='ACCU') # extract the accuracy scores for each
  heatmap_matrix <- data.frame(acc=acc4$acc3, width=acc4$width, height=acc4$height)
  
  return(heatmap_matrix)
}

# Function to create and save an accuracy heatmap
create_heatmap <- function(average_heatmap_matrix, filename = "heatmap.png") {
  # Create a matrix
  df2 <- with(average_heatmap_matrix, tapply(acc, list(shape = width, height), FUN= mean, na.rm=TRUE))
  
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
  
  # Finish saving to PNG file
  dev.off()
  
  return(heatmap)
}

# determine the optimal shape
determine_best_shape <- function(average_heatmap_matrix) {
  best_shape <- average_heatmap_matrix[which.max(average_heatmap_matrix$mean_acc), ]
  return(list("width" = best_shape$width, "height" = best_shape$height))
}

# run the actual SOM tests combining many of the functions above
run_som_tests <- function(TrainingData, TestingData) {
  results <- list()
  heatmap_matrix_list <- list() # to store heatmap_matrix for each iteration
  somsize3 = generate_shapes()
  
  for (bb in 1:10) { # do it ten times
    acc3list <- single_som_test(TrainingData, TestingData, somsize3[bb,1], somsize3[bb,2])
    heatmap_matrix_list[[bb]] <- generate_heatmap_matrix(acc3list)
    results[[bb]] <- list("acc3list" = acc3list)
  }
  
  # Combine all heatmap matrices
  all_heatmap_matrices <- do.call(rbind, heatmap_matrix_list)
  
  # Calculate mean accuracy for each width-height combination
  average_heatmap_matrix <- all_heatmap_matrices %>%
    group_by(width, height) %>%
    summarize(mean_acc = mean(acc))
  
  # make the heatmap using the average_heatmap_matrix
  heatmap <- create_heatmap(average_heatmap_matrix)
  
  best_shape <- determine_best_shape(average_heatmap_matrix)
  most_common_width <- best_shape$width
  most_common_height <- best_shape$height
  
  # Add the heatmap to the results list
  results[["average_heatmap"]] <- heatmap
  
  # Return the results, including the best shapes 
  return(list("results" = results, "most_common_width" = most_common_width, 
              "most_common_height" = most_common_height, 
              "average_heatmap_matrix" = average_heatmap_matrix))
}

# for when you have determined which shape is the best, run the full version
performOptimalSOM <- function(TrainingData, TestingData, most_common_width, most_common_height) {
  time_out<-system.time(
    ssom <- supersom(TrainingData, grid = somgrid(most_common_width, most_common_height, "hexagonal"))
  )
  ssom.pred <- predict(ssom, newdata = TestingData)
  ptab <- table(predictions = ssom.pred$predictions$act, act = TestingData$act)
  
  true_positives  <- diag(ptab)
  false_positives <- rowSums(ptab) - true_positives
  false_negatives <- colSums(ptab) - true_positives
  true_negatives  <- sum(ptab) - true_positives - false_positives - false_negatives
  
  SENS<-c(true_positives/(true_positives+false_negatives), shape=most_common_width)
  PREC<-c(true_positives/(true_positives+false_positives), shape=most_common_width)
  SPEC<-c(true_negatives/(true_negatives+false_positives), shape=most_common_width)
  ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives), shape=most_common_width)
  
  dat_out<-as.data.frame(rbind(SENS,PREC,SPEC,ACCU))
  myDF <- cbind(test = rownames(dat_out), dat_out, time=time_out[3], width=most_common_width, height=most_common_height)
  SOMout <- list(SOM = ssom, somperf = myDF)
  
  return(SOMout)
}

# final output, saving the trained SOM, plot it, and save the confusion matrix
save_and_plot_optimal_SOM <- function(TrainingData, TestingData, most_common_width, most_common_height, TestingData, save_path = getwd()) {
  # Perform SOM with the optimal width and height
  acc5 <- performOptimalSOM(TrainingData, TestingData, most_common_width, most_common_height)
  
  # Extract the prediction outputs
  pred_outputs <- acc5$somperf
  MSOM <- acc5$SOM
  
  # Define a palette of 14 aesthetically pleasing colors
  colsch <- colorRampPalette(c("#A6CEE3", "#1F78B4", "#4363d8", "#CAB2D6", "#fabebe", "#FB9A99", 
                               "#FF7F00", "#FDBF6F", "goldenrod1", "#FFFF99", "#bfef45", "#B2DF8A", 
                               "#33A02C", "#469990"))(14)
  
  # Save the plot as a PNG
  png(filename = file.path(save_path, "optimal_SOM_plot.png"))
  
  # Create the plot using the defined palette
  plot(MSOM, heatkey = TRUE, palette.name = colsch, type = "codes", shape = "straight", ncolors = 14)
  
  # Close the graphics device, effectively saving the PNG
  dev.off()
  
  # Predict the SOM for the testing data
  ssom.pred <- predict(MSOM, newdata = TestingData)
  ptab <- table(predictions = ssom.pred$predictions$act, act = TestingData$act)
  
  # Save the confusion matrix as a CSV
  write.csv(ptab, file.path(save_path, "MSOM_5by8.1_Confusion_Matrix.csv"))
  
  # Return the confusion matrix for additional use if necessary
  return(ptab)
}








#### INPUT: define all the variables ####
tests <- c("Condition1Random", "Condition2Random", "Condition1Chron", "Condition2Chron")

savepaths <- list("Condition1Random" = "Condition1/Random", "Condition2Random" = "Condition2/Random",
                  "Condition1Chron" = "Condition1/Chronological", "Condition2Chron" = "Condition2/Chronological")
TrainingData <- list("Condition1Random" = "Condition1/Random/TrainingData.rda", "Condition2Random" = "Condition2/Random/TrainingData.rda",
                     "Condition1Chron" = "Condition1/Chronological/TrainingData.rda", "Condition2Chron" = "Condition2/Chronological/TrainingData.rda")
TestingData <- list("Condition1Random" = "Condition1/Random/TestingData.rda", "Condition2Random" = "Condition2/Random/TestingData.rda",
                    "Condition1Chron" = "Condition1/Chronological/TestingData.rda", "Condition2Chron" = "Condition2/Chronological/TestingData.rda")

#### INPUT: Execution of the script ####
results_list <- list()

for (test in tests) {
  
  load(TrainingData[[test]])
  load(TestingData[[test]])
  
  # Run the test 10 times and select the optimal shape
  optimal_dimensions <- run_som_tests(TrainingData[[test]], TestingData[[test]])
  
  # Save the heatmap
  heatmap_path <- file.path(savepaths[[test]], "heatmap.png")
  create_heatmap(average_heatmap_matrix, filename = heatmap_path)
  
  # Make a SOM with that optimal shape
  som_results <- performOptimalSOM(TrainingData[[test]], TestingData[[test]], optimal_dimensions$most_common_width, optimal_dimensions$most_common_height)
  
  # Save the behavioural map and confusion matrix
  confusion_matrix <- save_and_plot_optimal_SOM(TrainingData, TestingData, optimal_dimensions$most_common_width, optimal_dimensions$most_common_height, TestingData, save_path = savepaths[[test]])
  
  results_list[[test]] <- list("heatmap" = heatmap, "som_results" = som_results, "confusion_matrix" = confusion_matrix)
}
