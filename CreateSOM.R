# Now this will be the code that creates the SOM from the training and testing data we created
# It has two tasks: 1) Testing multiple shapes to find optimal. 2) Applying that optimal shape to a final SOM.
# Adapted from Gaschk et al., 2023

library(pacman)
p_load(parallel, here)

setwd(here())

# for when you have determined which shape is the best, run the full version
performOptimalSOM <- function(TrDat, TstDat, most_common_width, most_common_height) {
  time_out<-system.time(
    ssom <- supersom(TrDat, grid = somgrid(most_common_width, most_common_height, "hexagonal"))
  )
  ssom.pred <- predict(ssom, newdata = TstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = TstDat$act)
  
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
save_and_plot_optimal_SOM <- function(TrDat, TstDat, most_common_width, most_common_height, save_path) {
  # Perform SOM with the optimal width and height
  acc5 <- performOptimalSOM(TrDat, TstDat, most_common_width, most_common_height)
  
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
  ssom.pred <- predict(MSOM, newdata = TstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = TstDat$act)
  
  # Save the confusion matrix as a CSV
  write.csv(ptab, file.path(save_path, "MSOM_5by8.1_Confusion_Matrix.csv"))
  
  # Return the confusion matrix for additional use if necessary
  return(ptab)
}


#### INPUT: Execution of the script ####
# You'll need to source Script 1 or manually input the optimal_dimensions for each test
results_list_2 <- list()

for (test in tests) {
  
  load(TrDat[[test]])
  load(TstDat[[test]])
  
  most_common_width <- results_list[[test]]$optimal_dimensions$most_common_width
  most_common_height <- results_list[[test]]$optimal_dimensions$most_common_height
  
  # Make a SOM with the optimal shape
  som_results <- performOptimalSOM(TrDat[[test]], TstDat[[test]], most_common_width, most_common_height)
  
  save_and_plot_optimal_SOM(TrDat[[test]], TstDat[[test]], most_common_width, most_common_height, savepaths[[test]])
  
  # Save the behavioural map and confusion matrix
  confusion_matrix <- save_and_plot_optimal_SOM(TrDat[[test]], TstDat[[test]], most_common_width, most_common_height, savepaths[[test]])
  
  results_list_2[[test]] <- list("som_results" = som_results, "confusion_matrix" = confusion_matrix)
}