# Now this will be the code that creates the SOM from the training and testing data we created

# when you have determined which shape is the best, run the full version and get all the outputs
performOptimalSOM <- function(trDat, tstDat, width, height, file_path, epochs) {
  #epochs = 100
  
  ssom <- supersom(trDat, grid = somgrid(width, height, "hexagonal"), rlen = epochs, whatmap = c("measurements", "activity"))
  
  # save this optimal SOM
  #file_path2 <- "Possum_data"
  file_path2 <- file.path(file_path, paste0(epochs, "_epochs"))
  save(ssom, file = file.path(file_path2, "Final_SOM.rda"))
  
  ssom.pred <- predict(ssom, newdata = tstDat, whatmap = "measurements")
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  # use table to make statistics for understanding the model performance
  true_positives  <- diag(ptab)
  false_positives <- rowSums(ptab) - true_positives
  false_negatives <- colSums(ptab) - true_positives
  true_negatives  <- rep(sum(ptab), length(true_positives)) - rowSums(ptab) - colSums(ptab) + true_positives
  
  SENS <- true_positives / (true_positives + false_negatives)
  PREC <- true_positives / (true_positives + false_positives)
  SPEC <- true_negatives / (true_negatives + false_positives)
  ACCU <- sum(true_positives) / sum(ptab)
  
  # Prepare the statistics data frame
  dat_out <- as.data.frame(rbind(SENS, PREC, SPEC, ACCU=ACCU))
  
  statistical_results <- cbind(test = rownames(dat_out), dat_out)
  write.csv(statistical_results, file.path(file_path2, "Statistical_results.csv"))
  
  SOMoutput <- list(SOM = ssom, SOM_performance = statistical_results)
  
  return(SOMoutput)
  
}

# final output, saving the trained SOM, plot it, and save the confusion matrix
save_and_plot_optimal_SOM <- function(trDat, tstDat, width, height, file_path, epochs) {
  
  # Create a confusion matrix
  load(file = file.path(file_path, paste0(epochs, "_epochs"), "Final_SOM.rda"))
  ssom.pred <- predict(ssom, newdata = tstDat, whatmap = "measurements")
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  write.csv(ptab, file.path(file_path, paste0(epochs, "_epochs"), "Confusion_Matrix.csv"))
  
  # make plots
  # Perform SOM with the optimal width and height
  SOMoutput <- performOptimalSOM(trDat, tstDat, width, height, file_path, epochs)
  
  # Extract the prediction outputs
  pred_outputs <- SOMoutput$SOM_performance
  SOM_model <- SOMoutput$SOM
  
  ## PLOTS 
  colours <- c("#A6CEE3", "#1F78B4", "#4363d8", "#CAB2D6", "#fabebe", "#FB9A99", 
               "#FF7F00", "#FDBF6F", "goldenrod1", "#FFFF99", "#bfef45", "#B2DF8A", 
               "#33A02C", "#469990")
  
  # Use the function to save and move the plots
  # main mapping plots
  plot_and_move("Mapping.png", paste0(file_path, "/", epochs, "_epochs"),
                { plot(SOM_model, type="mapping", pchs=20, col=colours, main="Mapping of behaviors on SOM") })
  plot_and_move("optimal_SOM_plot.png", paste0(file_path, "/", epochs, "_epochs"),
                { plot(SOM_model, heatkey = TRUE, col = colours, type = "codes", shape = "straight", ncolors = 14) })
  
  #plot_and_move("Training_Process.png", paste0(file_path, "/", epochs, "_epochs"),
                #{ plot(SOM_model, type="changes", main = "Training Process") })
  plot_and_move("Codes_Weight.png", paste0(file_path, "/", epochs, "_epochs"),
                { plot(SOM_model, type="codes", main="Codes/Weights of the SOM nodes") })
  plot_and_move("Counts.png", paste0(file_path, "/", epochs, "_epochs"),
                { plot(SOM_model, type="counts", main="Counts") })
  plot_and_move("Quality.png", paste0(file_path, "/", epochs, "_epochs"),
                { plot(SOM_model, type="quality", main="Quality") })
  # Return the confusion matrix for additional use if necessary
  return(ptab)
}
