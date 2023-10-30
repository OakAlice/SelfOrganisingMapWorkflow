# Now this will be the code that creates the SOM from the training and testing data we created

source("GeneralFunctions.R")

# when you have determined which shape is the best, run the full version and get all the outputs
performOptimalSOM <- function(trDat, tstDat, width, height, file_path) {
  
  ssom <- supersom(trDat, grid = somgrid(width, height, "hexagonal"))
  
  # save this optimal SOM
  save(ssom, file = file.path(file_path, "Final_SOM.rda"))
  
  ssom.pred <- predict(ssom, newdata = tstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  true_positives  <- diag(ptab)
  false_positives <- rowSums(ptab) - true_positives
  false_negatives <- colSums(ptab) - true_positives
  true_negatives  <- sum(ptab) - true_positives - false_positives - false_negatives
  
  SENS<-c(true_positives/(true_positives+false_negatives), shape=width)
  PREC<-c(true_positives/(true_positives+false_positives), shape=width)
  SPEC<-c(true_negatives/(true_negatives+false_positives), shape=width)
  ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives), shape=width)
  
  dat_out<-as.data.frame(rbind(SENS,PREC,SPEC,ACCU))
  statistical_results <- cbind(test = rownames(dat_out), dat_out)
  write.csv(statistical_results, file.path(file_path, "Statistical_results.csv"))
  
  SOMoutput <- list(SOM = ssom, SOM_performance = statistical_results)
  
  return(SOMoutput)
  
}

# final output, saving the trained SOM, plot it, and save the confusion matrix
save_and_plot_optimal_SOM <- function(trDat, tstDat, width, height, file_path) {
  
  # Create a confusion matrix
  load(file = file.path(file_path, "Final_SOM.rda"))
  ssom.pred <- predict(ssom, newdata = tstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  write.csv(ptab, file.path(file_path, "Confusion_Matrix.csv"))
  
  # make plots
  # Perform SOM with the optimal width and height
  SOMoutput <- performOptimalSOM(trDat, tstDat, width, height, file_path)
  
  # Extract the prediction outputs
  pred_outputs <- SOMoutput$SOM_performance
  SOM_model <- SOMoutput$SOM
  
  ## PLOTS 
  colours <- c("#A6CEE3", "#1F78B4", "#4363d8", "#CAB2D6", "#fabebe", "#FB9A99", 
               "#FF7F00", "#FDBF6F", "goldenrod1", "#FFFF99", "#bfef45", "#B2DF8A", 
               "#33A02C", "#469990")
  
  # Use the function to save and move the plots
  # main mapping plots
  #plot_and_move("Mapping.png", { plot(SOM_model, type="mapping", pchs=20, col=colours, main="Mapping of behaviors on SOM") })
  plot_and_move("optimal_SOM_plot.png", file_path,
                { plot(SOM_model, heatkey = TRUE, col = colours, type = "codes", shape = "straight", ncolors = 14) })
  
  plot_and_move("Training_Process.png", file_path,
                { plot(SOM_model, type="changes", main = "Training Process") })
  plot_and_move("Codes_Weight.png", file_path,
                { plot(SOM_model, type="codes", main="Codes/Weights of the SOM nodes") })
  plot_and_move("Counts.png", file_path,
                { plot(SOM_model, type="counts", main="Counts") })
  plot_and_move("Quality.png", file_path,
                { plot(SOM_model, type="quality", main="Quality") })
  # Return the confusion matrix for additional use if necessary
  return(ptab)
}
