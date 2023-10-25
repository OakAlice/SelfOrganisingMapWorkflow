# Now this will be the code that creates the SOM from the training and testing data we created
# It has two tasks: 1) Testing multiple shapes to find optimal. 2) Applying that optimal shape to a final SOM.
# Adapted from Gaschk et al., 2023

library(pacman)
p_load(parallel, here, dplyr, tidyverse, kohonen, RColorBrewer, data.table, sentimentr, lattice, glue, parallel, foreach, doParallel, moments)


setwd(here())

# for when you have determined which shape is the best, run the full version
performOptimalSOM <- function(trDat, tstDat, width, height) {
  time_out<-system.time(
    ssom <- supersom(trDat, grid = somgrid(width, height, "hexagonal"))
  )
  
  # save this optimal SOM
  save(ssom, file ="Final_SOM.rda")
  
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
  myDF <- cbind(test = rownames(dat_out), dat_out, time=time_out[3], width=width, height=height)
  SOMout <- list(SOM = ssom, somperf = myDF)
  
  return(SOMout)
}

# final output, saving the trained SOM, plot it, and save the confusion matrix
save_and_plot_optimal_SOM <- function(trDat, tstDat, width, height, save_path) {
  
  # train the SOM and then predict it across the testing data
  ssom <- supersom(trDat, grid = somgrid(9, 9, "hexagonal"))
  ssom.pred <- predict(ssom, newdata = tstDat)
  
  # Perform SOM with the optimal width and height
  acc5 <- performOptimalSOM(trDat, tstDat, width, height)
  
  # Extract the prediction outputs
  pred_outputs <- acc5$somperf
  SOM_model <- acc5$SOM
  
  ## PLOTS 
  
  # Property
  png(filename = file.path("Property.png"))
  plot(SOM_model, type="property", property = SOM_model$codes[2], main = "Property")
  dev.off()
  
  # Training process
  png(filename = file.path("Training_Process.png"))
  plot(SOM_model, type="changes", main = "Training Process")
  dev.off()
  
  # Codes Weights
  png(filename = file.path("Codes_Weight.png"))
  plot(SOM_model, type="codes", main="Codes/Weights of the SOM nodes")
  dev.off()
  
  # Counts
  png(filename = file.path("Counts.png"))
  plot(SOM_model, type="counts", main="Counts")
  dev.off()
  
  # Quality
  png(filename = file.path("Quality.png"))
  plot(SOM_model, type="quality", main="Quality")
  dev.off()
  
  # Mapping
  colours <- c("#A6CEE3", "#1F78B4", "#4363d8", "#CAB2D6", "#fabebe", "#FB9A99", 
               "#FF7F00", "#FDBF6F", "goldenrod1", "#FFFF99", "#bfef45", "#B2DF8A", 
               "#33A02C", "#469990")
  
  png(filename = file.path("Mapping.png"))
  plot(SOM_model, type="mapping", pchs=20, col=colours, main="Mapping of behaviors on SOM")
  dev.off()
  
  # Standard map from other papers
  png(filename = file.path("optimal_SOM_plot.png"))
  map_image <- plot(SOM_model, heatkey = TRUE, palette.name = colours, type = "codes", shape = "straight", ncolors = 14)
  print(map_image)
  dev.off()
  # note that "act" stands for activation: the activation level of a neuron (or node) shows 
  # how close the given input vector is to the weight vector of that neuron
  

  ## Confusion matrix
  # Predict the SOM for the testing data
  ssom.pred <- predict(SOM_model, newdata = tstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  # Save the confusion matrix as a CSV
  write.csv(ptab, file.path("Confusion_Matrix.csv"))
  
  # Return the confusion matrix for additional use if necessary
  return(ptab)
}


#### INPUT: Execution of the script ####
tests <- c("Condition1Random", "Condition2Random", "Condition1Chron", "Condition2Chron")

savepaths <- list("Condition1Random" = "Condition1/Random", "Condition2Random" = "Condition2/Random",
                  "Condition1Chron" = "Condition1/Chronological", "Condition2Chron" = "Condition2/Chronological")

# input these from the previous script
width <- list("Condition1Random" = 0, "Condition2Random" = 9,
              "Condition1Chron" = 0, "Condition2Chron" = 9)
  
height <- list("Condition1Random" = 0, "Condition2Random" = 7,
               "Condition1Chron" = 0, "Condition2Chron" = 6)

results_list_2 <- list()

for (test in tests) {
  
  #test = "Condition2Random"
  setwd(savepaths[[test]]) # set the working directory to our spot
  
  load("TrDat.rda")
  load("TstDat.rda")
  
  width <- width[[test]]
  height <- height[[test]]
  
  # Make a SOM with the optimal shape
  som_results <- performOptimalSOM(trDat, tstDat, width, height)
  
  save_and_plot_optimal_SOM(trDat, tstDat, width, height, savepaths[[test]])
  
  # Save the behavioural map and confusion matrix
  confusion_matrix <- save_and_plot_optimal_SOM(TrDat[[test]], TstDat[[test]], most_common_width, most_common_height, savepaths[[test]])
  
  results_list_2[[test]] <- list("som_results" = som_results, "confusion_matrix" = confusion_matrix)
  
  setwd(here()) # reset the working directory
}