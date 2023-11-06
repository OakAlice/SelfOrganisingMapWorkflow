# Comparing outcomes of the various SOM conditions
# pull each of the results and combine them into a central csv and set of graphs

Summarise_results <- function(ExperimentNumber, Results_tables) {
  Summary_results <- data.frame()
  
  for (results in Results_tables) {
    # table1 <- dat_out
    
    table1 <- read.csv(results)
    
    # Compute the average for each metric
    table1$average <- rowMeans(table1[, 3:10], na.rm = TRUE)
    
    # Extract subdirectory names from the path
    subdirs <- unlist(strsplit(dirname(results), "/"))[-1]
    names(subdirs) <- c("window", "overlap", "split", "epochs")
    
    # Add subdirectory information to the results
    table1 <- cbind(table1, as.data.frame(t(subdirs)))
    
    # Only keep the metrics and the subdirectory information
    average_table1 <- table1[, c("X", "average", "window", "overlap", "split", "epochs")]
    average_table1 <- average_table1 %>% spread(X, average)
    
    Summary_results <- rbind(Summary_results, average_table1)
  }
  
  write.csv(Summary_results, paste0("Experiment_", ExperimentNumber, "/Summary_results.csv"))
  return(Summary_results)
}

Plot_results <- function(ExperimentNumber, Results_maps) {
  # Create a grid layout for the images
  num_images <- length(Results_maps)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(num_images, 1)))
  
  # Plot each image with its subdirectory path as the title
  for (i in 1:num_images) {
    # Read the image
    img <- readPNG(Results_maps[i])
    
    # Extract subdirectory names from the path for the title
    title <- paste(dirname(Results_maps[i]), collapse = " / ")
    
    # Plot the image
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 1))
    grid.raster(img, interpolate=FALSE)
    grid.text(title, y = 1, just = "top", gp = gpar(fontsize = 12))
    popViewport()
  }
  
  # Save the combined plot to a file
  png(paste0("Experiment_", ExperimentNumber, "/Combined_maps.png"), width = 2400, height = 3200)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(num_images, 1)))
  for (i in 1:num_images) {
    img <- readPNG(Results_maps[i])
    title <- paste(dirname(Results_maps[i]), collapse = " / ")
    pushViewport(viewport(layout.pos.row = i, layout.pos.col = 1))
    grid.raster(img, interpolate=FALSE)
    grid.text(title, y = 1, just = "top", gp = gpar(fontsize = 12))
    popViewport()
  }
  dev.off()
}
