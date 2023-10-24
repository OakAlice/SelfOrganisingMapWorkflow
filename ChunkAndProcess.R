# Code to load, select, chunk, and produce features for dog movement data
# Contains 2 conditions - 1) with overlapping windows, and 2) non-overlapping windows

# Load required libraries
library(pacman)
p_load(here, e1071, data.table)

# Set the working directory to the location of the project
setwd(here())

# Function for ensuring directory exists or creating it if not
ensure_dir <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, showWarnings = FALSE)
  }
}

# Function to compute features for a given chunk of data
compute_features <- function(dat) {
  
  # Calculate all the features
  meanX <- mean(dat$ANeck_x)
  meanY <- mean(dat$ANeck_y)
  meanZ <- mean(dat$ANeck_z)
  
  meangX <- mean(dat$GNeck_x)
  meangY <- mean(dat$GNeck_y)
  meangZ <- mean(dat$GNeck_z)
  
  maxx <- max(dat$ANeck_x)
  maxy <- max(dat$ANeck_y)
  maxz <- max(dat$ANeck_z)
  maxgx <- max(dat$GNeck_x)
  maxgy <- max(dat$GNeck_y)
  maxgz <- max(dat$GNeck_z)
  
  minx <- min(dat$ANeck_x)
  miny <- min(dat$ANeck_y)
  minz <- min(dat$ANeck_z)
  mingx <- min(dat$GNeck_x)
  mingy <- min(dat$GNeck_y)
  mingz <- min(dat$GNeck_z)
  
  sdx <- sd(dat$ANeck_x)
  sdy <- sd(dat$ANeck_y)
  sdz <- sd(dat$ANeck_z)
  sdgx <- sd(dat$GNeck_x)
  sdgy <- sd(dat$GNeck_y)
  sdgz <- sd(dat$GNeck_z)
  
  SMA <- (sum(abs(dat$ANeck_x)) + sum(abs(dat$ANeck_y)) + sum(abs(dat$ANeck_z)))/nrow(dat)
  
  ODBA <- abs(dat$ANeck_x) + abs(dat$ANeck_y) + abs(dat$ANeck_z)
  VDBA <- sqrt(dat$ANeck_x^2 + dat$ANeck_y^2 + dat$ANeck_z^2) 
  
  minODBA <- min(ODBA)
  maxODBA <- max(ODBA)
  
  minVDBA <- min(VDBA)
  maxVDBA <- max(VDBA)
  
  sumODBA <- sum(ODBA)
  sumVDBA <- sum(VDBA)
  
  corXY <- cor(dat$ANeck_x, dat$ANeck_y, use="complete.obs")
  corXZ <- cor(dat$ANeck_x, dat$ANeck_z, use="complete.obs")
  corYZ <- cor(dat$ANeck_y, dat$ANeck_z, use="complete.obs")
  corgXY <- cor(dat$GNeck_x, dat$GNeck_y, use="complete.obs")
  corgXZ <- cor(dat$GNeck_x, dat$GNeck_z, use="complete.obs")
  corgYZ <- cor(dat$GNeck_y, dat$GNeck_z, use="complete.obs")
  
  # Here you can add other features and calculations as needed
  
  # Return a dataframe with all the features
  return(data.frame(meanX, meanY, meanZ, meangX, meangY, meangZ,
                    maxx, maxy, maxz, maxgx, maxgy, maxgz,
                    minx, miny, minz, mingx, mingy, mingz,
                    sdx, sdy, sdz, sdgx, sdgy, sdgz,
                    SMA, minODBA, maxODBA, minVDBA, maxVDBA, sumODBA, sumVDBA,
                    corXY, corXZ, corYZ, corgXY, corgXZ, corgYZ))
}

process_dog_data <- function(dog_id, DogMoveData, overlap=FALSE) {
  
  # Subset and select desired columns
  dog_data <- DogMoveData[DogMoveData$DogID == dog_id, c("DogID", "t_sec", "ANeck_x", "ANeck_y", "ANeck_z", 
                                                         "GNeck_x", "GNeck_y", "GNeck_z", "Task", "Behavior_1")]
  
  # Initialize an empty list to store the processed data chunks
  processed_chunks <- list()
  
  # Define the starting and ending points for the chunks
  st <- 1
  fn <- 100
  
  # Iterate over the chunks of data
  while (fn <= nrow(dog_data)) {
    
    # Extract the current chunk
    dat_chunk <- dog_data[st:fn, ]
    
    # Compute features for the chunk
    features_data <- compute_features(dat_chunk)
    
    # Add the processed chunk to the list
    processed_chunks <- c(processed_chunks, list(features_data))
    
    # Update starting and ending points for the next chunk
    if (overlap) {
      st <- st + 1
      fn <- fn + 1
    } else {
      st <- st + 100
      fn <- fn + 100
    }
    
  }
  
  # Combine all the processed chunks into a single data frame
  processed_data <- do.call(rbind, processed_chunks)
  # the reason why I chunk-process-recombine (seemingly pointless) is that I thought it would be faster 
  # my laptop was struggling as it was lol
  
  return(processed_data)
}

# Load the data
DogMoveData <- read.csv("DogMoveData.csv")

# Ensure directories exist
ensure_dir("IndDogMoveData")
ensure_dir("IndDogMoveChunked")
ensure_dir("Condition1")
ensure_dir("Condition2")

# Get unique DogIDs and process the first 4 dogs # again, only 4 dogs to spare my laptop
unique_dog_ids <- unique(DogMoveData$DogID)[1:4]

# Process data for each dog and condition
all_processed_data_condition1 <- lapply(unique_dog_ids, function(dog_id) process_dog_data(dog_id, DogMoveData, overlap=TRUE))
all_processed_data_condition2 <- lapply(unique_dog_ids, function(dog_id) process_dog_data(dog_id, DogMoveData, overlap=FALSE))

# Combine and save the processed data
write.csv(do.call(rbind, all_processed_data_condition1), 'Condition1_processed.csv')
write.csv(do.call(rbind, all_processed_data_condition2), 'Condition2_processed.csv')