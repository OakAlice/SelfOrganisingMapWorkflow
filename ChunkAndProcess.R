# Code to load, select, chunk, and produce features for dog movement data
# Contains 2 conditions - 1) with overlapping windows, and 2) non-overlapping windows

# install and load packages
install.packages("pacman")
library(pacman)
p_load(here, e1071, data.table)

# set the working directory to the location of the project
setwd(here())

# put DogMoveData into it
DogMoveData <- read.csv("DogMoveData.csv")

# this is a lot of data, for this test I'll only be using a small fraction of it
# in this example, the first 4 dogs

# Get unique DogIDs from the 'DogMoveData' dataframe
unique_dog_ids <- unique(DogMoveData$DogID)

# Create a directory to store the CSV files (optional)
dir.create("IndDogMoveData", showWarnings = FALSE)

# Loop through each unique DogID for the first 4
for (dog_id in unique_dog_ids[1:4]) {
  # Subset the dataframe for the current DogID
  dog_data <- DogMoveData[DogMoveData$DogID == dog_id, ]
  
  # Select the desired columns
  selected_columns <- dog_data[c("DogID", "t_sec", "ANeck_x", "ANeck_y", "ANeck_z", 
                                 "GNeck_x", "GNeck_y", "GNeck_z", "Task", 
                                 "Behavior_1")]
  
  # Name the CSV by ID
  filename <- paste0("IndDogMoveData/Dog_", dog_id, ".csv")
  
  # Save the selected data as a CSV file with the same name
  write.csv(selected_columns, filename, row.names = FALSE)
  
  print(filename)
}

file_list <- list.files("IndDogMoveData", full.names = TRUE)

# now, for these individuals, split each of the files into 10,000 row chunks
# Create a directory to store the processed CSV files
dir.create("IndDogMoveChunked", showWarnings = FALSE)

# Loop through each dog's dataframe
for (i in seq_along(unique_dog_ids)) {
  dog <- selected_dogs[[i]]
  dog_df <- read.csv(dog)
  dog_id <- unique(dog_df$DogID)
  
  # Split the dataframe into chunks of 10,000 rows
  chunk_size <- 10000
  num_chunks <- ceiling(nrow(dog_df) / chunk_size)
  
  for (j in 1:num_chunks) {
    start_row <- (j - 1) * chunk_size + 1
    end_row <- min(j * chunk_size, nrow(dog_df))
    
    # Create a new dataframe chunk
    chunk_df <- dog_df[start_row:end_row, ]
    
    # Label each new dataframe
    chunk_label <- sprintf("%s_%03d", dog_id, j)
    
    # Define the filename for the chunk
    filename <- file.path("IndDogMoveChunked/", paste0(chunk_label, ".csv"))
    
    # Save the chunk as a CSV file
    write.csv(chunk_df, filename, row.names = FALSE)
    
    cat("Saved file:", filename, "\n")
  }
}

# we now have the individual's data chunked and saved into a folder
# the next stage is to produce features, which we will do in 4 different ways
# workflow for windows and features from Gaschk et al., 2023

#### CONDITION ONE: Overlapping ####
# create a folder for this condition
dir.create("Condition1", showWarnings = FALSE)

filenames <- list.files("IndDogMoveChunked", full.names = TRUE)

# A for loop to run through all the files
for (ii in 1:length(filenames)) {
  
  #ii = 1 # test setting
  
  # Print the file name for progress tracking
  print(filenames[ii])
  
  # loading in the file from the correct folder
  data <- read.csv(filenames[ii], sep = ",", header = TRUE)
  
  # format
  options("digits" = 19)
  
  # setting the epoch length as 100 samples (i.e., at 100Hz = 1 sec)
  st <- 1
  fn <- 100
  
  # initialising an empty dataframe for the epoch
  dat_epoch <- data.frame(
    file = NA, time = NA, meanX = NA, meanY = NA, meanZ = NA, meangX = NA, meangY = NA, meangZ = NA,
    maxx = NA, maxy = NA, maxz = NA, maxgx = NA, maxgy = NA, maxgz = NA,
    minx = NA, miny = NA, minz = NA, mingx = NA, mingy = NA, mingz = NA,
    sdx = NA, sdy = NA, sdz = NA, sdgx = NA, sdgy = NA, sdgz = NA,
    SMA = NA, minODBA = NA, maxODBA = NA, minVDBA = NA, maxVDBA = NA,
    sumODBA = NA, sumVDBA = NA,
    corXY = NA, corXZ = NA, corYZ = NA, corgXY = NA, corgXZ = NA, corgYZ = NA,
    skx = NA, sky = NA, skz = NA, skgx = NA, skgy = NA, skgz = NA, act = NA
  )
  
  # intialising an empty dataframe for the file
  dat_file <- data.frame(
    file = NA, time = NA, meanX = NA, meanY = NA, meanZ = NA, meangX = NA, meangY = NA, meangZ = NA,
    maxx = NA, maxy = NA, maxz = NA, maxgx = NA, maxgy = NA, maxgz = NA,
    minx = NA, miny = NA, minz = NA, mingx = NA, mingy = NA, mingz = NA,
    sdx = NA, sdy = NA, sdz = NA, sdgx = NA, sdgy = NA, sdgz = NA,
    SMA = NA, minODBA = NA, maxODBA = NA, minVDBA = NA, maxVDBA = NA,
    sumODBA = NA, sumVDBA = NA,
    corXY = NA, corXZ = NA, corYZ = NA, corgXY = NA, corgXZ = NA, corgYZ = NA,
    skx = NA, sky = NA, skz = NA, skgx = NA, skgy = NA, skgz = NA, act = NA
  )
  
  # while the sample number is less than the total rows in the csv
  while (fn < nrow(data)) {
    
    # for this specified epoch
    dat1 <- data[st:fn, ]
    
    # calculate all the features
    meanX <- mean(dat1$ANeck_x)
    meany <- mean(dat1$ANeck_y)
    meanz <- mean(dat1$ANeck_z)
    
    meangX <- mean(dat1$GNeck_x)
    meangy <- mean(dat1$GNeck_y)
    meangz <- mean(dat1$GNeck_z)
    
    maxx <- max(dat1$ANeck_x)
    maxy <- max(dat1$ANeck_y)
    maxz <- max(dat1$ANeck_z)
    maxgx <- max(dat1$GNeck_x)
    maxgy <- max(dat1$GNeck_y)
    maxgz <- max(dat1$GNeck_z)
    
    minx <- min(dat1$ANeck_x)
    miny <- min(dat1$ANeck_y)
    minz <- min(dat1$ANeck_z)
    mingx <- min(dat1$GNeck_x)
    mingy <- min(dat1$GNeck_y)
    mingz <- min(dat1$GNeck_z)
    
    sdx <- sd(dat1$ANeck_x)
    sdy <- sd(dat1$ANeck_y)
    sdz <- sd(dat1$ANeck_z)
    sdgx <- sd(dat1$GNeck_x)
    sdgy <- sd(dat1$GNeck_y)
    sdgz <- sd(dat1$GNeck_z)
    
    SMA <- (sum(abs(dat1$ANeck_x)) + sum(abs(dat1$ANeck_y)) + sum(abs(dat1$ANeck_z)))/nrow(dat1)
    
    ODBA <- abs(dat1$ANeck_x) + abs(dat1$ANeck_y) + abs(dat1$ANeck_z)
    VDBA <- sqrt(dat1$ANeck_x^2 + dat1$ANeck_y^2 + dat1$ANeck_z^2) 
    
    minODBA <- min(ODBA)
    maxODBA <- max(ODBA)
    
    minVDBA <- min(VDBA)
    maxVDBA <- max(VDBA)
    
    sumODBA <- sum(ODBA)
    sumVDBA <- sum(VDBA)
    
    corXY <- cor(dat1$ANeck_x, dat1$ANeck_y)
    corXZ <- cor(dat1$ANeck_x, dat1$ANeck_z)
    corYZ <- cor(dat1$ANeck_y, dat1$ANeck_z)
    corgXY <- cor(dat1$GNeck_x, dat1$GNeck_y)
    corgXZ <- cor(dat1$GNeck_x, dat1$GNeck_z)
    corgYZ <- cor(dat1$GNeck_y, dat1$GNeck_z)
    
    skx <- skewness(dat1$ANeck_x)
    sky <- skewness(dat1$ANeck_y)
    skz <- skewness(dat1$ANeck_z)
    skgx <- skewness(dat1$GNeck_x)
    skgy <- skewness(dat1$GNeck_y)
    skgz <- skewness(dat1$GNeck_z)
    
    time <- as.POSIXct((dat1$t_sec[1] - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
    
    act = names(which.max(table(dat1$Behavior_1)))
    
    # put all of these into a 1 row dataframe specific to that epoch           
    dat_epoch <- data.frame(
      file = filenames[ii], time = time, meanX = meanX, meanY = meany, meanZ = meanz, meangX = meangX, meangY = meangy, meangZ = meangz,
      maxx = maxx, maxy = maxy, maxz = maxz, maxgx = maxgx, maxgy = maxgy, maxgz = maxgz,
      minx = minx, miny = miny, minz = minz, mingx = mingx, mingy = mingy, mingz = mingz,
      sdx = sdx, sdy = sdy, sdz = sdz, sdgx = sdgx, sdgy = sdgy, sdgz = sdgz,
      SMA = SMA, minODBA = minODBA, maxODBA = maxODBA, minVDBA = minVDBA, maxVDBA = maxVDBA,
      sumODBA = sumODBA, sumVDBA = sumVDBA,
      corXY = corXY, corXZ = corXZ, corYZ = corYZ, corgXY = corgXY, corgXZ = corgXZ, corgYZ = corgYZ,
      skx = skx, sky = sky, skz = skz, skgx = skgx, skgy = skgy, skgz = skgz, act = act
    )
    
    # bind that row to the dataframe for this whole file
    dat_file <- rbind(dat_file, dat_epoch)
    
    # set the overlap (same as for the training data)  
    st = st + 1
    fn = fn + 1
    
  }  # end loop that iterates over the epochs
  
  # Save the current processed data frame to a file
  filename <- sub(".*/([0-9]+_[0-9]+)\\.csv", "\\1", filenames[ii])
  write.csv(dat_file, paste0('Condition1/', filename, '_processed.csv'))
  
} # end for loop

# recombine them for the next stage
# List all files in the "Condition1" folder
file_list <- list.files("Condition1", full.names = TRUE)
# Read all files and combine them into a single dataframe
combined_dataframe <- rbindlist(lapply(file_list, fread), fill = TRUE)
write.csv(combined_dataframe, 'Condition1_processed.csv')

#### CONDITION TWO: Non-overlapping windows ####

# create a folder for this condition
dir.create("Condition2", showWarnings = FALSE)

filenames <- list.files("IndDogMoveChunked", full.names = TRUE)

# A for loop to run through all the files
for (ii in 1:length(filenames)) {
  
  #ii = 1 # test setting
  
  # Print the file name for progress tracking
  print(filenames[ii])
  
  # loading in the file from the correct folder
  data <- read.csv(filenames[ii], sep = ",", header = TRUE)
  
  # format
  options("digits" = 19)
  
  # setting the epoch length as 100 samples (i.e., at 100Hz = 1 sec)
  st <- 1
  fn <- 100
  
  # initialising an empty dataframe for the epoch
  dat_epoch <- data.frame(
    file = NA, time = NA, meanX = NA, meanY = NA, meanZ = NA, meangX = NA, meangY = NA, meangZ = NA,
    maxx = NA, maxy = NA, maxz = NA, maxgx = NA, maxgy = NA, maxgz = NA,
    minx = NA, miny = NA, minz = NA, mingx = NA, mingy = NA, mingz = NA,
    sdx = NA, sdy = NA, sdz = NA, sdgx = NA, sdgy = NA, sdgz = NA,
    SMA = NA, minODBA = NA, maxODBA = NA, minVDBA = NA, maxVDBA = NA,
    sumODBA = NA, sumVDBA = NA,
    corXY = NA, corXZ = NA, corYZ = NA, corgXY = NA, corgXZ = NA, corgYZ = NA,
    skx = NA, sky = NA, skz = NA, skgx = NA, skgy = NA, skgz = NA, act = NA
  )
  
  # intialising an empty dataframe for the file
  dat_file <- data.frame(
    file = NA, time = NA, meanX = NA, meanY = NA, meanZ = NA, meangX = NA, meangY = NA, meangZ = NA,
    maxx = NA, maxy = NA, maxz = NA, maxgx = NA, maxgy = NA, maxgz = NA,
    minx = NA, miny = NA, minz = NA, mingx = NA, mingy = NA, mingz = NA,
    sdx = NA, sdy = NA, sdz = NA, sdgx = NA, sdgy = NA, sdgz = NA,
    SMA = NA, minODBA = NA, maxODBA = NA, minVDBA = NA, maxVDBA = NA,
    sumODBA = NA, sumVDBA = NA,
    corXY = NA, corXZ = NA, corYZ = NA, corgXY = NA, corgXZ = NA, corgYZ = NA,
    skx = NA, sky = NA, skz = NA, skgx = NA, skgy = NA, skgz = NA, act = NA
  )
  
  # while the sample number is less than the total rows in the csv
  while (fn < nrow(data)) {
    
    # for this specified epoch
    dat1 <- data[st:fn, ]
    
    # calculate all the features
    meanX <- mean(dat1$ANeck_x)
    meany <- mean(dat1$ANeck_y)
    meanz <- mean(dat1$ANeck_z)
    
    meangX <- mean(dat1$GNeck_x)
    meangy <- mean(dat1$GNeck_y)
    meangz <- mean(dat1$GNeck_z)
    
    maxx <- max(dat1$ANeck_x)
    maxy <- max(dat1$ANeck_y)
    maxz <- max(dat1$ANeck_z)
    maxgx <- max(dat1$GNeck_x)
    maxgy <- max(dat1$GNeck_y)
    maxgz <- max(dat1$GNeck_z)
    
    minx <- min(dat1$ANeck_x)
    miny <- min(dat1$ANeck_y)
    minz <- min(dat1$ANeck_z)
    mingx <- min(dat1$GNeck_x)
    mingy <- min(dat1$GNeck_y)
    mingz <- min(dat1$GNeck_z)
    
    sdx <- sd(dat1$ANeck_x)
    sdy <- sd(dat1$ANeck_y)
    sdz <- sd(dat1$ANeck_z)
    sdgx <- sd(dat1$GNeck_x)
    sdgy <- sd(dat1$GNeck_y)
    sdgz <- sd(dat1$GNeck_z)
    
    SMA <- (sum(abs(dat1$ANeck_x)) + sum(abs(dat1$ANeck_y)) + sum(abs(dat1$ANeck_z)))/nrow(dat1)
    
    ODBA <- abs(dat1$ANeck_x) + abs(dat1$ANeck_y) + abs(dat1$ANeck_z)
    VDBA <- sqrt(dat1$ANeck_x^2 + dat1$ANeck_y^2 + dat1$ANeck_z^2) 
    
    minODBA <- min(ODBA)
    maxODBA <- max(ODBA)
    
    minVDBA <- min(VDBA)
    maxVDBA <- max(VDBA)
    
    sumODBA <- sum(ODBA)
    sumVDBA <- sum(VDBA)
    
    corXY <- cor(dat1$ANeck_x, dat1$ANeck_y)
    corXZ <- cor(dat1$ANeck_x, dat1$ANeck_z)
    corYZ <- cor(dat1$ANeck_y, dat1$ANeck_z)
    corgXY <- cor(dat1$GNeck_x, dat1$GNeck_y)
    corgXZ <- cor(dat1$GNeck_x, dat1$GNeck_z)
    corgYZ <- cor(dat1$GNeck_y, dat1$GNeck_z)
    
    skx <- skewness(dat1$ANeck_x)
    sky <- skewness(dat1$ANeck_y)
    skz <- skewness(dat1$ANeck_z)
    skgx <- skewness(dat1$GNeck_x)
    skgy <- skewness(dat1$GNeck_y)
    skgz <- skewness(dat1$GNeck_z)
    
    time <- as.POSIXct((dat1$t_sec[1] - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
    
    act = names(which.max(table(dat1$Behavior_1)))
    
    # put all of these into a 1 row dataframe specific to that epoch           
    dat_epoch <- data.frame(
      file = filenames[ii], time = time, meanX = meanX, meanY = meany, meanZ = meanz, meangX = meangX, meangY = meangy, meangZ = meangz,
      maxx = maxx, maxy = maxy, maxz = maxz, maxgx = maxgx, maxgy = maxgy, maxgz = maxgz,
      minx = minx, miny = miny, minz = minz, mingx = mingx, mingy = mingy, mingz = mingz,
      sdx = sdx, sdy = sdy, sdz = sdz, sdgx = sdgx, sdgy = sdgy, sdgz = sdgz,
      SMA = SMA, minODBA = minODBA, maxODBA = maxODBA, minVDBA = minVDBA, maxVDBA = maxVDBA,
      sumODBA = sumODBA, sumVDBA = sumVDBA,
      corXY = corXY, corXZ = corXZ, corYZ = corYZ, corgXY = corgXY, corgXZ = corgXZ, corgYZ = corgYZ,
      skx = skx, sky = sky, skz = skz, skgx = skgx, skgy = skgy, skgz = skgz, act = act
    )
    
    # bind that row to the dataframe for this whole file
    dat_file <- rbind(dat_file, dat_epoch)
    
    # set the overlap (same as for the training data)  
    st = st + 100
    fn = fn + 100
    
  }  # end loop that iterates over the epochs
   
  # Save the current processed data frame to a file
  filename <- sub(".*/([0-9]+_[0-9]+)\\.csv", "\\1", filenames[ii])
  write.csv(dat_file, paste0('Condition2/', filename, '_processed.csv'))
  
} # end for loop

# recombine them for the next stage
# List all files in the "Condition2" folder
file_list <- list.files("Condition2", full.names = TRUE)
# Read all files and combine them into a single dataframe
combined_dataframe <- rbindlist(lapply(file_list, fread), fill = TRUE)
write.csv(combined_dataframe, 'Condition2_processed.csv')
