## Execute the script, all functions in order

library(pacman)
p_load(here, dplyr, tidyverse, kohonen, data.table, lattice, glue, moments, fs)


# source the variables on the preceding script
source("VariableEntry.R")
source("GeneralFunctions.R")
source("FeatureProcessing.R")
source("CreateTrainingTestingData.R")

setwd(here())


#### Format Data ####
  # load in the data
  MoveData0 <- read.csv(MovementData)
  
  # select and rename the relevant columns
  MoveData <- subset_and_rename(MoveData0, columnSubset)
  
  # only select the test individuals
  if (exists("test_individuals")){
    selected_ids <- unique(MoveData$ID)[1:test_individuals]
    MoveData <- subset(MoveData, ID %in% selected_ids)
  }
  
  # format time # this will be different between studies
  # e.g., necessary for Axivity papers, but not for the DogMoveData
  #MoveData$time <- as.POSIXct((MoveData$time - 719529) * 86400, origin = "1970-01-01", tz = "UTC")
  
  # potentially downsample the data
  if (exists("desired_Hz") && exists("current_Hz")) {
    if (desired_Hz < current_Hz) {
      skip <- current_Hz / desired_Hz
      downsampled_data <- MoveData[seq(1, nrow(MoveData), by = skip), ]
    } else if (desired_Hz == current_Hz) {
      downsampled_data <- MoveData
    } else {
      message("desired_Hz is higher than the current_Hz. Cannot upsample.")
    }
  } else {
    message("Desired_Hz and current_Hz not both defined.")
  }
  MoveData <- MoveData[seq(1, nrow(MoveData), by = skip), ]
  
  # select only the chosen behaviours
  MoveData <- MoveData[MoveData$activity %in% selectedBehaviours, ]
  write.csv(MoveData, 'Formatted_MoveData.csv') # save the output
  

#### Create Directories ####  
# There can be different overlaps, different windows, and different splits
Experiment_path <- paste0("Experiment_", ExperimentNumber)
ensure_dir(Experiment_path) # experiment directory

for (w in window) { # for each of the window sizes create folders
  window_path <- file.path(Experiment_path, paste0(w, "_sec_window"))
  ensure_dir(window_path)
    
  for (o in overlap) { # for each of the overlap %s
    overlap_path <- file.path(window_path, paste0(o, "%_overlap"))
    ensure_dir(overlap_path)
      
    for (s in splitMethod) { # for each of the split methods
      split_path <- file.path(overlap_path, s)
      ensure_dir(split_path)
    }
  }
}

#### Feature Creation ####
MoveData <- read.csv('Formatted_MoveData.csv')
# Process data, creating features according to window length and overlap
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    
    # Define the correct subdirectory path using Experiment path, window, and overlap
    overlap_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"))
    print(overlap_path) # progress check
    
    processed_data <- process_data(MoveData, featuresList, window_length, overlap_percent)
    
    # Use the overlap_path to write the CSV in the correct subdirectory
    write.csv(processed_data, file.path(overlap_path, 'Processed_Data.csv'))
  }
}
  
#### Create Training and Testing Data ####
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    for (split in splitMethod) { # for each of the split methods 
    # Define the correct subdirectory path using Experiment path, window, and overlap
      file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), 'Processed_Data.csv')
      split_condition(file_path, threshold, split, trainingPercentage)
    }
  }
}

#### Trial SOM shapes ####
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    for (split in splitMethod) { # for each of the split methods 
      
      #window_length <- 1
      #overlap_percent <- 50
      #split <- c("chronological")
      
      file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split)
      
      # progress tracking
      print(file_path)
      
      load(file = file.path(file_path, "TrainingData.rda"))
      load(file = file.path(file_path, "TestingData.rda"))
      optimal_dimensions <- run_som_tests(trDat, tstDat, file_path)
      write.csv(optimal_dimensions, file.path(file_path, "Optimal_dimensions.csv"))
    }
  }
}

#### DEVELOP THE SOM MAPS ####
# Essentially the same execution as above, but using the optimal dimensions
for (window_length in window) { # for each of the windows
  for (overlap_percent in overlap) { # for each of the overlaps
    for (split in splitMethod) { # for each of the split methods 
      
      #window_length <- 1
      #overlap_percent <- 50
      #split <- c("random")
      
      file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split)
      
      # progress tracking
      print(file_path)
      
      # load the files
      load(file = file.path(file_path, "TrainingData.rda"))
      load(file = file.path(file_path, "TestingData.rda"))
      optimal_dimensions <- read.csv(file = file.path(file_path, "Optimal_dimensions.csv"))
      
      # extract the shape
      width <- optimal_dimensions$best_width
      height <- optimal_dimensions$best_height
      
      # produce the results
      som_results <- performOptimalSOM(trDat, tstDat, width, height, file_path)
      save_and_plot_optimal_SOM(trDat, tstDat, width, height, file_path)
    }
  }
}
  
