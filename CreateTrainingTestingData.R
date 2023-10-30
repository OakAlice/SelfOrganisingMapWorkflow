# Code to create the training and testing data, saving them both as .rda files

source("GeneralFunctions.R")
source("VariableEntry.R")

# balance the data according to the above determined value
balance_data <- function(dat, threshold) {
  
  #dat <- processed_data # test setting
    
    # Determine counts of each 'activity' and identify over-represented behaviors
    activity_counts <- dat %>% 
      group_by(activity) %>%
      tally() %>%
      mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
    
    # For over-represented behaviors, sample the desired threshold number of rows
    oversampled_data <- dat %>% 
      inner_join(filter(activity_counts, n > threshold), by = "activity") %>%
      group_by(activity) %>%
      sample_n(size = first(over_threshold), replace = FALSE) # Use the calculated threshold
    
    # For other behaviors, take all rows
    undersampled_data <- dat %>% 
      anti_join(filter(activity_counts, n > threshold), by = "activity")
    
    # Combine and return
    return(bind_rows(oversampled_data, undersampled_data))
  }
  
# Formatting the data #### MAY HAVE TO CHANGE THIS
trSamp2 <- function(x) { 
    d <- x[,2:21]
    activity <- as.factor(x$activity) # Corresponding activities
    out <- list(measurements = as.matrix(d), activity = activity)
    return(out)
  }
  
# process the data
split_condition <- function(file_path, threshold, split, trainingPercentage) {
  # file_path <- "Experiment_3/1_sec_window/0%_overlap/Processed_Data.csv"
  # split <-  "LOIO"
  
  dat <- read.csv(file_path)
  dat <- na.omit(dat)
  
  # Balance the data
  dat <- balance_data(dat, threshold)

# Split data by different conditions
  if (split == "random") {
    
    # if split is random, select randomly based on the specified trainingPercentage
    ind <- dat %>% 
      group_by(activity) %>%
      sample_frac(trainingPercentage)
    
    trDat <- ind
    
    tstDat <- anti_join(dat, ind, by = "X")
    
  } else if (split == "chronological") { 
    
    # Group by ID and behavior, take the first % as the training 
    # and calculate the split index for each ID-behavior combination
    id_behavior_split <- dat %>%
      group_by(ID, activity) %>%
      mutate(split_index = floor(trainingPercentage * n()))
    
    # Split data into training and testing based on the calculated split index for each ID-behavior combination
    train_data_list <- id_behavior_split %>%
      group_by(ID, activity) %>%
      group_split() %>%
      lapply(function(.x) .x[1:unique(.x$split_index[1]), ])
    
    test_data_list <- id_behavior_split %>%
      group_by(ID, activity) %>%
      group_split() %>%
      lapply(function(.x) .x[(unique(.x$split_index[1]) + 1):nrow(.x), ])
    
    # Combine all the training and testing data
    trDat <- bind_rows(train_data_list)
    tstDat <- bind_rows(test_data_list)
    
  } else if (split == "LOIO") {
    
    # Sample a random individual from the dataset
    unique_IDs <- unique(dat$ID)
    selected_individual <- sample(unique_IDs, 1)
    #selected_individual <- 20
    
    tstDat <- dat %>% filter(ID == selected_individual)
    
    trDat <- dat %>% filter(ID != selected_individual)
  }
  
  # Apply trSamp2 to the resultant datasets to format them for the SOM
  trDat <- trSamp2(trDat)
  tstDat <- trSamp2(tstDat)
  
  # Save the training data
  training_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TrainingData.rda')
  save(trDat, file = training_file_path)
  
  # save the testing data
  testing_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TestingData.rda')
  #testing_file_path <- file.path('Included_TestingData.rda')
  save(tstDat, file = testing_file_path)
}
