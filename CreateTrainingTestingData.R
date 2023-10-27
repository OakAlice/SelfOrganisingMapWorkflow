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
  
# Formatting the data
trSamp2 <- function(x) { 
    d <- x[,1:32]
    activity <- as.factor(x$activity) # Corresponding activities
    out <- list(measurements = as.matrix(d), activity = activity)
    return(out)
  }
  
# process the data
split_condition <- function(file_path, threshold, split, trainingPercentage) {
  
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
    
    # Group by ID and behavior, arrange chronologically (assuming there's a timestamp column for this),
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
    
    # find the individuals who exhibited every behaviour
    unique_behaviors <- unique(dat$behavior)
    
    # Identify individuals who have exhibited all behaviors
    selected_individuals <- dat %>%
      group_by(ID) %>%
      summarise(all_behaviors = all(unique_behaviors %in% behavior)) %>%
      filter(all_behaviors) %>%
      pull(ID)
    
    # Check if there are enough individuals to split into training and test sets
    if (length(selected_individuals) <= 2) {
      print("Insufficient number of individuals who have exhibited all behaviors. Skipping this iteration.")
      return()
    }
    
    # select an ID as the test set
    test_id <- sample(selected_individuals, 1)
    
    tstDat <- dat %>% filter(ID == test_id)
    trDat <- dat %>% filter(ID != test_id)
    
  }
  
  # Apply trSamp2 to the resultant datasets to format them for the SOM
  trDat <- trSamp2(trDat)
  tstDat <- trSamp2(tstDat)
  
  # Save the training data
  training_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TrainingData.rda')
  save(trDat, file = training_file_path)
  
  # save the testing data
  testing_file_path <- file.path(Experiment_path, paste0(window_length, "_sec_window"), paste0(overlap_percent, "%_overlap"), split, 'TestingData.rda')
  save(tstDat, file = testing_file_path)
}
