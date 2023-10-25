# Code to create the training and testing data, save as a .rda
# these are saved to their own directories, allowing them to be retrieved later 

library(pacman)
p_load(here, dplyr)

setwd(here())

## MANUAL SELECTION OF OVERREPRESENTATION VALUE
  # before processing the data, choose how many rows to remove
  dat0 <- read.csv("Condition1_processed.csv") # Example filename for visualization
  table_activity <- table(dat0$activity)
  barplot(table_activity, las = 2)
  # put the thresholds you choose into the command at the very bottom

# Functions
# ensure directories exist
  ensure_dir_exists <- function(dir_name) {
    if (!dir.exists(dir_name)) {
      dir.create(dir_name, recursive = TRUE, showWarnings = FALSE)
    }
  }  
  
# downsample the data according to the above determined value
  downsample_data <- function(data, threshold) {
    
    # Determine counts of each 'activity' and identify over-represented behaviors
    activity_counts <- data %>% 
      group_by(activity) %>%
      tally() %>%
      mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
    
    # For over-represented behaviors, sample the desired threshold number of rows
    oversampled_data <- data %>% 
      inner_join(filter(activity_counts, n > threshold), by = "activity") %>%
      group_by(activity) %>%
      sample_n(size = first(over_threshold), replace = FALSE) # Use the calculated threshold
    
    # For other behaviors, take all rows
    undersampled_data <- data %>% 
      anti_join(filter(activity_counts, n > threshold), by = "activity")
    
    # Combine and return
    return(bind_rows(oversampled_data, undersampled_data))
  }
  
# Formatting the data
trSamp2 <- function(x) { 
    d <- x[,5:47] ## INPUT ## Match these to the actual columns
    activity <- as.factor(x$activity) # Corresponding activities
    out <- list(measurements = as.matrix(d), activity = activity)
    return(out)
  }
  
# process the data
split_condition <- function(cond, filename, conditions, threshold) {
  
  dat <- read.csv(filename)
  dat <- na.omit(dat)
  
  # Balance the data
  dat <- downsample_data(dat, threshold)
  
  # ensure the directories exist
  ensure_dir_exists(file.path(cond, "Random"))
  ensure_dir_exists(file.path(cond, "Chronological"))
  
# Version One: Random 70:30 split
  ind <- dat %>% group_by(dat$activity) %>% sample_frac(.7)
  trDat<-trSamp2(ind)
  tstind<-subset(dat, !(dat$X %in% ind$X))
  tstDat<-trSamp2(tstind)
  
  # save the random training and testing data
  save(trDat, file = file.path(cond, "Random", "TrDat.rda"))
  save(tstDat, file = file.path(cond, "Random", "TstDat.rda"))
  
# Version Two: Chronological 70:30 split
  # Extract individual_ID from the file column
  dat$individual_ID <- as.numeric(sub(".*_(\\d+).*", "\\1", dat$file))
  
  # Split data based on individual_ID and calculate the 70% index for each individual
  ind2 <- dat %>%
    group_by(individual_ID) %>%
    arrange(individual_ID) %>%
    mutate(split_index = floor(0.7 * n()))
  
  # Split data into training and testing based on the calculated split index
  train_data_list <- ind2 %>%
    group_by(individual_ID, .add = TRUE) %>%
    group_split() %>%
    lapply(function(.x) .x[1:unique(.x$split_index[1]), ])
  
  test_data_list <- ind2 %>%
    group_by(individual_ID, .add = TRUE) %>%
    group_split() %>%
    lapply(function(.x) .x[(unique(.x$split_index[1]) + 1):nrow(.x), ])
  
  # Combine all the training and testing data
  trDat <- bind_rows(train_data_list)
  tstDat <- bind_rows(test_data_list)
  
  # Apply trSamp2 to the resultant datasets
  trDat <- trSamp2(trDat)
  tstDat <- trSamp2(tstDat)
  
  # Save the chronological training and testing data
  save(trDat, file = file.path(cond, "Chronological", "TrDat.rda"))
  save(tstDat, file = file.path(cond, "Chronological", "TstDat.rda"))
}

#### INPUT names of the files and thresholds ####
# Process both conditions
#condition_name <- c("Condition1", "Condition2")
condition_name <- c("Condition2")
filename <- list("Condition1" = "Condition1_processed.csv", "Condition2" = "Condition2_processed.csv")
conditions <- list("Condition1" = "Overlap", "Condition2" = "Nonoverlap")
threshold <- list("Condition1" = 20000, "Condition2" = 400)
for (cond in condition_name) {
  split_condition(cond, filename[[cond]], conditions[[cond]], threshold[[cond]])
}
