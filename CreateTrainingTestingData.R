# Code to create the training and testing data, save as a .rda
# these are saved to their own directories, allowing them to be retrieved later 

library(pacman)
p_load(here, tidyverse, kohonen, RColorBrewer, data.table, sentimentr, lattice, glue, parallel, foreach, doParallel, moments)

setwd(here())

## MANUAL SELECTION OF OVERREPRESENTATION VALUE
  # before processing the data, choose how many rows to remove
  dat0 <- read.csv("Condition1_processed.csv") # Example filename for visualization
  table_act <- table(dat0$act)
  barplot(table_act, las = 2)
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
    
    # Determine counts of each 'act' and identify over-represented behaviors
    act_counts <- data %>% 
      group_by(act) %>%
      tally() %>%
      mutate(over_threshold = ifelse(n > threshold, threshold, n)) # Use the min of n and threshold
    
    # For over-represented behaviors, sample the desired threshold number of rows
    oversampled_data <- data %>% 
      inner_join(filter(act_counts, n > threshold), by = "act") %>%
      group_by(act) %>%
      sample_n(size = first(over_threshold), replace = FALSE) # Use the calculated threshold
    
    # For other behaviors, take all rows
    undersampled_data <- data %>% 
      anti_join(filter(act_counts, n > threshold), by = "act")
    
    # Combine and return
    return(bind_rows(oversampled_data, undersampled_data))
  }
  
# Formatting the data
trSamp2 <- function(x) { 
    d <- x[,5:47] ## INPUT ## Match these to the actual columns
    act <- as.factor(x$act) # Corresponding activities
    out <- list(measurements = as.matrix(d), act = act)
    return(out)
  }
  
# process the data
split_condition <- function(condition_name, filename, condition, threshold) {
  
  dat <- read.csv(filename)
  
  # Balance the data
  dat <- downsample_data(dat, threshold)
  
  # ensure the directories exist
  ensure_dir_exists(file.path(condition_name, "Random"))
  ensure_dir_exists(file.path(condition_name, "Chronological"))
  
  # Version One: Random 70:30 split
  ind <- dat %>% group_by(dat$act) %>% sample_frac(.7)
  trDat<-trSamp2(ind)
  tstind<-subset(dat, !(dat$X %in% ind$X))
  tstDat<-trSamp2(tstind)
  
  # save the random training and testing data
  save(trDat, file = file.path(condition_name, "Random", "TrDat.rda"))
  save(tstDat, file = file.path(condition_name, "Random", "TstDat.rda"))
  
  # Version Two: Chronological 70:30 split for each individual
    # Extract individual_ID from the file column
    dat$individual_ID <- as.numeric(sub(".*_(\\d+).*", "\\1", dat$file))
    
    # Split data by individual_ID
    split_data <- dat %>%
      group_by(individual_ID) %>%
      arrange(individual_ID) %>%
      do({
        .data <- .
        split_index <- floor(0.7 * nrow(.data))
        list(
          train_data = .data[1:split_index, ],
          test_data = .data[(split_index + 1):nrow(.data), ]
        )
      })
  
    # Extract training and testing data
    trDat <- bind_rows(split_data$train_data)
    tstDat <- bind_rows(split_data$test_data)
    
    # Apply trSamp2 to the resultant datasets
    trDat <- trSamp2(trDat)
    tstDat <- trSamp2(tstDat)
  
  # save the chronological training and testing data
  save(trDat, file = file.path(condition_name, "Chronological", "TrDat.rda"))
  save(tstDat, file = file.path(condition_name, "Chronological", "TstDat.rda"))
}

#### INPUT conditions of splitting ####
# Process both conditions
condition_name <- list("Condition2", "Condition1") # backwards order so the first ones go faster
conditions <- list("Condition1" = "Overlap", "Condition2" = "Nonoverlap")
threshold <- list("Condition1" = 20000, "Condition2" = 400)
for (condition in condition_name) {
  split_condition(condition_name, filename, conditions[[condition_name]], threshold[[condition_name]])
}
