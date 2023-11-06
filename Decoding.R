## Running the SOM on labelled data 
# This script will simply use the created SOMs to decode data
# normally this process if for unlabelled data, but I'll use it on labelled so we can verify results

#### INCOMPLETE -> Currently only works for the specific dog data


# load in the SOM
conditions <- c("Original", "Improved")
som_path <- c("Original" = "Experiment_2/1_sec_window/0%_overlap/chronological/Final_SOM.rda") # ssom
Labelled_data <- "Experiment_6/1_sec_window/0%_overlap/Processed_Data.csv"


# formatting function
trSamp2 <- function(x) {   d <- x[,2:33] # select only the data rows
out <- list(measurements = as.matrix(d))
return(out)
}


# load in processed and labelled data
Labelled_feature_data <- read.csv(Labelled_data)

Data <- trSamp2(Labelled_feature_data)

results <- list()

#for (condition in conditions) {
  condition <- "Original"
  
  # Load the SOM model
  load(som_path[[condition]])
  # predict the answers
  ssom.pred <- predict(ssom, newdata = Data, whatmap = "measurements") # predict behaviors
  
  # make the actual and the predicted values side by side
  # behaviours
  Labelled_feature_data$true_behaviours <- Labelled_feature_data$activity
  Labelled_feature_data$predicted_behaviours <- ssom.pred$predictions$activity ##insert behaviors
  
  # remove irrelevant columns
  columns <- c("X", "ID", "true_behaviours", "predicted_behaviours")
  Labelled_feature_data <- subset(Labelled_feature_data, select = columns)
  
  # Compare predicted activities with actual activities
  comparison <- data.frame(actual = Labelled_feature_data$true_behaviours, predicted = Labelled_feature_data$predicted_behaviours)

  
#### STATISTICS ####
  # Calculate some accuracy metrics
  accuracy <- sum(comparison$actual == comparison$predicted) / nrow(comparison)
  confusion_matrix <- table(comparison$actual, comparison$predicted)
  
  # use table to make statistics for understanding the model performance
  true_positives  <- diag(confusion_matrix)
  false_positives <- rowSums(confusion_matrix) - true_positives
  false_negatives <- colSums(confusion_matrix) - true_positives
  true_negatives  <- rep(sum(confusion_matrix), length(true_positives)) - rowSums(confusion_matrix) - colSums(confusion_matrix) + true_positives
  
  SENS <- true_positives / (true_positives + false_negatives)
  PREC <- true_positives / (true_positives + false_positives)
  SPEC <- true_negatives / (true_negatives + false_positives)
  ACCU <- sum(true_positives) / sum(confusion_matrix)
  
  # Prepare the statistics data frame
  dat_out <- as.data.frame(rbind(SENS, PREC, SPEC, ACCU=ACCU))
  
  statistical_results <- cbind(test = rownames(dat_out), dat_out)
  write.csv(statistical_results, file.path("Experiment_6/Statistical_results.csv"))

#### GRAPHS ####
  
  ## Barplot
  # Create a long format data for actual and predicted
  # Melt the comparison dataframe into long format specifying the measured variables
  comparison_long <- melt(comparison, measure.vars = c("actual", "predicted"), variable.name = "Type", value.name = "Behaviour")
  
  # Check the head of the melted dataframe
  head(comparison_long)
  
  # count the differences
  counts <- comparison_long %>%
    group_by(Behaviour, Type) %>%
    summarize(Count = n())
  
  counts <- counts %>%
    group_by(Behaviour) %>%
    mutate(PercentDiff = (abs(diff(Count)) / sum(Count)) * 100) %>%
    mutate(PercentDiff = round(PercentDiff,1)) %>%
    ungroup()
  
  # Create the bar plot
  ggplot(counts, aes(x = Behaviour, y = Count, fill = Type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
    geom_text(
      aes(label = ifelse(Type == "actual", paste0(PercentDiff, "%"), ""), 
          y = -5), # Set a negative value to position below the bars
      position = position_dodge(width = 0.9),
      hjust = 0,
      vjust = 1  # You may need to adjust this
    ) +
    scale_fill_manual(values = c("grey", "lightblue")) +
    labs(x = 'Behaviour', y = 'Count') +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill=NA, linewidth=1.5),
      axis.ticks = element_blank()
    )
  
  
  ## Dotplot
  ggplot(Labelled_feature_data, aes(x = true_behaviours, y = predicted_behaviours)) +
    geom_jitter(width = 0.2, height = 0.2, size = 2, alpha = 0.6) +
    theme_minimal() +
    labs(x = 'Actual Behaviour', y = 'Predicted Behaviour') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
