## General Page for functions not specifically for any task

# Function to subset and rename columns to match the general format
subset_and_rename <- function(df, column_map) {
  # Check if all columns in the mapping exist in the dataframe
  if (all(names(column_map) %in% colnames(df))) {
    # Subset the dataframe
    df <- df[, names(column_map)]
    
    # Rename the columns
    colnames(df) <- column_map
    
    return(df)
  } else {
    stop("Some columns from the mapping are missing in the dataframe.")
  }
}

# Function for ensuring directory exists or creating it if not
ensure_dir <- function(dir_name) {
  if (!dir.exists(dir_name)) {
    dir.create(dir_name, showWarnings = FALSE)
  }
}

