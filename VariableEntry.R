## Variable Entry
# The script where the variables are selected

#### ONE VARIABLE / SET PER RUN ####

# Experiment Number # so all the results go into differerent folders and dont write over
ExperimentNumber <- 1

# The data to analyse
MovementData <- "DogMoveData.csv"

# how many individuals to sample # mainly to spare my laptop processing power for mini-tests
test_individuals <- 4

# Desired sampling frequency, as Hz (potentially different from actual sampling frequency)
current_Hz <- 100
desired_Hz <- 20

# tell me what each of the columns are the ID, Time, X_accel, Y_accel, Z_accel, X_gyro, Y_gyro, Z_gyro, and activity
columnSubset <- c("DogID" = "ID", "t_sec" = "time", 
                  "ANeck_x" = "X_accel", "ANeck_y" = "Y_accel", "ANeck_z" = "Z_accel",
                  "GNeck_x" = "X_gyro", "GNeck_y" = "Y_gyro", "GNeck_z" = "Z_gyro",
                  "Behavior_1" = "activity")

# select the behaviours to include in the analysis
selectedBehaviours <- c("Drinking", "Eating", "Lying chest", "Panting", "Playing", 
                        "Sitting", "Sniffing", "Standing", "Trotting", "Walking")

# Features to be calculated on every axis, select from following list
# time included automatically
featuresList <- c("mean", "max", "min", "sd", "cor", "SMA", "minODBA", "maxODBA", "minVDBA", "maxVDBA")

# Proportion of training data, as a decimal percentage (for chronological and random only)
trainingPercentage <- 0.7

# Sampling Threshold, run the below code to visualise the dataset and choose a threshold
####### THIS IS GOING TO HAVE TO BE AUTO, OR A PROMPT SCREEN???
threshold <- 400

#### CAN TRIAL MULTIPLE PER RUN ####

# Window length, in seconds
window <- 1

# Window overlap, as a % # if <0, is overlapping
overlap <- c(0, 50)

# Training Testing split method (choose from: random, chronological, LOIO)
splitMethod <- c("random", "chronological", "LOIO")
