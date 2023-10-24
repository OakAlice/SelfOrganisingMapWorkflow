# DataLeakage
Testing the effects of data leakage on model overfitting and accuracy using the kohonen Self-Organising Map. 

I've observed that there isn't a consistent method for data pre-processing in machine learning related to accelerometry behavior recognition. My goal is to understand the influence of various preprocessing techniques on overall model performance. Specifically, I wanted to look at the interaction of window overlap, and the strategy for splitting training and testing data. I hypothesized that combining overlapping windows with random data splits might introduce data leakage which could artificially inflate accuracy, making such models appear to "outperform" models without that leakage. For non-leakage models, I used chronological splits (using the first 70% of data from each individual for training and the last 30% for testing) as well as non-overlapping windows.

I used the Self-Organizing Map (SOM) becauseI am familiar with it from previous research. Expansion of this project would trial multiple different models.

Data from [Vehkoaja et al., 2020](https://www.sciencedirect.com/science/article/pii/S2352340922000348). SOM code adapted from the github of [Gaschk et al., 2023](https://github.com/cclemente/Animal_accelerometry/tree/main).

## Test conditions
- Overlapping windows, random split
- Overlapping windows, chronological split
- Nonoverlapping windows, random split
- Nonoverlapping windows, chronological split

## Workflow
1. ChunkAndProcess.R -> Convert the original DogMoveData.csv into chunks, and produce features with or without window overlap
2. CreateTrainingTestingData.R -> Divide into training and testing data via random or non-random split, save the .rda output into respecive directories
3. CreateSOM.R -> Workflow for training a self-organising map on each of the training and testing pairs produced above.
4. VisualisingResults.R -> Code for producing plots and comparisons between the conditions