# DataLeakage
Testing the effects of data leakage on model overfitting and accuracy using the kohonen Self-Organising Map. 

I've observed that there isn't a consistent method for data pre-processing in machine learning related to accelerometry behavior recognition. My goal is to understand the influence of various preprocessing techniques on overall model performance. This script is designed to load in a labelled animal movement dataset and then run experiements with different hyperparameters. I used the Self-Organizing Map (SOM) because I am familiar with it from previous research. Expansion of this project will include multiple models.

Example data used was dog movement data from [Vehkoaja et al., 2020](https://www.sciencedirect.com/science/article/pii/S2352340922000348). SOM code adapted from the github of [Christofer Clemente](https://github.com/cclemente/Animal_accelerometry/tree/main).

## Workflow
Download the scripts to the local computer, download the data to the same folder.
- VariableEntry.R -> Choose the variables to be run through the experiments.
- ExecuteScript.R -> Master Script that pulls functions from each subsequent file, integrates, and executes.
- GeneralFunctions.R -> Functions that get used in multiple scripts or are just random.
- FeatureProcessing.R -> Function page to produce features with or without window overlap.
- CreateTrainingTestingData.R -> Divide into training and testing data.
- TrialSOMShapes.R -> Trialing different SOM shapes in order to find the optimal grid size for use in the next step.
- CreateSOM.R -> Workflow for training a self-organising map of the size produced above.
