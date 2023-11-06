# Workflow for Self-Organising Maps

I've observed that there isn't a consistent method for data pre-processing in machine learning related to accelerometry behavior recognition. My goal is to understand the influence of various preprocessing techniques on overall model performance. This script is designed to load in a labelled animal movement dataset and then run experiments with different model parameters. I used the Self-Organizing Map (SOM) because I am familiar with it from previous research. Expansion of this project will include multiple models.

Example data used was dog movement data from [Vehkoaja et al., 2020](https://www.sciencedirect.com/science/article/pii/S2352340922000348) and possum movement data from Annett et al., 2023 (published link soon). SOM code adapted from the github of [Christofer Clemente](https://github.com/cclemente/Animal_accelerometry/tree/main).

## Variables
This script can be used to produce a SOM from any raw accelerometry data. You can adjust the degrees of freedom (i.e., axes), sampling rate, window length, window overlap, calculated features, selected behaviours, validation split, and number of data presentation epochs, from a single script. I will expand this list as I build in flexinility for more parameters. Future work will develop this into a user-friendly R-shiny.

## Workflow
Download the scripts to the local computer, download the data to the same folder.

- [VariableEntry.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/VariableEntry.R) -> Choose the variables to be run through the experiments.
- [ExecuteScript.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/ExecuteScript.R) -> Master Script that pulls functions from each subsequent file, integrates, and executes.\
- [ReformattingData.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/ReformattingData.R) -> Workflow to format the csv into an appropriate format.
- [GeneralFunctions.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/GeneralFunctions.R) -> Functions that get used in multiple scripts or are just random.
- [FeatureProcessing.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/FeatureProcessing.R) -> Function page to produce features with or without window overlap.
- [CreateTrainingTestingData.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/CreateTrainingTestingData.R) -> Divide into training and testing data. Note that you will have to manually experiment with the threshold value and set it approporiately.
- [TrialSOMShapes.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/TrialSOMShapes.R) -> Trialing different SOM shapes in order to find the optimal grid size for use in the next step.
- [CreateSOM.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/CreateSOM.R) -> Workflow for training a self-organising map of the size produced above.
- [SOMResults.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/SOMResults.R) -> Combine the results of each test and combine into a central csv for ease of comparison. Possibly broken?
- [Decoding.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/Decoding.R) -> Applying the SOM to new labelled data  to determine an objective accuracy score. This script isn't generalisable yet.
  
