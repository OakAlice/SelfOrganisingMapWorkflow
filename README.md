# Workflow for Self-Organising Maps

I don't recommend using this version - I've made a lot of changes since this was made. Consider this a relic of my thinking at the beginning of my PhD.

---

This is an updated version of Chris' original script (found here: [Christofer Clemente](https://github.com/cclemente/Animal_accelerometry/tree/main)). Unfortunately, that script had a few errors, which have been corrected here. Changes are the correct implementation of the kohonen package, ensured independence training and testing data, as well as the inclusion of a validation set, upon which to tune hyperparameters (NOTE: final correction still in progress). 

Example data used was dog movement data from [Vehkoaja et al., 2020](https://www.sciencedirect.com/science/article/pii/S2352340922000348)

## Variables
This script can be used to produce a SOM from any raw accelerometry data. You can adjust the degrees of freedom (i.e., axes), sampling rate, window length, window overlap, calculated features, selected behaviours, validation split, and number of data presentation epochs, from a single script. I will expand this list as I build in flexinility for more parameters.

## Workflow
Download the scripts to the local computer, download the data to the same folder.

- [VariableEntry.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/VariableEntry.R) -> Choose the variables to be run through the experiments.
- [ExecuteScript.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/ExecuteScript.R) -> Master Script that pulls functions from each subsequent file, integrates, and executes.\
- [ReformattingData.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/ReformattingData.R) -> Workflow to format the csv into an appropriate format.
- [GeneralFunctions.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/GeneralFunctions.R) -> Functions that get used in multiple scripts or are just random.
- [FeatureProcessing.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/FeatureProcessing.R) -> Function page to produce features with or without window overlap.
- [CreateTrainingTestingData.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/CreateTrainingTestingData.R) -> Divide into training and testing data. Note that you will have to manually experiment with the threshold value and set it approporiately. //NOTE: Still have to include validation set
- [TrialSOMShapes.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/TrialSOMShapes.R) -> Trialing different SOM shapes in order to find the optimal grid size for use in the next step.
- [CreateSOM.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/CreateSOM.R) -> Workflow for training a self-organising map of the size produced above.
- [SOMResults.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/SOMResults.R) -> Combine the results of each test and combine into a central csv for ease of comparison. Possibly broken?
- [Decoding.R](https://github.com/OakAlice/SelfOrganisingMapWorkflow/blob/main/Decoding.R) -> Applying the SOM to new labelled data  to determine an objective accuracy score. This script isn't generalisable yet.
  
