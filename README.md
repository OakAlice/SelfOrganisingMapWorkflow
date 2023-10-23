# DataLeakage
Testing the effects of data leakage on model overfitting and accuracy using the kohonen Self-Organising Map. 

I have noticed that there is not a standardised approach to data pre-processing in accelerometry behaviour recognition machine learning, and I want to find out what impact the different preprocessing steps have on the overall model performance. In these scripts I am testing the interaction between the window overlap and the method of splitting the training and testing data. My hypothesis was that the use of overlapping windows and random splitting would result in data leakage, causing those models to have an inflated accuracy 'outperforming' non-dataleakage models - for example, models that used chronological splits (i.e., first 70% from each individual tested on last 30% from each individual) and non-overlapping window models. I used the SOM because I am familiar with it from prior research.

Data from [Vehkoaja et al., 2020](https://www.sciencedirect.com/science/article/pii/S2352340922000348). SOM code adapted from the github of [Gaschk et al., 2023](https://github.com/cclemente/Animal_accelerometry/tree/main).

##Test conditions
Overlapping windows, random split
Overlapping windows, chronological split
Nonoverlapping windows, random split
Nonoverlapping windows, chronological split

##Workflow
1. ChunkAndProcess.R -> Convert the original DogMoveData.csv into chunks, and produce features with or without window overlap
2. CreateTrainingTestingData.R -> Divide into training and testing data via random or non-random split, save the .rda output into respecive directories
3. CreateSOM.R -> Workflow for training a self-organising map on each of the training and testing pairs produced above.
4. VisualisingResults.R -> Code for producing plots and comparisons between the conditions