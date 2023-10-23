# Now this will be the code that creates the SOM
# To test each of the conditions, rerun the code with different ##INPUT##
# Adapted from Gaschk et al., 2023

# install.packages("pacman")
library(pacman)
p_load(here, tidyverse, kohonen, RColorBrewer, data.table, sentimentr, lattice, glue, parallel, foreach, doParallel, moments)

# set the working directory to the project
setwd(here())

####INPUT: Choose training data ####
  ## Condition1_processed.csv <- Overlapping data
  ## Condition2_processed.csv <- Non-overlapping data

dat0 <- read.csv("Condition2_processed.csv")

dat <- dat0 # so I don't have to read it in again if I mess up

unique(dat$act) # check these
act <- as.factor(dat$act) # set as a factor

dat<-dat[which(dat$act!='<undefined>'),] # remove all unknown activities
dat <- dat[dat$act != '<undefined' & dat$act != 'Synchronization', ] # remove all of these too
dat <- na.omit(dat) # remove all the NAs

#### INPUT: Combine behaviours ####
# Only do this if you've already run the code
#ind<-which(dat$act=="Climb.1" | dat$act=="Climb.2" | dat$act=="Climb.3" | dat$act=="Climb.4" )
#levels(dat$act) <- c(levels(dat$act), "Climb")
#dat$act[ind]<-"Climb"

####INPUT: Balance the dataset ####
# visualise the current proportions
table_act <- table(dat$act)
barplot(table_act, las = 2)

# remove the overrepresented behaviours by reducing all behaviours to the same max number of rows
# Group the data by 'act' and count the number of rows in each group
act_counts <- dat %>%
  group_by(act) %>%
  summarise(count = n())

pp <- 400 # max number of rows, change this

# Filter 'act' values with more than X rows
over_acts <- act_counts %>%
  filter(count > pp) %>% ## CHANGE THIS NUMBER HERE
  pull(act)

# Initialize an empty dataframe to store the subsampled data
dat_subsampled <- data.frame()

# Loop through 'act' values with more than pp rows
for (act_value in over_acts) {
  # Filter rows for the current 'act' value and randomly sample pp rows
  sampled_data <- dat %>%
    filter(act == act_value) %>%
    sample_n(size = pp, replace = FALSE)
  
  # Combine the sampled data with the existing results
  dat_subsampled <- bind_rows(dat_subsampled, sampled_data)
}

# Include all rows for behaviors with less than or equal to pp rows
dat_subsampled <- bind_rows(dat_subsampled, dat %>%
                              filter(act %in% act_counts$act[act_counts$count <= pp]))

# Assign the result back to 'dat'
dat <- dat_subsampled

# Visually check the distribution of 'act'
table_act <- table(dat$act)
barplot(table_act, las = 2)

####INPUT: Choose training split ####

# Version One: Random 70:30 split
  # 70% training data
  ind <- dat %>% group_by(dat$act) %>% sample_frac(.7)
  trDat<-trSamp2(ind)
  
  #remaining 20%
  tstind<-subset(dat, !(dat$X %in% ind$X))
  tstDat<-trSamp2(tstind)

## Version Two: Chronological Split
  #### DO THIS

#### OPTOMISE SHAPE OF SOM ####
# Run experiments to test variations of SOM shapes

library(parallel)
numCores <- detectCores()
registerDoParallel(numCores)

gc()
# see which shape is the best (10 times)
for (bb in 1:10) {
  
  somsize<-rep(seq(4,9,1),6) #create some widths
  somsize2<-rep(4:9, times=1, each=6) # create some lengths
  somsize3<-cbind(somsize,somsize2) # these are the sizes
  
  acc3list <- matrix()
  # perform the SOM mapping a bunch of times
  system.time(
    acc3list<-foreach(i = 1:36, .packages=c('kohonen'), .combine=rbind) %dopar% {
      doSOMperf(trDat,tstDat, somsize3[i,1], somsize3[i,2])
    }
  )
  
  acc3<-acc3list
  
  # long_DF <- acc3 %>% gather(act, acc3, c(Climb.1, Climb.2, Climb.3, Climb.4, Tree.movement, Tree.still, Walking.1, Walking.2, Walking.3, Walking.4, Ground.movement, Ground.still))
  long_DF <- acc3 %>% gather(act, acc3, c("Lying chest", "Panting", "Playing", "Sitting", "Sniffing",       
                                          "Trotting", "Walking", "Shaking", "Eating",         
                                          "Pacing", "Standing", "Drinking", "Galloping", "Carrying object"))
  
  #boxplot(acc3~shape.somsize, subset(long_DF, test=='ACCU'))
  #boxplot(acc3~shape.somsize+test, long_DF)
  
  acc4<-subset(long_DF, test=='ACCU')
  
  #make heat map 
  df1<-data.frame(acc=acc4$acc3, width=acc4$width, height=acc4$height)
  
  #creates a matrix
  df2<-with(df1, tapply(acc, list(shape = width, height), FUN= mean, na.rm=TRUE))
  
  #CREATE OWN COLOURMAP
  colours_heat3 = c('#F4E119', '#F7C93B', '#C4BB5F', '#87BE76', '#59BD87', '#2CB6A0', '#00AAC1', '#1B8DCD', '#3D56A6', '#3A449C')
  
  #LATTICE PLOT
  levelplot(t(df2), cex.axis=1.0, cex.lab=1.0, col.regions=colorRampPalette(rev(colours_heat3)), 
            screen = list(z = -90, x = -60, y = 0),
            xlab=list(label='height', cex = 1.0),
            ylab=list(label='width', cex = 1.0),
            main=list(label= paste0('ACCU'), cex=1.0), 
            colorkey=list(labels=list(cex=1.0)),
            scales = list(cex=1.0),
            asp=1)
}


#### INPUT: CHOOSE THE BEST PERFORMING SOM AND DISPLAY ####
system.time(
  ssom <- supersom(trDat, grid = somgrid(5, 8, "hexagonal"))
)
system.time(
  ssom.pred <- predict(ssom, newdata = tstDat)
)

## Create Original Master SOM
acc5  <- doSOM(trDat,tstDat, 5, 8) ## outputs list(SOM, somperf)
# save the prediction outputs
pred_outputs <- acc5$somperf
#write.csv(ptab, "COnfusionMatrix_NonverlapRandom.csv", row.names = FALSE)

MSOM <- acc5$SOM

#make the plot
colsch <- colorRampPalette(c("#A6CEE3" ,"#1F78B4" ,"#B2DF8A" ,"#33A02C", "#FB9A99" ,"#E31A1C", "#FDBF6F" ,"#FF7F00" ,"#CAB2D6", "#6A3D9A", "#FFFF99"))
cols2<-brewer.pal(11, 'Set3')
plot(MSOM, heatkey = TRUE, palette.name=colsch, type = "codes", shape = "straight", ncolors = 11)


#outputting the data for later
save(MSOM, file="MSOM_NonoverlapRandom_5by8.1.rda")
#save(tstDat, file = "tstDat_OverlapRandom.rda")

ssom.pred <- predict(MSOM, newdata = tstDat)
ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)

#Save SOM figure
#SAVE grid figures
save(ptab, file =  "MSOM_5by8.1_Confusion_Matrix.rda")
write.csv(ptab, "MSOM_5by8.1_Confusion_Matrix.csv")



#### FUNCTIONS ####
#SOM function

z=9
zz=9

doSOMperf <- function(trDat, tstDat, z, zz) { 
  time_out<-system.time(
    ssom <- supersom(trDat, grid = somgrid(z, zz, "hexagonal"))
  )
  ssom.pred <- predict(ssom, newdata = tstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  true_positives  <- diag(ptab)
  false_positives <- rowSums(ptab) - true_positives
  false_negatives <- colSums(ptab) - true_positives
  true_negatives  <- sum(ptab) - true_positives - false_positives - false_negatives
  
  SENS<-c(true_positives/(true_positives+false_negatives), shape=z)
  ##Precision - how often a model is right when it predicts a behaviour
  PREC<-c(true_positives/(true_positives+false_positives), shape=z)
  #Specifitity -how often the absense of the behaviour is correctly identified by the model
  SPEC<-c(true_negatives/(true_negatives+false_positives), shape=z)
  #Accuracy - How often the model is right
  ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives), shape=z)
  
  dat_out<-as.data.frame(rbind(SENS,PREC,SPEC,ACCU))
  myDF <- cbind(test = rownames(dat_out), dat_out, time=time_out[3], width=z, height=zz)
  return(myDF)
}


doSOM <- function(trDat, tstDat, z, zz) { # Draw a sample of size 10k from all biboff, build a SOM, predict to rest data and compute overall accuracy
  time_out<-system.time(
    ssom <- supersom(trDat, grid = somgrid(z, zz, "hexagonal"))
  )
  ssom.pred <- predict(ssom, newdata = tstDat)
  ptab <- table(predictions = ssom.pred$predictions$act, act = tstDat$act)
  
  true_positives  <- diag(ptab)
  false_positives <- rowSums(ptab) - true_positives
  false_negatives <- colSums(ptab) - true_positives
  true_negatives  <- sum(ptab) - true_positives - false_positives - false_negatives
  
  SENS<-c(true_positives/(true_positives+false_negatives), shape=z)
  ##Precision - how often a model is right when it predicts a behaviour
  PREC<-c(true_positives/(true_positives+false_positives), shape=z)
  #Specifitity -how often the absense of the behaviour is correctly identified by the model
  SPEC<-c(true_negatives/(true_negatives+false_positives), shape=z)
  #Accuracy - How often the model is right
  ACCU<-c((true_positives+true_negatives)/(true_positives+true_negatives+false_positives+false_negatives), shape=z)
  
  dat_out<-as.data.frame(rbind(SENS,PREC,SPEC,ACCU))
  myDF <- cbind(test = rownames(dat_out), dat_out, time=time_out[3], width=z, height=zz)
  SOMout <- list(SOM = ssom, somperf = myDF)
  
  return(SOMout)
}

# A custom function to make life easier
trSamp2 <- function(x) { # Creates a training or test matrix, from data frame x, using a sample of size n (default is all rows)			
  d <- x[,5:47] ## INPUT ## Match these to the actual columns
  act <- as.factor(x$act) # Corresponding activities
  out <- list(measurements = as.matrix(d), act = act)
  return(out)
}

#####
