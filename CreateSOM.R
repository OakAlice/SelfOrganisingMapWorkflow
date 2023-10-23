# Now this will be the code that creates the SOM
# To test each of the conditions, rerun the code with different ##INPUT##
# Adapted from Gaschk et al., 2023

#### read in all the training and testing data from previous stage

TrainingData <- list("Condition1Random" = "Condition1/Random/TrDat.rda", "Condition2Random" = "Condition2/Random/TrDat.rda",
                      "Condition1Chron" = "Condition1/Chronological/TrDat.rda", "Condition2Chron" = "Condition2/Chronological/TrDat.rda")
TestingData <- list("Condition1Random" = "Condition1/Random/TstDat.rda", "Condition2Random" = "Condition2/Random/TstDat.rda",
                      "Condition1Chron" = "Condition1/Chronological/TstDat.rda", "Condition2Chron" = "Condition2/Chronological/TstDat.rda")
 

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


#####
